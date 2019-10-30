
# Public structure of incidence rs_2014 class
#' @noRd
incidence_rs2014_public = list(
  weekly = NULL,
  intake = NULL,
  participant = NULL,
  params = NULL,
  syndroms = NULL,
  profiler = NULL,
  verbose = FALSE,
  design = NULL,
  output = NULL,

  # @param weekly data
  # @param intake intake data
  # @param syndroms syndroms column names in weekly
  # @param design design_incidence
  # @param output list of output types to compute
  initialize = function(weekly, intake, params, syndroms, design, output=c('inc','zlow','age')) {

    if(!requireNamespace("dplyr")) {
      stop("dplyr package needed")
    }

    if(!requireNamespace("epitools")) {
      stop("epitools package needed")
    }

    # params default values
    def = list(
      ignore.first.delay = NA, # Number of days form his first survey befotre to include a participant (not compatible with ignore.first)
      ignore.first = F,  # Remove all first surveys (regardless history, @deprecated)
      ignore.first.only.new = F, # Ignore first rule only for new participant (uses intake$first.season column @see load_results_for_incidence)
      exclude.same = F, # should exclude syndrom if same episode is on (partcipant still active)
      exclude.same.delay = 15, # max delay to exclude syndrom if same (not excluded if over this delay)
      active.max.freq = NA, # max delay between 2 surveys (in weeks)
      active.week.before = NA, # Number of week before each computed week to count active participants
      active.week.after = NA, # Number of week after each computed week to count active participants
      active.min.surveys = NA # min number of surveys for each participant (not active if less)
    )

    params = default.params(params, def)

    syndroms = as.vector(syndroms)
    names(syndroms) = NULL # Be sure no names is defined

    self$params = params
    self$syndroms = syndroms
    self$design = design
    self$output = output
    self$weekly = weekly
    self$intake = intake

    if( is.null(output) ) {
      output =  eval(formals()$output)
    }


  },

  # Prepare data for incidence computation with RS2014 method
  # compute first list of participants and filter with some parameters (with constant effects over time)
  prepare = function() {

    weekly = self$weekly
    syndroms = self$syndroms
    ignore.first.delay = self$params$ignore.first.delay
    ignore.first.only.new = self$params$ignore.first.only.new

    exclude.same  = self$params$exclude.same
    exlude.same.delay = self$params$exclude.same.delay
    active.max.freq = self$params$active.max.freq
    active.min.surveys = self$params$active.min.surveys
    onset.columns = self$params$onset.columns

    if(is.null(self$profiler)) {
      track <- function(...) {} # do nothing
    } else {
      track <- function(step) {
        self$profiler$track(step, "prepare")
      }
    }

    weekly$date = as.Date(trunc(weekly$timestamp, "day"))
    track("date")

    if(exclude.same) {
      weekly = weekly[ order(weekly$person_id, weekly$timestamp), ]
      weekly$delay.date = calc_weekly_delay(weekly, "date")
      # same.episode using recoded value (see recode_weekly())
      same = !is.na(weekly$same.episode) & weekly$same.episode == "Yes"  & (is.na(weekly$delay.date) | weekly$delay.date <= exlude.same.delay)
      for(ss in syndroms) {
        # Cancel a syndrom report if flagged as same episode as previous
        i = !is.na(weekly[, ss]) & weekly[, ss] > 0
        weekly[ same & i, ss ] = 0
      }
      track("exclude-same")
    }

    if( is.null(onset.columns) ) {
      # Default behaviour
      # onset = first available date from symptom start, fever.start and survey date
      onset.columns = c('fever.start', 'sympt.start', 'date')
    }

    weekly$onset = as.Date(apply(weekly[, onset.columns ], 1, coalesce))
    track("onset")

    # @TODO
    # Actual strategy will remove aberrant data (if fever.start or sympt.start are too far)
    # Depending on params the participant will be counted as active or not

    weekly$yw = iso_yearweek(weekly$onset)
    track("yw")

    if( !is.na(ignore.first.delay) ) {
      # We need to keep first date
      first.date = aggregate(list(date.first=weekly$onset), list(person_id=weekly$person_id), min)
      track("first.date")
    }

    # Now aggregate to the week and person
    weekly = aggregate(as.list(weekly[, syndroms, drop=FALSE]), list(person_id=weekly$person_id, yw=weekly$yw), sum, na.rm=T)
    track("syndrom-agg")

    weekly[, syndroms] = weekly[, syndroms] > 0 # Only one syndrom report by participant by week
    track("syndrom")

    weekly = weekly[ order(weekly$person_id, weekly$yw), ]
    weekly$delay.previous = calc_weekly_delay(weekly, time.col='yw')
    track("delay.previous")

    participant = aggregate(list(yw.first=weekly$yw), list(person_id=weekly$person_id), min)
    track("participant")

    if( !is.na(ignore.first.delay) ) {
      # Need date of first "survey" (actually, onset) for this parameter
      participant = merge(participant, first.date , by='person_id', all.x=T)

      if( isTRUE(ignore.first.only.new) ) {
        if( is.null(self$intake$first.season) ) {
          stop("ignore.first.only.new option requires intake to have a 'first.season' column indicating if participant is new for the season")
        }
        participant = merge(participant, self$intake[, c('person_id','first.season')], by='person_id', all.x=T)
      }
      track("ignore.first")
    }

    participant = merge(participant, aggregate(list(yw.last=weekly$yw), list(person_id=weekly$person_id), max), by='person_id')
    track("yw.last")

    if( !is.na(active.min.surveys) ) {
      weekly$nb = 1
      participant = merge(participant, aggregate(nb ~ person_id, data=weekly, sum), by='person_id')
      track("min.suveys")
    }

    if(!is.na(active.max.freq)) {
      participant = merge(participant, aggregate(delay.previous ~ person_id, data=weekly, max, na.rm=T), by='person_id')
      track("min.suveys")
    }

    self$weekly = weekly
    self$participant = participant
  },

  #' Internal function
  #' Estimate incidence with rs2014 method for a given week
  #' @param yw yearweek number to compute
  compute_week = function(yw)
  {
    weekly = self$weekly
    intake = self$intake
    participant = self$participant
    syndroms = self$syndroms

    active.week.before = self$params$active.week.before
    active.week.after = self$params$active.week.after
    active.max.freq = self$params$active.max.freq # max delay between 2 surveys (in weeks)
    active.min.surveys = self$params$active.min.surveys
    ignore.first.delay = self$params$ignore.first.delay
    ignore.first.only.new = self$params$ignore.first.only.new # only ignore first survey for new participant for this season

    select.count = list()

    # Time profiler (for dev)
    if(is.null(self$profiler)) {
      track_time = function(...) {}
    } else {
      track_time = function(name) {
        self$profiler$track(name, yw)
      }
    }

    # Track count of participants (for introspection)
    track <- function(name, n=NULL) {
      if(!is.null(self$profiler)) {
        self$profiler$track(name, yw)
      }
      if( is.null(n) ) {
        n = nrow(participant)
      }
      if(self$verbose) {
        cat(" * ", name,"=", n, "\n")
      }
      select.count[[name]] <<- n
    }

    ###
    # Active participants
    ####
    track("base")

    # Should have first survey before considered week
    participant = participant[ participant$yw.first <= yw, ]

    track("first_after_week")

    # Keep participant when first survey is more than $ignore.first.delay days, remove otherwise
    if( !is.na(ignore.first.delay) ) {
      d = floor(monday_of_week(yw) - participant$date.first) >= ignore.first.delay
      if(ignore.first.only.new) {
        # Take into account of the first.season, only participant with first.season=T can be removed
        # if first.season=NA, first.season is censored (cannot be determined), keep the participant regardless delay
        d = ifelse(!is.na(participant$first.season) & participant$first.season, d, T) # always keep if not first season for a participant
      }
      participant = participant[ d, ]
    }
    track("ignore_first_delay")

    # Should have at least one survey in a time window defined by active.week.after & active.week.before
    if( !is.na(active.week.after) ) {
      active.last.week = iso_yearweek(monday_of_week(yw) + active.week.after * 7)
    } else {
      active.last.week = max(weekly$yw)
    }
    if( !is.na(active.week.before) ) {
      active.first.week = iso_yearweek(monday_of_week(yw) - active.week.before * 7)
    } else {
      active.first.week = min(weekly$yw)
    }

    a = unique(weekly$person_id[ weekly$yw >= active.first.week & weekly$yw <= active.last.week ])

    participant = participant[ participant$person_id %in% a, ]
    rm(a)

    track("in_active_window")

    if( !is.na(active.max.freq) ) {
      # Keep only participants with a minimal frequency of surveys
      participant = participant[ participant$delay.previous <= active.max.freq, ]
      track("max_freq")
    }

    if( !is.na(active.min.surveys) ) {
      # Keep only participants with a minimal frequency of surveys
      participant = participant[ participant$nb >= active.min.surveys, ]
      track("min.surveys")
    }

    track("final")

    if(nrow(participant) == 0) {
      r = list()
      attr(r, "select.count") <- select.count
      return(r)
    }

    strata = self$design$strata

    if( is.null(strata) ) {
      # Use dummy strata column
      # It more costly than avoid aggregation but this garantees that the same procedure is used to compute regardless design
      intake$dummy = 1
      strata = 'dummy'
    }

    participant$active = 1

    # active participant = having at least one survey on 3 weeks
    participant = merge(participant, intake[, c('person_id', strata)], by='person_id', all.x=T) # get the geo code
    track_time("merge_intake")

    # cat("Strata: ", paste(strata,collapse=','))

    active.week = aggregate(list(active=participant$active), participant[, strata, drop=FALSE], sum)
    track_time("active.week")

    # now calculate number of participant by health status for the current week

    # now we only take account of the week of interest
    weekly = weekly[ weekly$yw == yw, ] # only keep our week of interest
    weekly = weekly[ weekly$person_id %in% participant$person_id, ] # only keep weekly from active participant

    track("weekly_for_part", nrow(weekly))

    if(nrow(weekly) == 0) {
      r = list()
      attr(r, "select.count") = select.count
      return(r)
    }

    # Count number of syndroms by user and by week
    count = aggregate(as.list(weekly[, syndroms, drop=FALSE]), list(person_id=weekly$person_id), sum)
    track_time("count")

    count = merge(count, intake[, c('person_id', strata)], by='person_id', all.x=T) # get the geo code for each user
    track_time("count.intake")

    # make syndroms exclusive in a week ?
    # nop now

    count[, syndroms] = as.integer(count[, syndroms] > 0) # syndroms counted only once for each user

    # aggregate by strata
    count.week = aggregate(count[, syndroms, drop=FALSE], count[ , strata, drop=FALSE], sum)
    track_time("count.week")

    # merge with active
    count.week = merge(active.week, count.week, by=strata, all.x=T)
    track_time("merge.count.week")

    if( !(length(self$output) == 1 && is.na(self$output) )) {
      # Compute incidence from count data
      r = calc_adjusted_incidence(count.week, design=self$design, syndroms=syndroms, output=self$output)
      track_time("calc.incidence")

    } else {
      r = list(count = count.week)
    }

    attr(r, "select.count") <- select.count

    r
  },
  # Compute incidence
  compute = function(weeks=NULL, verbose=T, progress=F, verticalize=FALSE) {
    "Compute incidence"

    # Prepare data for all weeks (common part)
    if(verbose) {
      str(self$params)
      cat("Preparing data...")
    } else {
      if(progress) {
        cat('~')
      }
    }

    self$prepare()

    if(verbose) {
      cat("done.\n")
    }

    inc = NULL
    inc.zlow = NULL
    inc.age = NULL
    count = NULL
    selection = NULL

    if( is.null(weeks) ) {
      weeks = unique(self$weekly$yw)
    }

    for(w in weeks) {

      if(verbose) {
        cat("Week",w,"\n")
      } else {
        if(progress) {
          cat(".")
        }
      }

      r = self$compute_week(yw=w)

      if( !is.null(r$inc) && nrow(r$inc) > 0 ) {
        ii = r$inc
        ii$yw = w
        inc = dplyr::bind_rows(inc, ii)
      }

      if( !is.null(r$zlow) && nrow(r$zlow)  > 0) {
        ii = r$zlow
        ii$yw = w
        inc.zlow = dplyr::bind_rows(inc.zlow, ii)
      }

      if( !is.null(r$age) && nrow(r$age)  > 0) {
        ii = r$age
        ii$yw = w
        inc.age = dplyr::bind_rows(inc.age, ii)
      }

      if( !is.null(r$count) && nrow(r$count)  > 0) {
        ii = r$count
        ii$yw = w
        count = dplyr::bind_rows(count, ii)
      }

      if("selection" %in% self$output) {
        ii = attr(r, "select.count")
        ii = data.frame(step=names(ii), count=unlist(ii))
        ii$yw = w
        selection = dplyr::bind_rows(selection, ii)
      }
    }

    if(progress) {
      cat("\n")
    }

    if(verticalize) {

      syndroms = self$syndroms
      if(!is.null(inc)) {
        inc = verticalize_incidence(inc, ids="yw", syndroms = syndroms )
      }

      if( !is.null(inc.age) ) {
        inc.age = verticalize_incidence(inc.age, ids=c("yw",'age.cat'), syndroms = syndroms )
      }

      if( !is.null(inc.zlow) ) {
        inc.zlow = verticalize_incidence(inc.zlow, ids=c("yw", self$design$geo_column), syndroms=syndroms)
      }

    }

    structure(
      list(
        inc=inc,
        inc.zlow=inc.zlow,
        inc.age=inc.age,
        count=count,
        selection=selection
      ),
      syndroms = self$syndroms,
      design=self$design,
      params=self$params,
      method="rs2014"
    )
  } # compute

) # Public


#' Compute Incidence using rs2014 method
#' Active participants are selected using a set of rules
#'
#' @docType class
#'
#' @field weekly weekly data
#' @field intake intake data
#' @field participants data.frame() with all available participants and commputed criterias used during computation
#' @field params parameters for computation
#' @field syndroms character vector of column names containing syndroms classification for each weekly
#' @field profiler profiler
#' @field design design stratification from \code{\link{design_incidence}}
#' @field output vector of character (see Details outupus)
#'
#' @details Parameters:
#' \describe{
#'  \item{ignore.first.delay}{Number of days form his first survey befotre to include a participant (not compatible with ignore.first)}
#'  \item{ignore.first.only.new}{Ignore first rule only for new participant (uses intake$first.season column @see load_results_for_incidence)}
#'  \item{exclude.same}{should exclude syndrom if same episode is on (partcipant still active)}
#'  \item{exclude.same.delay}{max delay to exclude syndrom if same (not excluded if over this delay)}
#'  \item{active.max.freq}{max delay between 2 surveys (in weeks)}
#'  \item{active.week.before}{Number of week before each computed week to count active participants}
#'  \item{active.week.after}{Number of week after each computed week to count active participants}
#'  \item{active.min.surveys}{min number of surveys for each participant (not active if less)}
#' }
#'
#' @details Input:
#' Input data are expected to have some columns & some fixes. They are loaded and prepared using \code{\link{load_results_for_incidence}}
#'
#' @details Output:
#' Incidence estimator can output several kind of datasets
#' \describe{
#'  \item{inc}{inc=incidence at (national level)}
#'  \item{zlow}{"lower geographic level (z)", if estimator is stratified by geographic level}
#'  \item{age}{"age" age-specific incidence, age categories should be in "age.cat" column in intake data}
#'  \item{count}{raw count of syndroms used for each syndrom by week, before incidence is computed}
#' }
#'
#' Apply the full algorithm for rs2014 incidence computation
#'
#' @export
IncidenceRS2014 = R6Class("IncidenceRS2014", public = incidence_rs2014_public)

