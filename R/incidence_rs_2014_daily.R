
# Public structure of incidence rs_2014 class
#' @noRd
incidence_rs2014_daily_public = list(
    weekly = NULL,
    intake = NULL,
    participant = NULL,
    params = NULL,
    syndromes = NULL,
    profiler = NULL,
    verbose = FALSE,
    design = NULL,
    output = NULL,

    initialize = function(weekly, intake, syndromes, params, design, output=c('inc')) {

      requireNamespace("dplyr", quietly = FALSE)
      requireNamespace("epitools", quietly = FALSE)

      if( is.null(output) ) {
        output =  eval(formals()$output)
      }

      # params default values
      def = list(
        ignore.first.delay = NA, # Number of days form his first survey befotre to include a participant (not compatible with ignore.first)
        ignore.first.only.new = F, # Ignore first rule only for new participant (uses intake$first.season column @see load_results_for_incidence)
        exclude.same = F, # should exclude syndrom if same episode is on (partcipant still active)
        exclude.same.delay=15, # max delay to exclude syndrom if same (not excluded if over this delay)
        active.max.freq=NA, # max delay between 2 surveys (in weeks)
        active.day.before=NA, # Number of week before each computed week to count active participants
        active.day.after=NA, # Number of week after each computed week to count active participants
        active.min.surveys=NA, # min number of surveys for each participant (not active if less)
        active.use.mean=FALSE
      )

      params = default.params(params, def)

      self$params = params
      self$output = output
      self$weekly = weekly
      self$intake = intake
      self$design = design
      self$syndromes = syndromes
    },


    prepare = function() {

      weekly = self$weekly
      syndromes = self$syndromes

      ignore.first.delay = self$params$ignore.first.delay
      ignore.first.only.new = self$params$ignore.first.only.new

      exclude.same  = self$params$exclude.same
      exlude.same.delay = self$params$exclude.same.delay
      active.max.freq = self$params$active.max.freq
      active.min.surveys = self$params$active.min.surveys

      weekly$date = as.Date(trunc(weekly$timestamp, "day"))

      if(exclude.same) {
        weekly = weekly[ order(weekly$person_id, weekly$timestamp), ]
        weekly$delay.date = calc_weekly_delay(weekly, "date")
        same = !is.na(weekly$same.episode) & weekly$same.episode == YES  & (is.na(weekly$delay.date) | weekly$delay.date <= exlude.same.delay) # recoded value !
        for(ss in syndromes) {
          # Cancel a syndrom report if flagged as same episode as previous
          i = !is.na(weekly[, ss]) & weekly[, ss] > 0
          weekly[ same & i, ss ] = 0
        }
      }

      if(!hasName(weekly, "onset")) {
        stop("Weekly data should have onset column defined")
      }

      # @TODO
      # Actual strategy will remove aberrant data (if fever.start or sympt.start are too far)
      # Depending on params the participant will be counted as active or not

      # Now aggregate to the week and person
      weekly = aggregate(as.list(weekly[, syndromes]), list(person_id=weekly$person_id, onset=weekly$onset), sum, na.rm=T)
      weekly[, syndromes] = weekly[, syndromes] > 0 # Only one syndrom report by participant by day

      weekly = weekly[ order(weekly$person_id, weekly$onset), ]

      weekly = weekly %>% dplyr::group_by(person_id) %>% dplyr::mutate(delay.previous=c(NA, diff(onset)))

      #weekly$delay.previous = calc_weekly_delay(weekly, time.col='onset')

      participant = weekly %>% dplyr::group_by(person_id) %>% dplyr::summarise(date.first=min(onset))

      if( !is.na(ignore.first.delay) ) {
        if( isTRUE(ignore.first.only.new) ) {
          if( is.null(self$intake$first.season) ) {
            stop("ignore.first.only.new option requires intake to have a 'first.season' column indicating if participant is new for the season")
          }
          participant = merge(participant, self$intake[, c('person_id', 'first.season')], by='person_id', all.x=T)
        }
      }

      date.last = weekly %>% dplyr::group_by(person_id) %>% dplyr::summarise(date.last=max(onset))

      participant = merge(participant, date.last, by='person_id')

      if( !is.na(active.min.surveys) ) {
        weekly$nb = 1
        nb.survey = weekly %>% dplyr::group_by(person_id) %>% dplyr::summarise(nb=sum(nb))
        participant = merge(participant, nb.survey, by='person_id')
      }

      if(!is.na(active.max.freq)) {
        dd = weekly %>% dplyr::group_by(person_id) %>% dplyr::summarise(delay.previous=max(delay.previous, na.rm=T))
        participant = merge(participant, dd, by='person_id')
      }

      self$weekly = weekly
      self$participant = participant
  },


  # Internal function
  # Estimate incidence with rs2014 method for a given date
  compute_date = function(date)
  {

    active.day.before = self$params$active.day.before
    active.day.after = self$params$active.day.after
    active.max.freq = self$params$active.max.freq # max delay between 2 surveys (in days)
    active.min.surveys = self$params$active.min.surveys
    active.use.mean = self$params$active.use.mean
    ignore.first.delay = self$params$ignore.first.delay
    ignore.first.only.new = self$params$ignore.first.only.new # only ignore first survey for new participant for this season

    # Track count of participants (for introspection)
    track = create_step_tracker()

    participant = self$participant
    weekly = self$weekly
    syndromes = self$syndromes

    ###
    # Active participants
    ####
    track$add("base", nrow(participant))

    # Should have first survey before considered date
    participant = participant[ participant$date.first <= date, ]

    track$add("first_after_week", nrow(participant))

    # Keep participant when first survey is more than $ignore.first.delay days, remove otherwise
    if( !is.na(ignore.first.delay) ) {
      d = floor(date - participant$date.first) >= ignore.first.delay
      if(ignore.first.only.new) {
        # Take into account of the first.season, only participant with first.season=T can be removed
        # if first.season=NA, first.season is censored (cannot be determined), keep the participant regardless delay
        d = ifelse(!is.na(participant$first.season) & participant$first.season, d, T) # always keep if not first season for a participant
      }
      participant = participant[ d, ]
    }

    track$add("ignore_first_delay", nrow(participant))

    if( !is.na(active.max.freq) ) {
      # Keep only participants with a minimal frequency of surveys
      participant = participant[ participant$delay.previous <= active.max.freq, ]
      track$add("max_freq", nrow(participant))
    }

    if( !is.na(active.min.surveys) ) {
      # Keep only participants with a minimal frequency of surveys
      participant = participant[ participant$nb >= active.min.surveys, ]
      track$add("min.surveys", nrow(participant))
    }

    # Should have at least one survey in a time window defined by active.week.after & active.week.before
    if( !is.na(active.day.after) ) {
      active.last.day = date + active.day.after
    } else {
      active.last.day = max(weekly$onset)
    }

    if( !is.na(active.day.before) ) {
      active.first.day = date - active.day.before
    } else {
      active.first.day = min(weekly$onset)
    }

    a = weekly %>% dplyr::filter(dplyr::between(onset, active.first.day, active.last.day)) %>% dplyr::select(person_id)
    a = unique(a$person_id)

    participant = participant[ participant$person_id %in% a, ]

    track$add("in_active_window", nrow(participant))

    track$add("final", nrow(participant))

    if(nrow(participant) == 0) {
      r = list()
      attr(r, "select.count") <- track$get_steps()
      return(r)
    }

    strata = self$design$strata

    if( is.null(strata) ) {
      # Use dummy strata column
      # It more costly than avoid aggregation but this garantees that the same procedure is used to compute regardless design
      self$intake$dummy = 1
      strata = 'dummy'
    }

    participant$active = 1

    # active participant = having at least one survey on 3 weeks
    participant = merge(participant, self$intake[, c('person_id', strata)], by='person_id', all.x=T) # get the geo code

    # cat("Strata: ", paste(strata,collapse=','))

    active = aggregate(list(active=participant$active), participant[, strata, drop=FALSE], sum)

    if(nrow(active) == 0) {
      return(structure(list(), "select.count"=track$get_steps()))
    }

    if(active.use.mean) {
      w = 1 + sum(active.day.after, active.day.before, na.rm=T)
      active$active = active$active / w # Use mean of participants seen on window size
    }

    # now calculate number of participant by health status for the current week

    # now we only take account of the week of interest
    weekly = weekly[ weekly$onset == date, ] # only keep our week of interest
    weekly = weekly[ weekly$person_id %in% participant$person_id, ] # only keep weekly from active participant

    track$add("weekly_for_part", nrow(weekly))

    if(nrow(weekly) == 0) {
      r = list()
      attr(r, "select.count") = track$get_steps()
      return(r)
    }

    # Count number of syndrom by user and by week
    count = aggregate(as.list(weekly[, syndromes]), list(person_id=weekly$person_id), sum)

    count = merge(count, self$intake[ , c('person_id', strata)], by='person_id', all.x=T) # get the geo code for each user

    # make syndrom exclusive in a date ?
    # nop now

    count[, syndromes] = as.integer(count[, syndromes] > 0) # syndrom counted only once for each user

    # aggregate by strata
    count.strata = aggregate( count[, syndromes, drop=F], count[, strata, drop=F], sum, na.rm=T)

    if( nrow(count.strata) == 0 ) {
      count.strata = active
      count.strata[, syndromes] = as.numeric(NA)
    } else {
      # merge with active
      count.strata = merge(active, count.strata, by=strata, all.x=T)
    }

    if( !is.na(self$output) ) {
      # Compute incidence from count data
      r = calc_adjusted_incidence(count.strata, design=self$design, syndromes=self$syndromes, output=self$output)
    } else {
      r = list(count = count.strata)
    }

    attr(r, "select.count") <- track$get_steps()

    r
  },

  # Compute weekly incidence for all requested weeks
  # Apply the full algorithm for rs2014 incidence computation
  # prepare data and then compute for each week by calling estimate_incidence_rs2014_week
  compute = function(dates=NULL, verbose=T, progress=T) {

    # Prepare data for all weeks (common part)
    if(verbose) {
      str(self$params)
      cat("Preparing data...")
    } else {
      if( !is.null(progress) ) {
        cat('~')
      }
    }

    self$verbose = verbose

    self$prepare()

    if(verbose) {
      cat("done.\n")
    }


    inc = NULL
    inc.zlow = NULL
    inc.age = NULL
    count = NULL
    if( is.null(dates) ) {
      dates = unique(self$weekly$onset)
    }

    use.progress = FALSE
    if( isTRUE(progress) ) {
      progress = dplyr::progress_estimated(length(dates), min_time = 5)
      use.progress = TRUE
    }

    for(i in seq_along(dates)) {
      date = dates[i]
      if(verbose) {
        cat("Week", date,"\n")
      } else {
        if( use.progress ) {
          progress$tick()$print()
        }
      }

      r = self$compute_date(date=date)

      if( !is.null(r$inc) && nrow(r$inc) > 0 ) {
        ii = r$inc
        ii$date = date
        inc = dplyr::bind_rows(inc, ii)
      }

      if( !is.null(r$zlow) && nrow(r$zlow)  > 0) {
        ii = r$zlow
        ii$date = date
        inc.zlow = dplyr::bind_rows(inc.zlow, ii)
      }

      if( !is.null(r$age) && nrow(r$age)  > 0) {
        ii = r$age
        ii$date = date
        inc.age = dplyr::bind_rows(inc.age, ii)
      }

      if( !is.null(r$count) && nrow(r$count)  > 0) {
        ii = r$count
        ii$date = date
        count = dplyr::bind_rows(count, ii)
      }

    }
    if(use.progress) {
      cat("\n")
    }
    structure(
      list(
        inc=inc,
        inc.zlow=inc.zlow,
        inc.age=inc.age,
        count=count
      ),
      syndromes = self$syndromes,
      design=self$design,
      params=self$params,
      method="rs2014"
    )
  }
)

#' Incidence estimator using rs2014 method implemented using daily agregatation
#'
#' @field weekly weekly data (loaded using \code{\link{load_results_for_incidence}})
#' @field intake intake data (loaded using \code{\link{load_results_for_incidence}})
#' @field participants data.frame() with all available participants and commputed criterias used during computation
#' @field params parameters for computation
#' @field syndromes character vector of column names containing syndromes classification for each weekly
#' @field verbose logical show verbose message
#' @field design design stratification from \code{\link{design_incidence}}
#' @field output vector of character, see \code{\link{IncidenceRS2014}}
#'
#' @details Input:
#' Input data are expected to have some columns & some fixes. They are loaded and prepared using \code{\link{load_results_for_incidence}}
#'
#' @details Parameters:
#'
#' \describe{
#'  \item{ignore.first.delay}{Number of days form his first survey befotre to include a participant (not compatible with ignore.first)}
#'  \item{ignore.first.only.new}{Ignore first rule only for new participant (uses intake$first.season column}
#'  \item{exclude.same}{should exclude syndrom if same episode is on (partcipant still active)}
#'  \item{exclude.same.delay}{max delay to exclude syndrom if same (not excluded if over this delay)}
#'  \item{active.max.freq}{max delay between 2 surveys (in weeks)}
#'  \item{active.day.before}{Number of week before each computed week to count active participants}
#'  \item{active.day.after}{Number of week after each computed week to count active participants}
#'  \item{active.min.surveys}{min number of surveys for each participant (not active if less)}
#'  \item{active.use.mean}{Use mean number of active participant over the windows [active.day.before, active.day.after]}
#' }
#' @export
IncidenceDailyRS2014 = R6Class("IncidenceDailyRS2014", public = incidence_rs2014_daily_public)

