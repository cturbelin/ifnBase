
#' Prepare data for incidence computation with RS2014 method
#' compute first list of participants and filter with some parameters (with constant effects over time)
#' @param weekly weekly data.frame()
#' @param intake intake data.frame()
#' @param syndrom chr[] list of column names containing syndrom state for each participant
#' @param params list list of parameters used to compute incidence
#' @param profile logical profile computation steps
incidence_rs2014_prepare_data = function(weekly, intake, syndrom, params, profile=FALSE) {

  ignore.first = params$ignore.first
  ignore.first.delay = params$ignore.first.delay
  ignore.first.only.new = params$ignore.first.only.new

  exclude.same  = params$exclude.same
  exlude.same.delay = params$exclude.same.delay
  active.max.freq = params$active.max.freq
  active.min.surveys = params$active.min.surveys
  onset.columns = params$onset.columns

  start.time = Sys.time()
  last.time = start.time
  track = function(step) {
    time = Sys.time()
    cat("time ", step, round(1000 * as.numeric(time - last.time)), "ms\n")
    last.time <<- time
  }

  if( ignore.first ) {
    # @Deprecated param
    # Remove first weekly survey
    # To discuss : should a first survey in the previous period (with no survey in the current week on calculation (week.point)
    # be counted as active participant (and only remove if the first survey is in the current week period ?)
    weekly = weekly[ weekly$order > 1, ]
  }

  # weekly = calc.weekly.order(weekly)

  weekly$date = as.Date(trunc(weekly$timestamp, "day"))
  if(profile) track("date")

  if(exclude.same) {
    weekly = weekly[ order(weekly$person_id, weekly$timestamp), ]
    weekly$delay.date = calc.weekly.delay(weekly, "date")
    same = !is.na(weekly$same.episode) & weekly$same.episode == "Yes"  & (is.na(weekly$delay.date) | weekly$delay.date <= exlude.same.delay) # recoded value !
    for(ss in syndrom) {
      # Cancel a syndrom report if flagged as same episode as previous
      i = !is.na(weekly[, ss]) & weekly[, ss] > 0
      weekly[ same & i, ss ] = 0
    }
    if(profile) track("exclude-same")
  }

  if( is.null(onset.columns) ) {
    # Default behaviour
    # onset = first available date from symptom start, fever.start and survey date
    onset.columns = c('fever.start', 'sympt.start', 'date')
  }
  weekly$onset = as.Date(apply(weekly[, onset.columns ], 1, coalesce))
  if(profile) track("onset")

  # @TODO
  # Actual strategy will remove aberrant data (if fever.start or sympt.start are too far)
  # Depending on params the participant will be counted as active or not


  weekly$yw = iso_yearweek(weekly$onset)
  if(profile) track("yw")

  if( !is.na(ignore.first.delay) ) {
    # We need to keep first date
    first.date = aggregate(list(date.first=weekly$onset), list(person_id=weekly$person_id), min)
    if(profile) track("first.date")
  }

  # Now aggregate to the week and person
  weekly = aggregate(as.list(weekly[, syndrom]), list(person_id=weekly$person_id, yw=weekly$yw), sum, na.rm=T)
  if(profile) track("syndrom-agg")
  weekly[, syndrom] = weekly[, syndrom] > 0 # Only one syndrom report by participant by week
  if(profile) track("syndrom")

  weekly = weekly[ order(weekly$person_id, weekly$yw), ]
  weekly$delay.previous = calc.weekly.delay(weekly, time.col='yw')
  if(profile) track("delay.previous")

  participant = aggregate(list(yw.first=weekly$yw), list(person_id=weekly$person_id), min)
  if(profile) track("participant")

  if( !is.na(ignore.first.delay) ) {
    # Need date of first "survey" (actually, onset) for this parameter
    participant = merge(participant, first.date , by='person_id', all.x=T)

    if( isTRUE(ignore.first.only.new) ) {
      if( is.null(intake$first.season) ) {
        stop("ignore.first.only.new option requires intake to have a 'first.season' column indicating if participant is new for the season")
      }
      participant = merge(participant, intake[, c('person_id','first.season')], by='person_id', all.x=T)
    }
    if(profile) track("ignore.first")

  }

  participant = merge(participant, aggregate(list(yw.last=weekly$yw), list(person_id=weekly$person_id), max), by='person_id')
  if(profile) track("yw.last")

  if( !is.na(active.min.surveys) ) {
    weekly$nb = 1
    participant = merge(participant, aggregate(nb ~ person_id, data=weekly, sum), by='person_id')
    if(profile) track("min.suveys")
  }

  if(!is.na(active.max.freq)) {
    participant = merge(participant, aggregate(delay.previous ~ person_id, data=weekly, max, na.rm=T), by='person_id')
    if(profile) track("min.suveys")
  }

  list(weekly=weekly, participant=participant)
}


#' Internal function
#' Estimate incidence with rs2014 method for a given week
#' @param weekly (cleaned data @see incidence_rs2014_prepare_data )
#' @param intake
#' @param syndrom list of column names
#' @param participant selected participants data (precomputed by incidence_rs2014_prepare_data)
#' @param yw yearweek number to compute
#' @param desgin stratification design (@see design.incidence)
#' @param output output definition (@see output.incidence)
estimate_incidence_rs2014_week = function(weekly, intake, syndrom, participant, yw,
                                          params,
                                          design,
                                          output = c('inc','zlow','age'), verbose=T, profile=FALSE)
{

  active.week.before = params$active.week.before
  active.week.after = params$active.week.after
  active.max.freq = params$active.max.freq # max delay between 2 surveys (in weeks)
  active.min.surveys = params$active.min.surveys
  ignore.first.delay = params$ignore.first.delay
  ignore.first.only.new = params$ignore.first.only.new # only ignore first survey for new participant for this season

  select.count = list()

  # Track count of participants (for introspection)
  track <- function(name, n=NULL) {
    if(profile) {
      track_time(name)
    }
    if( is.null(n) ) {
      n = nrow(participant)
    }
    if(verbose) {
      cat(" * ", name,"=", n, "\n")
    }
    select.count[[name]] <<- n
  }

  start.time = Sys.time()
  last.time = start.time

  track_time = function(m) {
    time = Sys.time()
    cat(m, round(1000 * as.numeric(time - last.time)), " ms.\n")
    last.time <<- time
  }

  if( is.null(output) ) {
    output =  eval(formals()$output)
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

  strata = design$strata

  if( is.null(strata) ) {
    # Use dummy strata column
    # It more costly than avoid aggregation but this garantees that the same procedure is used to compute regardless design
    intake$dummy = 1
    strata = 'dummy'
  }

  participant$active = 1

  # active participant = having at least one survey on 3 weeks
  participant = merge(participant, intake[, c('person_id', strata)], by='person_id', all.x=T) # get the geo code
  if(profile) track_time("merge_intake")

  # cat("Strata: ", paste(strata,collapse=','))

  active.week = aggregate(list(active=participant$active), agg.group(participant, strata), sum)
  if(profile) track_time("active.week")

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

  # Count number of syndrom by user and by week
  count = aggregate(as.list(weekly[, syndrom]), list(person_id=weekly$person_id), sum)
  if(profile) track_time("count")

  count = merge(count, intake[,c('person_id', strata)], by='person_id', all.x=T) # get the geo code for each user
  if(profile) track_time("count.intake")

  # make syndrom exclusive in a week ?
  # nop now

  count[, syndrom] = as.integer(count[, syndrom] > 0) # syndrom counted only once for each user

  # aggregate by strata
  count.week = aggregate( as.list(count[, syndrom]), agg.group(count, strata), sum)
  if(profile) track_time("count.week")

  # merge with active
  count.week = merge(active.week, count.week, by=strata, all.x=T)
  if(profile) track_time("merge.count.week")

  if( !(length(output) == 1 && is.na(output) )) {
    # Compute incidence from count data
    r = calc_adjusted_incidence(count.week, design=design, syndroms=syndrom, output=output)
    if(profile) track_time("calc.incidence")

  } else {
    r = list(count = count.week)
  }

  attr(r, "select.count") <- select.count

  r
}

#' Compute weekly incidence for all requested weeks
#' Apply the full algorithm for rs2014 incidence computation
#' prepare data and then compute for each week by calling estimate_incidence_rs2014_week
estimate_incidence_rs2014 = function(weekly, intake, syndrom, params, design, output=NULL, weeks=NULL, verbose=T, progress=F, profile=FALSE) {

  # params default values
  def = list(
    ignore.first.delay = NA, # Number of days form his first survey befotre to include a participant (not compatible with ignore.first)
    ignore.first = F,  # Remove all first surveys (regardless history, @deprecated)
    ignore.first.only.new = F, # Ignore first rule only for new participant (uses intake$first.season column @see load_results_for_incidence)
    exclude.same = F, # should exclude syndrom if same episode is on (partcipant still active)
    exclude.same.delay=15, # max delay to exclude syndrom if same (not excluded if over this delay)
    active.max.freq=NA, # max delay between 2 surveys (in weeks)
    active.week.before=NA, # Number of week before each computed week to count active participants
    active.week.after=NA, # Number of week after each computed week to count active participants
    active.min.surveys=NA # min number of surveys for each participant (not active if less)
  )

  params = default.params(params, def)

  syndrom = as.vector(syndrom)
  names(syndrom) = NULL # Be sure no names is defined

  # Prepare data for all weeks (common part)
  if(verbose) {
    str(params)
    cat("Preparing data...")
  } else {
    if(progress) {
      cat('~')
    }
  }

  d = incidence_rs2014_prepare_data(weekly, intake, syndrom, params, profile=profile)

  weekly = d$weekly
  participant = d$participant
  rm(d)

  if(verbose) {
    cat("done.\n")
  }

  inc = NULL
  inc.zlow = NULL
  inc.age = NULL
  count = NULL
  selection = NULL

  if( is.null(weeks) ) {
    weeks = unique(weekly$yw)
  }
  for(w in weeks) {
    if(verbose) {
      cat("Week",w,"\n")
    } else {
      if(progress) {
        cat(".")
      }
    }
    r = estimate_incidence_rs2014_week(weekly=weekly, participant=participant, intake=intake, yw=w, syndrom=syndrom,  params=params, design=design, output=output, verbose=verbose, profile=F)
    if( !is.null(r$inc) && nrow(r$inc) > 0 ) {
      ii = r$inc
      ii$yw = w
      inc = bind_rows(inc, ii)
    }
    if( !is.null(r$zlow) && nrow(r$zlow)  > 0) {
      ii = r$zlow
      ii$yw = w
      inc.zlow = bind_rows(inc.zlow, ii)
    }
    if( !is.null(r$age) && nrow(r$age)  > 0) {
      ii = r$age
      ii$yw = w
      inc.age = bind_rows(inc.age, ii)
    }
    if( !is.null(r$count) && nrow(r$count)  > 0) {
      ii = r$count
      ii$yw = w
      count = bind_rows(count, ii)
    }
    if("selection" %in% output) {
      ii = attr(r, "select.count")
      ii = data.frame(step=names(ii), count=unlist(ii))
      ii$yw = w
      selection = bind_rows(selection, ii)
    }
  }
  if(progress) {
    cat("\n")
  }
  structure(
    list(inc=inc, inc.zlow=inc.zlow, inc.age=inc.age, count=count,selection=selection),
    syndroms = syndrom,
    design=design,
    params=params,
    method="rs2014"
  )
}
