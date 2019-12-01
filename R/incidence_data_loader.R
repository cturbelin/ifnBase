#' Load data for incidence computation
#'
#' This function standardize the loading of data in order to compute incidence
#'
#' @param season season int[1] \code{\link{calc_season}}
#' @param age.categories int[], breaks to compute age groups
#' @param geo geographic level name to load
#' @param syndrome.from list() parameters to create the syndrome columns it will be used to call \code{\link{compute_weekly_syndromes}}
#' @param country country to load
#' @param first.season list of first season participants handling parameters (see details)
#' @param columns list() extra columns to load in each survey data (see details)
#' @param onset onset_design quosure expression used to compute onset date, see \code{\link{compute_onset}}, by default use \code{\link{base_onset_design}}
#' @details syndrome.from:
#' syndrome parameter will indicate how to create syndrome columns in weekly. A syndrome column is just a logical value column indicating if a weekly survey match a syndrome definition
#' This list will be used as arguments to call \code{\link{compute_weekly_syndromes}}
#'
#' @details first seasons:
#' By default nothing is done on first season.
#' if first season is TRUE, then use the default of all following parameters
#' Otherwise provide a list with these parameters
#' \describe{
#'  \item{censored_value}{value to put to all participant when season is censored for first participation, default is FALSE}
#'  \item{from}{From which seasons consider the participation ("previous" or "all"), default is "all" }
#' }
#'
#' @details Columns:
#' Several extra parameters can be provided:
#' \describe{
#'  \item{keep.all}{if TRUE keep all column in weekly data, if FALSE only keep a restricted list}
#'  \item{weekly}{Supplementary weekly columns to load see \code{\link{survey_load_results}}}
#'  \item{intake}{Supplementary intake columns to load see \code{\link{survey_load_results}}}
#'  \item{params}{list of parameters used as arguments but sometimes completed to an actual version, like onset if null is provided}
#' }
#'
#' @details First season computing:
#'
#' This feature compute a flag for each participant in the loaded intake indicating if the loaded season is the first participating season of the participant.
#'
#' In european database all seasons are not available for all countries. In this case a censoring can be applied (considering all participants are not in their first season).
#' A function named `get_season_censoring(country)` should be available in the platform definition returing for a given country the season until wich data should
#' be censored (if the loaded i season is before or equal to this returned season year, the data will be censored for all participants)
#'
#' @export
#' @return list() with intake, weekly, syndromes (vector of name of syndrome columns)
load_results_for_incidence = function(season, age.categories, syndrome.from=list(), geo=NULL, country=NULL, first.season=NULL, columns=list(), onset=NULL) {

  # We will need this packages
  requireNamespace("dplyr")
  requireNamespace("rlang")

  syndrome.from = swMisc::merge_list(syndrome.from, list(health.status=TRUE))

  # Output params list
  params = list(
    first.season = first.season,
    age.categories = age.categories,
    geo = geo,
    season = season,
    country = country,
    columns = columns
  )

  # Load data for incidences calculation

  weekly.columns = unique(c(get_columns_for_incidence(), columns$weekly))
  weekly = survey_load_results("weekly", weekly.columns, season=season, country = country)

  params$weekly.columns = weekly.columns

  if(nrow(weekly) == 0) {
    message("No data for this season\n")
    return(NULL)
  }

  i = is.na(weekly$person_id)
  if( any(i) ) {
    message("Removing ", sum(i)," weekly with unknown person_id")
    weekly = weekly[!i, ]
  }
  rm(i)

  # Load InfluenzaNet default health status
  weekly = recode_weekly(weekly, health.status=isTRUE(syndrome.from$health.status))

  # Number of the weekly by participant
  weekly = calc_weekly_order(weekly)

  # get intake, only keep the last available intake
  # We should probably take the last intake available for each week
  intake.def = survey_definition("intake")
  intake.columns = unique(c('timestamp', 'date.birth',  intake.def$geo.column, columns$intake))
  intake = survey_load_results("intake", intake.columns , geo=geo, season=season, country=country)

  params$intake.columns = intake.columns

  i = is.na(intake$person_id)
  if( any(i) ) {
    message("Removing ", sum(i)," intake with unknown person_id\n")
    intake = intake[!i, ]
  }
  rm(i)

  # Complete intake for users that are not in the intake of the current season
  intake = complete_intake_strategy(weekly, intake, intake.columns=intake.columns)

  # Current strategy keeps the last available survey data for each user
  # One participant will have one age and one location during all the season
  intake = keep_last_survey(intake)

  intake$age = calc_age(intake$date.birth, intake$timestamp) # @see share/lib/survey

  intake = subset(intake, select=-c(date.birth, timestamp)) # remove uneeded variables

  if( !is.null(age.categories) ) {
    intake$age.cat = cut_age(intake$age, age.categories)
  }

  params$censor.season = NA

  # Compute first season column for each participant
  # In some country it cannot be assessed because data are not available
  # In this case, first season is censored (assumed to be not the first season for all participants) before the known in this country
  if( !is.null(first.season) ) {
    censor.season = FALSE

    if( isTRUE(first.season) ) {
      first.season = list() # Use default parameters
    }

    if(!is.list(first.season)) {
      rlang::abort("first season should be either a list or TRUE")
    }

    first.season = swMisc::merge_list(first.season, list(censored_value=FALSE, from='all'))

    if(!is.logical(first.season$censored_value)) {
      rlang::abort("first.season$censored_value should be logical")
    }

    if(!first.season$from %in% c('all','previous')) {
      rlang::abort("first.season$from should 'all' or 'previous'")
    }

    if( isTRUE(platform_env("first.season.censored") ) ) {
      # Do we need to censor this season
      message("Looking for first season censoring")
      get_season_censoring = platform_env("get_season_censoring")

      if(is.null(get_season_censoring) ) {
        rlang::abort("`get_season_censoring` is not defined for this platform")
      }

      ss = get_season_censoring(country)
      if(!is.na(ss)) {
        # Apply censorship when the season is before or equal to the censored season
        censor.season = season <= ss
      }
    }

    if(censor.season) {

      # Cannot known if the participant are in first season for the given season
      # So assume that it is not the first season for all participants
      # This will deactivate the criteria based on the first survey's delay (ignore.first.delay)
      message("First season is censored all participant are not in first season")
      intake$first.season = first.season$censored_value
    } else {
      message("First season is not censored, fetching participants data")
      ss = NULL
      if( identical(first.season$from, "previous") ) {
        ss = -1
      }
      # Get list of previous seasons participants
      previous = survey_participant_previous_season(season, ids=intake$person_id, from=ss, country=country)
      intake$first.season = !intake$person_id %in% previous # first is not in previous season
    }
    params$censor.season = censor.season
  }

  # check for some conditions
  stopifnot(all(table(weekly$id) == 1))

  # Columns to keep in weekly
  keep.cols = c('id','person_id','timestamp', 'date', 'order', 'same.episode','sympt.start','fever.start')

  # Compute syndromes columns in weekly using syndrome.from parameters as arguments
  syndrome.from$intake = intake
  syndrome.from$weekly = weekly
  weekly = do.call(compute_weekly_syndromes, syndrome.from)

  # Get back list of syndrome columns
  syndromes = attr(weekly, "syndromes")

  if(!isTRUE(columns$keep.all)) {
    weekly = weekly[, c(keep.cols, syndromes)]
  }

  if(is.null(onset)) {
    onset = base_onset_design()
  }

  # Compute onset column given the strategy
  weekly = compute_onset(weekly, onset)

  params$onset = onset

  structure(
    list(
      intake=intake,
      weekly=weekly,
      syndromes=syndromes,
      params=params
    ),
    class="incidence_loader"
  )
}

#' Create syndrome columns in the weekly data
#'
#' This function use syndrome classifier (called provider)
#'
#'
#' @param weekly weekly data.frame()
#' @param intake intake data.frame()
#' @param health.status bool use the default health status computed using InfluenzaNet default strategy (used in the website)
#' @param regroup.syndromes bool use syndrome grouping (recode syndrome list for Influenzanet's health status list) to a simplier list
#' @param keep.status bool keep the original health status (from InfluenzaNet view), renamed to "status.old"
#' @param provider function(weekly,intake) returning a data.frame to be merged into weekly (using "id" weekly's column as merge key), useable to compute custom syndromes
#' @export
compute_weekly_syndromes <- function(intake, weekly, health.status=TRUE, keep.status=FALSE, regroup.syndromes=TRUE, provider=NULL) {

  # Use InfluenzaNet base health status
  syndromes = c()
  if(health.status) {
    if(regroup.syndromes) {
      if(keep.status) {
        weekly$status.old = weekly$status
      }
      weekly$status = regroup.syndrome(weekly$status)
      syndromes = syndromes.set$grouped$levels
      names(syndromes) = syndromes
    } else {
      # get aliases from status from db and pretty names
      syndromes = syndromes.set$influenzanet.2012$pretty
    }

    # Create an indicator column for each levels of the 'status' column
    for(i in 1:length(syndromes)) {
      n = names(syndromes)[i]
      weekly[, syndromes[i] ] = ifelse( weekly$status == n, 1, 0)
    }

  }

  # Use an external syndromes provider to compute other definitions
  if( !is.null(provider) ) {
    if(is_syndrome_provider(provider)) {
      r = provider$compute(weekly, intake)
    } else {
      r = provider(weekly, intake)
    }
    n = names(r)
    n = n[ n != 'id'] # remove id column, as it is not a syndrome name
    weekly = merge(weekly, r, by='id', all.x=T)
    syndromes = c(syndromes, n)
    rm(r)
  }

  attr(weekly, "syndromes") <- syndromes
  weekly
}

#' Complete intake according to platform's strategy
#' @export
#' @param data data.frame() survey data with all participants (typically weekly), see \code{\link{complete_intake}}
#' @param intake data.frame() intake data loaded on same period as data
#' @param ... other parameters to pass to \code{\link{complete_intake}}
complete_intake_strategy = function(data, intake, ...) {
  strategy = platform_env("complete.intake")
  # No strategy defined : do not complete
  if( is.null(strategy) ) {
    return(intake)
  }
  max.year = NA
  if(!is.null(strategy$max.year)) {
    max.year = strategy$max.year
  }
  complete_intake(data, intake, max.year=max.year, ...)
}

#' Complete intake for a survey using intake from the past seasons
#' @param data data.frame() survey data with all participants should have at least (timestamp, person_id) columns. For example weekly survey data
#' @param intake data.frame() intake survey data loaded on the same period as data
#' @param intake.columns columns to load with intake
#' @param geo geo levels to load with intake, list of names of levels to load see \code{\link{geo_level}}
#' @param max.year maximum number of year to get data before the minimal intake date
#' @param fix.timestamp logical. if TRUE, imported intake will have timestamp set to minimal date of \code{intake} parameter
#' @return intake with extra intake loaded from previous season
#' @export
complete_intake = function(data, intake, intake.columns, geo=NULL, max.year=NA, fix.timestamp=TRUE) {
  # Complete intake for users that are not in the intake of the current season

  p = unique(data$person_id[!data$person_id %in% intake$person_id])
  if(length(p) > 0) {
    message(paste("Completing intake from previous data for ", length(p)," participants"))
    dates = list()
    min_time = min(intake$timestamp)
    dates$max = as.Date(min_time) # Before the first survey
    if( !is.na(max.year) ) {
      dates$min = dates$max - (max.year * 365)
    }
    ii = survey_load_results("intake", intake.columns, survey.users=p, geo=geo, debug=F, date=dates)
    if( nrow(ii) > 0) {
      ii = keep_last_survey(ii)
      intake$complete = FALSE
      ii$complete = TRUE
      if(fix.timestamp) {
        ii$timestamp.org = ii$timestamp
        ii$timestamp = min_time
      }
      intake = dplyr::bind_rows(intake, ii)
    }
  } else {
    message("No need to complete intakes")
  }

  intake
}


