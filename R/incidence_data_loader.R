#' Load data for incidence computation
#'
#' This function standardize the loading of data in order to compute incidence
#'
#' @param season season int[1] \code{\link{calc_season}}
#' @param age.categories int[], breaks to compute age groups
#' @param geo geographic level name to load
#' @param syndrome.from list() parameters to create the syndrome columns it will be used to call \code{\link{compute_weekly_syndromes}}
#' @param country country to load
#' @param first.season first season participants handling parameters. could be "previous" (only for the previous season) or TRUE to activate it
#' @param columns list() extra columns to load in each survey data (see details)
#'
#' @details syndrome.from:
#' syndrome parameter will indicate how to create syndrome columns in weekly. A syndrome column is just a logical value column indicating if a weekly survey match a syndrome definition
#' This list will be used as arguments to call \code{\link{compute_weekly_syndromes}}
#'
#' @details Columns:
#' Several extra parameters can be provided:
#' \describe{
#'  \item{keep.all}{if TRUE keep all column in weekly data, if FALSE only keep a restricted list}
#'  \item{weekly}{Supplementary weekly columns to load see \code{\link{survey_load_results}}}
#'  \item{intake}{Supplementary intake columns to load see \code{\link{survey_load_results}}}
#' }
#' @export
#' @return list() with intake, weekly, syndromes (vector of name of syndrome columns)
load_results_for_incidence = function(season, age.categories, syndrome.from=list(), geo=NULL, country=NULL, first.season=NULL, columns=list()) {

  syndrome.from = swMisc::merge_list(syndrome.from, list(health.status=TRUE))

  # Load data for incidences calculation

  weekly.columns = unique(c(get_columns_for_incidence(), columns$weekly))
  weekly = survey_load_results("weekly", weekly.columns, season=season, country = country)

  if(nrow(weekly) == 0) {
    cat("No data for this season\n")
    return(NULL)
  }

  i = is.na(weekly$person_id)
  if( any(i) ) {
    message("Removing ", sum(i)," weekly with unknown person_id")
    weekly = weekly[!i, ]
  }
  rm(i)

  weekly = recode_weekly(weekly, health.status=F)

  weekly$sympt.start = as.Date(as.character(weekly$sympt.start))
  weekly$fever.start = as.Date(as.character(weekly$fever.start))

  # Set date in the future to NA (not possible cases)
  weekly$sympt.start[ !is.na(weekly$sympt.start) & weekly$sympt.start > weekly$date ] <- NA
  weekly$fever.start[ !is.na(weekly$fever.start) & weekly$fever.start > weekly$date ] <- NA

  # Number of the weekly by participant
  weekly = calc_weekly_order(weekly)

  # Load InfluenzaNet default health status
  if( isTRUE(syndrome.from$health.status) ) {
    weekly = survey_load_health_status(weekly)
  }

  # get intake, only keep the last available intake
  # We should probably take the last intake available for each week
  intake.def = survey_definition("intake")
  intake.columns = unique(c('timestamp', 'date.birth',  intake.def$geo.column, columns$intake))
  intake = survey_load_results("intake", intake.columns , geo=geo, season=season, country=country)

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

  # Compute first season column for each participant
  # In some country it cannot be assessed because data are not available
  # In this case, first season is censored (assumed to be not the first season for all participants) before the known in this country
  if( !is.null(first.season) ) {
    censor.season = FALSE

    if( isTRUE(platform_env("first.season.censored") ) ) {
      # Do we need to censor this season
      get_first_season_country = platform_env("get_first_season_country")

      if(is.null(get_first_season_country) ) {
        stop("`get_first_season_country` is not defined for this platform")
      }

      # Apply censorship only if season is before this season for the country
      censor.season = season <= get_first_season_country(country)
    }

    if(censor.season) {
      # Cannot known if the participant are in first season for the given season
      # So assume that it is not the first season for all participants
      # This will deactivate the criteria based on the first survey's delay (ignore.first.delay)
      message("First season is censored all participant are not in first season")
      intake$first.season = FALSE
    } else {
      ss = NULL
      if( identical(first.season, "previous") ) {
        ss = -1
      }
      # Get list of previous seasons participants
      previous = survey_participant_previous_season(season, ids=intake$person_id, seasons=ss)
      intake$first.season = !intake$person_id %in% previous # first is not in previous season
    }

  }

  # check for some conditions
  stopifnot(all(table(weekly$id) == 1))

  # Columns to keep in weekly
  keep.cols = c('person_id','timestamp', 'date', 'order', 'same.episode','sympt.start','fever.start')

  # Compute syndromes columns in weekly using syndrome.from parameters as arguments
  syndrome.from$intake = intake
  syndrome.from$weekly = weekly
  weekly = do.call(compute_weekly_syndromes, syndrome.from)

  # Get back list of syndrome columns
  syndromes = attr(weekly, "syndromes")

  if(!isTRUE(columns$keep.all)) {
    weekly = weekly[, c(keep.cols, syndromes)]
  }

  list(
    intake=intake,
    weekly=weekly,
    syndromes=syndromes
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
#' @rdname complete_intake
complete_intake_strategy = function(data, intake, ...) {
  strategy = platform_env("complete.intake")
  # No strategy defined : do not complete
  if( is.null(strategy) ) {
    return(intake)
  }
  complete_intake(data, intake, ...)
}

#' Complete intake for a survey using intake from the past seasons
#' @param data data.frame() survey data with all participants should have at least (timestamp, person_id) columns. For example weekly survey data
#' @param intake data.frame() intake survey data loaded on the same period as data
#' @param intake.columns columns to load with intake
#' @param geo geo levels to load with intake
#' @return intake with extra intake loaded from previous season
#' @export
complete_intake = function(data, intake, intake.columns, geo=NULL, max.year=NA) {
  # Complete intake for users that are not in the intake of the current season

  p = unique(data$person_id[!data$person_id %in% intake$person_id])
  if(length(p) > 0) {
    message(paste("Completing intake from previous data for ", length(p)," participants"))
    dates = list()
    dates$max = min(intake$timestamp) # Before the first survey
    if( !is.na(max.year) ) {
      dates$min = dates$max - max.year
    }
    ii = survey_load_results("intake", intake.columns, survey.users=p, geo=geo, debug=F, date=dates)
    if( nrow(ii) > 0) {
      ii = keep_last_survey(ii)
      intake = dplyr::bind_rows(intake, ii)
    }
  } else {
    message("No need to complete intakes")
  }

  intake
}


