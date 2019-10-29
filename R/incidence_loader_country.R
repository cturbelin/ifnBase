#' Load data for incidence computation
#'
#' This function standardize the loading of data in order to compute incidence
#'
#' @param season season int[1] number to load (first year of the season, ex: 2011 for the season 2011-2012)
#' @param age.categories int[], breaks to compute age groups
#' @param geo geographic level name to load
#' @param syndrom.from list() parameters to create the syndrom columns it will be used to call \code{\link{compute_weekly_syndroms}}
#' @param country country to load
#' @param first.season first season participants handling parameters. could be "previous" (only for the previous season) or TRUE to activate it
#' @param extra list() extra parameters
#'
#' @details syndrom.from:
#' syndrom parameter will indicate how to create syndrom columns in weekly. A syndrom column is just a logical value column indicating if a weekly survey match a syndrom definition
#' This list will be used as arguments to call \code{\link{compute_weekly_syndroms}}
#'
#' @details extra:
#' Several extra parameters can be provided:
#' \describe {
#'  \item{weekly.all.columns}{if TRUE keep all column in weekly data, if FALSE only keep a restricted list}
#'  \item{weekly.supp.cols}{Supplementary weekly columns to load}
#' }
#' @export
#' @return list() with intake, weekly, syndroms (vector of name of syndrom columns)
load_results_for_incidence = function(season, age.categories, syndrom.from=list(), geo=NULL, country=NULL, first.season=NULL, extra=list()) {

  syndrom.from = swMisc::merge_list(syndrom.from, list(health.status=TRUE))

  hh = season.def(season)

  # Load data for incidences calculation

  weekly = survey_load_results("weekly", c(get_columns_for_incidence(), extra$weekly.supp.cols), season=season, country = country)

  if(nrow(weekly) == 0) {
    cat("No data for this season\n")
    return(NULL)
  }

  i = is.na(weekly$person_id)
  if( any(i) ) {
    message("Removing ", sum(i)," weekly with unknown person_id\n")
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
  if( isTRUE(syndrom.from$health.status) ) {
    weekly = survey_load_health_status(weekly, health.table=tables$health)
  }

  # get intake, only keep the last available intake
  # We should probably take the last intake available for each week
  intake.def = survey_definition("intake")
  intake.columns = c('timestamp', 'date.birth',  intake.def$geo.column)
  intake = survey_load_results("intake", intake.columns , geo=geo, season=season, country=country)

  i = is.na(intake$person_id)
  if( any(i) ) {
    message("Removing ", sum(i)," intake with unknown person_id\n")
    intake = intake[!i, ]
  }
  rm(i)

  # Complete intake for users that are not in the intake of the current season
  intake = complete_intake_strategy(weekly, intake)

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

  # Compute syndrom columns in weekly using syndrom.from parameter
  syndrom.from$intake = intake
  syndrom.from$weekly = weekly
  weekly = do.call(compute_weekly_syndroms, syndrom.from)

  # Get back list of syndrom columns
  syndroms = attr(weekly, "syndroms")

  if(!isTRUE(extra$weekly.all.columns)) {
    weekly = weekly[, c(keep.cols, syndroms)]
  }

  list(
    intake=intake,
    weekly=weekly,
    syndroms=syndroms
  )

}

#' Create syndrom columns in the weekly data
#'
#' This function use syndrom classifier (called provider)
#'
#'
#' @param weekly weekly data.frame()
#' @param intake intake data.frame()
#' @param health.status bool use the default health status computed using InfluenzaNet default strategy (used in the website)
#' @param regroup.syndrom bool use syndrom grouping (recode syndrom list for Influenzanet's health status list) to a simplier list
#' @param keep.status bool keep the original health status (from InfluenzaNet view), renamed to "status.old"
#' @param provider function(weekly,intake) returning a data.frame to be merged into weekly (using "id" weekly's column as merge key), useable to compute custom syndroms
#' @export
compute_weekly_syndroms <- function(intake, weekly, health.status=TRUE, keep.status=FALSE, regroup.syndrom=TRUE, provider=NULL) {

  # Use InfluenzaNet base health status
  if(health.status) {
    if(regroup.syndrom) {
      if(keep.status) {
        weekly$status.old = weekly$status
      }
      weekly$status = regroup.syndrom(weekly$status)
      syndroms = syndromes.set$grouped$levels
      names(syndroms) = syndroms
    } else {
      # get aliases from status from db and pretty names
      syndroms = syndromes.set$influenzanet.2012$pretty
    }

    # Create an indicator column for each levels of the 'status' column
    for(i in 1:length(syndroms)) {
      n = names(syndroms)[i]
      weekly[, syndroms[i] ] = ifelse( weekly$status == n, 1, 0)
    }

  }

  # Use an external syndroms provider to compute other definitions
  if( !is.null(provider) ) {
    r = provider(weekly, intake)
    n = names(r)
    n = n[ n != 'id'] # remove id column, as it is not a syndrom name
    weekly = merge(weekly, r, by='id', all.x=T)
    syndroms = c(syndroms, n)
    rm(r)
  }

  attr(weekly, "syndroms") <- syndroms
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

# # symptoms
# 'no.sympt'='Q1_0',
# 'fever'='Q1_1',
# 'chills'='Q1_2',
# 'rhino'='Q1_3',
# 'sneeze'='Q1_4',
# 'sorethroat'='Q1_5',
# 'cough'='Q1_6',
# 'dyspnea'='Q1_7',
# 'headache'='Q1_8',
# 'pain'='Q1_9',
# 'chestpain'='Q1_10',
# 'asthenia'='Q1_11',
# 'anorexia'='Q1_12',
# 'sputum'='Q1_13',
# 'wateryeye'='Q1_14',
# 'nausea'='Q1_15',
# 'vomiting'='Q1_16',
# 'diarrhea'='Q1_17',
# 'abdopain'='Q1_18',
# 'sympt.other'='Q1_19',


#'
#' Compute country specific classifier for ILI syndrom.
#' @param country country code
#' @param weekly weekly survey data
#' @param intake
#' @param season
#' @param use.fever.level if TRUE take into account of the reported fever level
#' @param unknown.has.fever = if TRUE consider that unknown fever level has fever at the right level
#' @param min.syndrom.count in case of imprecise definition about number of symptom, use this number as mininum to consider the syndrom
#'
syndrom_provider_ili_country = function(country, weekly, intake, season, use.fever.level=T, unknown.has.fever=T, min.syndrom.count=1) {

  # Count
  at_least = function(d, columns, n=1) {
    apply(select_df(d, columns), 1, function(r) { sum(r) >= n})
  }

  sudden = !is.na(weekly$sympt.sudden) & weekly$sympt.sudden

  if(use.fever.level) {
    has_fever_38 = weekly$fever & ifelse(is.na(weekly$highest.temp), unknown.has.fever, weekly$highest.temp >= 3)
    has_fever_39 = weekly$fever & ifelse(is.na(weekly$highest.temp), unknown.has.fever, weekly$highest.temp >= 4)
  } else {
    has_fever_38 = weekly$fever
    has_fever_39 = weekly$fever
  }

  ili = NULL


  if(country == "ES") {
    # The present definition of ILI case is the one proposed by the European Union:
    #
    #   - sudden onset of symptoms
    # - at least one of the four general symptoms: fever, malaise, headache, myalgia
    # - at least one of the three respiratory symptoms: cough, sore throat, dispnea
    # - lack of other suspected symptoms

    ili = sudden & at_least(weekly, c('fever', 'asthenia', 'headache', 'pain'), n=1) & at_least(weekly, c('cough', 'sorethroat', 'dyspnea'), n=1)

  }

  if(country == "IE") {
    # Influenza-like illness is characterised by the sudden onset of symptoms with a temperature of 38°C or more,
    # in the absence of any other disease, with at least two of the following: dry cough, headache, sore muscles and a sore throat.
    ili = has_fever_38 & sudden & at_least(weekly, c('cough', 'sorethroat', 'headache', 'pain'), n=2)

  }

  if(country == "IT") {
    # Since 2014-2015 the case definition has been modified to adapt it to the one used by ECDC:
    #   - sudden onset
    # - at least one of these general symptoms: fever, malaise, headache, muscle pain
    # - at least one of these respiratory symptoms: cough, sore throat, heavy breath
    #
    # Before 2014-2015 the definition would include the measurement of the fever:
    #   - sudden onset
    # - fever above 38 °C with at least one of the general symptoms: headache, malaise, chills, asthenia and at
    # least one of these respiratory symptoms: cough, sore throat, nasal congestion
    if(season >= 2014) {
      ili = sudden & at_least(weekly, c('fever', 'asthenia', 'headache', 'pain')) & at_least(weekly, c('cough','sorethroat','dyspnea'))
    } else {
      ili = sudden & has_fever_38 & at_least(weekly, c('headache', 'asthenia', 'chills', 'asthenia')) & at_least(weekly, c('cough','sorethroat','rhino'))
    }
  }

  if(country == "NL") {
    # Not the standardized WHO or EU case definition.
    # The case definition used according to the ‘PEL criteria': acute onset AND rectal temperature >38°C AND
    # at least one of the following symptoms: cough, coryza, sore throat, frontal headache, retrosternal pain, myalgia.
    # Provided by the Member State through a survey.

    ili = sudden & has_fever_38 & at_least(weekly, c('cough','rhino', 'sorethroat','headache','chestpain','pain'), n=1)
    # What about sneezing ?

  }

  if(country == "BE") {
    # Not the standardized WHO or EU ILI case definition. The case definition used is:
    # sudden onset of fever with respiratory symptoms AND general symptoms. Provided by the Member State through a survey
    ili =  sudden & at_least(weekly, c('headache',  'chills', 'asthenia','pain'), n=min.syndrom.count) & at_least(weekly, c('cough','sorethroat','rhino','dyspnea'), n=min.syndrom.count)
  }

  if(country == "SE") {
    ili = weekly$ili
  }

  if(country == "PT") {
    #
    # Início súbito,
    #
    # + 1 dos seguintes sintomas sistémicos:
    # -Febre ou febrícula,
    # -Mal-estar, debilidade, prostração,
    # -Cefaleia,
    # -Mialgias ou dores generalizadas.
    #
    # + 1 dos seguintes sintomas respiratórios:
    # - Tosse,
    # - Dor de garganta ou inflamação da mucosa nasal ou faríngea sem sinais respiratórios relevantes,
    # - Dificuldade respiratória.
    ili = sudden & at_least(weekly, c('fever','chills','asthenia', 'headache','pain'), n=1) & at_least(weekly, c('cough','sorethroat','rhino', 'sneeze','dyspnea'), n=1)

  }

  if(country == "DK") {
    # “The Danish sentinel ILI case def. is;
    # sudden onset of fever or feverishness (chills) AND any symptom of malaise, headache or muscle pain
    # AND at least one of the following symptoms: cough, sore throat or shortness of breath”.

    ili = sudden & at_least(weekly, c('fever','chills')) & at_least(weekly, c('asthenia', 'headache','pain'), n=1) & at_least(weekly, c('cough','sorethroat','dyspnea'), n=1)
  }

  if(country == "FR") {
    # respi = r$sorethroat | r$cough | r$dyspnea
    # fever = r$fever & (!is.na(r$highest.temp) & r$highest.temp %in% c(3:5))
    # Not exact definition, but the one selected on french's data study

    ili = sudden &  at_least(weekly, c('sorethroat','cough','dyspnea'), n=min.syndrom.count) & has_fever_39 & at_least(weekly, c('pain','headache'))

  }

  if(country == "UK") {
    # No specific definition..By default use
    ili = weekly$ili
  }

  ili
}

