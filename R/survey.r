##
# survey data managements
# Functions dedicated to get surveys data and definitions from
##

#' aliases for symptom columns
#'
#' Aliases are meaningfull names for each columns of the survey data. InfluenzaNet surveys data are named using simple names (Q + number)
#' but these names are not errorproof and are hard to memorize.
#'
#' These aliases are defined in the weekly survey definition in the platform file using
#' @seealso \code{\link{platform_define_survey}}
#' They should not vary from one season to another (another column means another name)
#' @return character vector for variable names
#' @export
get_symptoms_aliases <- function() {
  def = survey_definition("weekly")
  def$labels$symptoms
}

#' Convert DB column name (InfluenzaNet column names in the db) from (revert=F) and to question variable name (revert=T)
#'
#' Description of mapping are defined in the survey  by \code{\link{platform_define_survey}} in the platform file
#' @param cols names of columns to rename
#' @param def list of aliases
#' @param revert if TRUE, aliases to Db names, if false (default) DB names to aliases
#' @export
survey_aliases <- function(cols, def, revert=F) {
  aliases = def$aliases
  if(revert) {
    n = unlist(aliases)
    aliases <- names(aliases)
    names(aliases) <- n
  }
  f = match(cols, names(aliases))
  cols[ !is.na(f) ] = unlist(aliases[ f[ !is.na(f) ] ])
  cols
}

#' Get the default language used for survey
#' Uses 'survey_default_language' variable that should be defined in platform file
#' @export
survey_default_language = function() {
  # survey_default_language should be defined in platfrom
  return(.Share$survey_default_language)
}

#' Return the "labels" for a given question variable name
#'
#' If it is a multichoice question, it will return the names of variable containing all the responses choices for this question
#' If it is a single choice question, it will return a list of language independent labels (but human meaningful)
#'
#' These labels are defined in the survey  by \code{\link{platform_define_survey}} in the platform file
#'
#' @param survey name of the survey
#' @param question name of the question variable, or if multichoice name of the question group (sometimes a more generic name)
#' @return character vector of names
#' @export
survey_labels <- function(survey, question) {
	def = survey_definition(survey)
	labels = def$labels[[question]]
	if(length(labels) == 1) {
	  pattern = glob2rx(labels)
	  n = names(def$aliases)
	  labels = n[ grep(pattern,n) ]
	}
	labels
}

#' Get list of variable names matching the given pattern (glob style)
#'
#' These labels are defined in the survey by \code{\link{platform_define_survey}} in the platform file
#'
#' @param survey survey name
#' @param pattern glob style pattern (ex "visit.*")
#' @return character vector of questions variable names
#' @export
#' @importFrom utils glob2rx
#'
#' @examples
#' \dontrun{
#'  survey_questions_like("weekly", "visit.*") # All variables starting with visit.*
#' }
#'
survey_variables_like <- function(survey, pattern) {
  p = glob2rx(pattern)
  def = survey_definition(survey)
  n = names(def$aliases)
  n[grep(p, n)]
}

#' @rdname survey_variables_like
#' @export
survey_questions_like <- survey_variables_like

#' Recode data from the DB storage code to the R labels (or to its translaction into default language)
#'
#' This function recode from database values to labels (more meaningful). The recoding mapping for each variable is
#' defined \code{\link{platform_define_survey}} usually in the platform file
#'
#' @param x values to recodes
#' @param variable name of the question variable name to use for recoding
#' @param survey name of the survey
#' @param translate if TRUE, try to translate the labels (using i18n function @seealso i18n)
#' @param question old parameter name for variable, for compatibility
#' @return vector of recoded value in factor
#' @export
survey_recode <- function(x, variable, survey, translate=F, question=NULL) {

  if(!is.null(question)) {
    variable = question
  }

  if(is.factor(x)) {
    rlang::abort("Cannot recode a factor, already recoded ?")
  }

  recodes = survey_variable_recoding(survey = survey, name=variable, must.exists = TRUE)

  if(is.null(labels)) {
    rlang::abort(sprintf("Unknown labels for variable %s", variable))
  }

  recode_var(x, recodes, translate=translate)
}

#' Recode values with a given mapping
#' @param x value to recode
#' @param mapping list of mapping db value to label, label as name and db value as value
#' @param translate if TRUE translate labels with \code{\link{i18n}}
#' @return factor vector of recoded values
recode_var <- function(x, mapping, translate=FALSE) {
  codes = as.vector(mapping)
  labels = names(mapping)

  if(length(codes) != length(labels)) {
    rlang::abort("codes and labels should have exact same length", mapping=mapping)
  }

  if(translate) {
    labels = i18n(labels)
  }

  factor(x, codes, labels)
}

#' Get the recoding mapping of a variable in a survey
#'
#' @param survey character survey name
#' @param name variable name
#' @param must.exists if TRUE an error will be raised if name doesnt exists in survey definition
#' @return vector with label as name, database value as value
#' @export
survey_variable_recoding <- function(survey, name, must.exists=TRUE) {
  def = survey_definition(survey)
  recodes = def$recodes[[ name ]]
  if(is.null(recodes) && must.exists) {
    rlang::abort(sprintf("Unknown codes for variable name %s", name))
  }
  recodes
}

#' Get all the recodings defined in a survey
#'
#' @param survey character survey name
#'
#' @return list with question variable name in name, a mapping as value
#' @export
survey_recodings <- function(survey) {
  def = survey_definition(survey)
  return(def$recodes)
}

#' Returns TRUE if the survey's data are store using a single table model
#' @return logical
#' @param survey survey name
#' @export
survey_single_table <- function(survey) {
  def = survey_definition(survey)
  isTRUE(def$single.table)
}

#' Get survey definition
#' Survey definition is a data structure of entries defining variable mapping (db column name to variable name, recoding, variable sets)
#' They are defined by \code{\link{platform_define_survey}} usually in the platform file
#' @param survey survey name
#' @return list parameters describing a survey
#'
#' @export
survey_definition = function(survey) {
  def = .Share$epiwork.tables[[ survey ]]
  if( is.null(def) ) {
    rlang::abort(paste0("Unknown survey definition for '", survey,'"'))
  }
  def
}

#' Keep the last survey for each participant
#'
#' Some survey could have exactly the same timestamp
#' So using this function is safer than only using timestamp
#'
#' @param data data.frame with at least (timestamp,person_id, id) columns
#' @export
keep_last_survey = function(data) {
  data = data[ order(data$person_id, data$timestamp), ]
  ii = aggregate(id ~ person_id, data=data, tail, n=1)
  data = merge(ii, data, by='id', all.x=T, suffixes=c('','.1'))
  data = subset(data, select=-person_id.1)
  data
}


#' Get historical season definition
#'
#' Each season should be described in the platform using \code{\link{platform_season_history}}
#' historical.tables is a list with an entry for each season (season = year number of the first september in the season)
#' \describe{
#'  \item{intake}{table containing the intake survey data for the season}
#'  \item{weekly}{table containing the weekly survey data for the season}
#'  \item{health}{table/view containing the health status for each weekly for the season}
#'  \item{year.pop}{population year to use}
#'  \item{dates}{list(start, end), starting and ending date of the season}
#' }
#' This entries are defined
#' @param season season definition to get
#' @param silent boolean (not used.)
#' @return return the entry of historical.tables config for the season (first year of the season)
#' @export
season.def = function(season, silent=F) {
  h = .Share$historical.tables[[as.character(season)]]
  if( is.null(h) ) {
    stop(paste("Unknown season", season,"in historical tables"))
  }
  h
}

#' Get list of available season names
#'
#' @seealso \code{\link{concepts}}
#'
#' @return character vector of season names
#' @export
get_historical_seasons = function() {
  names(.Share$historical.tables)
}


#' Get Historical tables
#'
#' Historical tables is a list, with season year (first year of the season) as name and season definition
#' @seealso Season in \code{\link{concepts}}
#'
#' @seealso season.def
#' @export
get_historical_tables <- function() {
  .Share$historical.tables
}


