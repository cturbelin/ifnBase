##
# survey data managements
# Functions dedicated to get surveys data and definitions from
# an influenzaNet DB, following the plateform's definition
# Each platform has a platform file, places in share/platform containing all platform specific variable definition and functions

# sympt.aliases =
#' aliases for symptom columns
#' Aliases are meaningfull names for each columns of the survey data. InfluenzaNet surveys data are named using simple names (Q + number)
#' but these names are not errorproof and are hard to memorize.
#' epiwork.tables list defined in platform config file should describe the mapping between column names in the DB (Qxxx) and the column
#' names used in R.
#' They should not vary from one season to another (another column means another name)
#' @export
get_symptoms_aliases <- function() {
  def = survey_definition("weekly")
  def$labels$symptoms
}

#' Convert column name (InfluenzaNet column names) from (revert=F) and to aliases (revert=T)
#'
#' Description of mapping is set in the config variable epiwork.tables defined
#' in the platform configuration files
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

#' Return the "labels" for a given question
#'
#' If it is a multichoice question, it will return the names of columns containing all the responses choices for this question
#' If it is a single choice question, it will return a list of language independent labels (but human meaningful)
#' @param survey name of the survey
#' @param question name of the question, or if multichoice name of the question group (sometimes a more generic name)
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

#' Returns list of question aliases matching the given pattern (glob style)
#' @param survey survey name
#' @param pattern glob style pattern (ex "visit.*")
#' @export
#' @importFrom utils glob2rx
survey_questions_like <- function(survey, pattern) {
  p = glob2rx(pattern)
  def = survey_definition(survey)
  n = names(def$aliases)
  n[grep(pattern, n)]
}

#' Recode data from the DB storage code to the R labels (or to its translaction into default language)
#'
#' This function recode the code used in the DB and replace each values by a label more meaningful (like the column name but applied
#' on response values of a question). Mapping between DB codes and labels are described in the survey's section of epiwork.tables
#' (defined in platform file):
#' \describe{
#'  \item{codes}{section are the list of the DB codes}
#'  \item{labels}{section are the labels corresponding to each code (in the same order)}
#' }
#'
#' @param x values to recodes
#' @param question name of the question (alias here)
#' @param survey name of the survey
#' @param translate if TRUE, try to translate the labels (using i18n function @seealso i18n)
#' @export
survey_recode <- function(x, question, survey, translate=F) {
  def = survey_definition(survey)

  recodes = def$recodes[[ question ]]

  if(is.null(recodes)) {
    stop(sprintf("Unknown codes for question %s", question))
  }

  codes = as.vector(recodes)
  labels = names(recodes)

  if(is.null(labels)) {
    stop(sprintf("Unknown labels for question %s", question))
  }

  if(length(codes) != length(labels)) {
    stop(sprintf("invalid number of labels or codes for question %s", question))
  }

  if(translate) {
    labels = i18n(labels)
  }
  x = factor(x, codes, labels)
}

#' Returns TRUE if the survey's data are store using a single table model
#' @param survey survey name
#' @export
survey_single_table <- function(survey) {
  def = survey_definition(survey)
  isTRUE(def$single.table)
}

#' Get survey definition
#' Survey definition is a list
#' @param survey survey name
#' @export
survey_definition = function(survey) {
  def = .Share$epiwork.tables[[ survey ]]
  if( is.null(def) ) {
    stop("Unknown survey definition")
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
#' Each season should be described in the platform in historical.tables variables
#' historical.tables is a list with an entry for each season (season = year number of the first september in the season)
#' \describe{
#'  \item{intake}{table containing the intake survey data for the season}
#'  \item{weekly}{table containing the weekly survey data for the season}
#'  \item{health}{table/view containing the health status for each weekly for the season}
#'  \item{year.pop}{population year to use}
#'  \item{dates}{list(start, end), starting and ending date of the season}
#' }
#'
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
#' @export
get_historical_seasons = function() {
  names(.Share$historical.tables)
}


#' Get Historical tables
#'
#' Historical tables is a list, with season year (first year of the season) as name and season definition
#'
#' @seealso season.def
#' @export
get_historical_tables <- function() {
  .Share$historical.tables
}


