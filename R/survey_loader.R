## Survey loader function
## need db connexion
##
# Load informations about surveys

#' load options of a question from the database, identified by its question_id
#' low level function
#' @param question_id int[]
#' @param translation_id id the translation to use
#' @family survey-load
#' @return data.frame() option_id, text
#' @export
survey_load_options <- function(question_id, translation_id=NULL) {
  r = dbQuery('SELECT id as option_id, text, value, question_id from pollster_option where question_id IN(', paste(question_id, collapse=',') ,")")
  if( !is.null(translation_id) ) {
    trans = dbQuery('SELECT option_id, text from pollster_translationoption where translation_id=', translation_id)
    r = merge(r, trans, by='option_id', suffixes=c('.en', ''))
  }
  r
}

#' load questions of a survey from the database
#' @family survey definition functions
#' @param survey survey name (@see epiwork.tables)
#' @family survey-load
#' @param translation_id id of the translation to use
#' @export
survey_load_questions <- function(survey, translation_id=NULL) {
  def = survey_definition(survey)
  survey_id = def$survey_id
  r = dbQuery('SELECT id as question_id, title, type, data_name from pollster_question where survey_id=',survey_id)
  if( isTRUE(translation_id) ) {
    tt = survey_load_translation(survey=survey)
    if(nrow(tt) > 0 ) {
      translation_id = tt$translation_id[1]
    } else {
      translation_id = NULL
      warning(paste("Unable to find translation for survey", survey,"for default language"))
    }
  }

  if( !is.null(translation_id) ) {
    trans = dbQuery('SELECT question_id, title from pollster_translationquestion where translation_id=',translation_id)
    r = merge(r, trans, by='question_id', suffixes=c('.en',''))
  }
  r
}


#' load a translation for a survey for a given language
#' @param survey survey name
#' @param language code (as used in the db)
#' @family survey-load
#' @export
survey_load_translation <-function(survey, language=survey_default_language()) {
  def = survey_definition(survey)
  survey_id = def$survey_id
  dbQuery("SELECT id as translation_id, title from pollster_translationsurvey where survey_id=",survey_id," and language='",language,"'")
}


#' get the question_id (id in the question table in the influenzanet db) of a question
#' @param survey name of the survey
#' @param varname variable name of the question (db's column name or R alias of the column)
#' @param language language translation to use
#' @return int
#' @family survey-load
#' @export
survey_question_id <- function(survey, varname, language=survey_default_language()) {
  def = survey_load_all(survey, language=language)
  varname = survey_aliases(varname, def)
  def$questions$question_id[ def$questions$data_name %in% varname ]
}

#' Get the options for a survey's varname
#' This is not the "options" defined
#' @param survey survey name
#' @param varname variable name
#' @family survey-load
#' @export
survey_options_for <- function(survey, varname) {
  id = survey_question_id(survey, varname)
  def = survey_definition(survey)
  def$options[ def$options$question_id %in% id,]
}

#' Load all definition data about a survey
#' @param survey survey name
#' @param language translation language to use, by default use \code{survey_default_language}
#' @return definition of the survey (also update the global definition in memory)
#' @family survey-load
#' @seealso survey_default_language
#' @export
survey_load_all <- function(survey, language=survey_default_language()) {
  def = survey_definition(survey)
  if( !is.null(def$questions) ) {
    return(def)
  }
  tr = survey_load_translation(survey, language)
  def$translation_id = tr$translation_id
  def$questions = survey_load_questions(survey, tr$translation_id)
  def$options = survey_load_options(def$questions$question_id, tr$translation_id)
  .Share$epiwork.tables[[survey]] <- def # update the table definition
  def
}
