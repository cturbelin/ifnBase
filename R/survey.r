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

#' load a survey results set
#'
#' survey_load_results loads results set for a given survey. It handles several features:
#'  \itemize{
#'    \item mapping of column names (in the DB) to meaningful names (defined in epiwork.tables)
#'    \item merging with geographic tables (geo parameters set geographic level to use)
#'    \item selecting data from a season (automatic table and/or dates bound selection)
#' }
#'
#' Note : "global_id" is not used here we use person_id (sequential index of the user = primary key of survey_surveyuser table) instead because it is really cheaper for memory
#'
#' @param survey : survey name (as defined in epiwork.tables)
#' @param cols column list (you can use aliases)
#' @param geo code for the geo levels to add to the results set (only if the survey contains a geographic column)
#' @param date list(min=,max=) of min and max date to restrict the loaded data (use of the timestamp of the survey)
#' @param db.table override table name (to hanlde backup tables or test table)
#' @param survey.users list of id of SurveyUsers, load data only for these ids
#' @param account boolean if TRUE include "account" id (account user id). Only works if "user" column is defined
#' @param where where condition to apply on the query (caution, no check)
#' @param season if not NULL, season number, will load data for a given season, by default load all data from "pollster_results_[survey]"
#' @param channel if not null get data only for a given channel value
#' @param cols.sup supplementary column or SQL select clauses (caution no check)
#' @param gid if TRUE load gid column
#' @param debug if TRUE show query
#' @param country country name (if survey data has country column)
#' @return data.frame data of the survey
#' @export
survey_load_results = function(survey, cols, geo=NULL, date=NULL, db.table=NULL, survey.users=NULL, debug=F, account=F, where=c(), season=NULL, channel=NULL, cols.sup=c(), gid=F, country=NULL) {
 def = survey_definition(survey)
 if( !is.null(season) ) {
   h = season.def(season)
   if( is.null(db.table) ) {
     db.table = h[[survey]]
   }
   if( isTRUE(def$single.table) ) {
     # All seasons data are in one single table
     # Use "dates" of the season to load data, or use limits from "date" parameter if defined (and requested dates included in the season's ones)
     # end date could be null (during the current season) & is replaced by the current data & time
     d = lapply(h$dates, function(x) { if(!is.null(x)) { as.POSIXct(x) } else { Sys.time()} })
     if( is.null(date) ) {
       date = list(min=d$start, max=d$end)
     } else {
       # Check that the requested data are included
       date = lapply(date, as.POSIXct)
       if(!is.null(date$min)) {
         if( !( date$min >= d$start && date$min <= d$end )) {
           stop(paste("Requested minimum date", date$min," is not included in the season period (", d$start,"-", d$end,")"))
         }
       } else {
         date$min = d$start
       }
       if( !is.null(date$max) ) {
         if( !( date$max >= d$start && date$max <= d$end )) {
           stop(paste("Requested minimum date", date$min," is not included in the season period (", d$start,"-", d$end,")"))
         }
       } else {
         date$max = d$end
       }
     }
   }
 }
 if( is.null(db.table) ) {
   cat("loading survey using default table\n")
   tb = def$table
 } else {
   cat("loading survey using table ", db.table,"\n")
   tb = db.table
 }
 if( !is.null(date) ) {
   cat("Loading data from ", date$min, "to", date$max,"\n")
 }
 if( length(cols) == 1 && (cols == "*") ) {
    cc = "p.*"
 } else {
    cols = survey_aliases(cols, def)
    cc = paste('p."', cols, '"', sep='')
    cc = c('p.id as id', cc)
 }
 if(account) {
   cc = c("s.user_id as account_id", cc)
 }
 if(gid) {
   cc = c(cc, "p.global_id")
 }
 if( !is.null(geo) ) {
     if( is.null(def$geo.column) ) {
       stop("geographic column not defined for this column")
     }
     geo.column = def$geo.column
     gg = c()
     if( is.logical(geo) ) {
        gg = unlist(.Share$geo.levels)
     } else {
        gg = .Share$geo.levels[geo]
     }
     cc = c(cc, gg)
     if(debug) {
       cat("Joining geo table g, including columns", paste(gg, collapse=', '),"\n")
     }
     geo.levels.table = attr(.Share$geo.levels, "table")
     j = paste0(' left join ', geo.levels.table,' g on g."', .Share$geo.levels[[.Share$geo.level.base]],'"=p."', geo.column, '"')
     if( isTRUE( attr(.Share$geo.levels,"join.country") ) ) {
       j = paste(j,' and g."country"=p."country"')
     }
 } else {
    j = ''
 }
 if( !is.null(survey.users) ) {
    where = c(where, paste0('s.id IN(', paste(survey.users, collapse=','), ')'))
 }
 if( !is.null(date) ) {
    if( !is.null(date$min) ) {
       where = c(where, paste("timestamp >= '", date$min, "'", sep=''))
    }
    if( !is.null(date$max) ) {
      m = paste0(as.Date(date$max) + 1,' 00:00:00') # last day + 1
      where = c(where, paste("timestamp < '",m,"'", sep=''))
    }
 }

 #
 # Disable channel if explicitly set to NA

 if( !is.null(channel) ) {
   if( !is.na(channel) ) {
    where = c(where, paste('"channel"=\'', channel,'\'',sep=''))
    cat("Fetching channel=",channel,"\n")
   } else {
     cat("Fetching all channels\n")
   }
 } else {
   use_channel = get0("platform.use.channel", ifnotfound = FALSE, envir = .Share)
   if(use_channel) {
     cat("Fetching only empty channel\n")
     where = c(where, "channel=''")
   }
 }

 if( !is.null(country) ) {
    if( is.null(def$aliases$country) ) {
        stop("Country column not set for this table, unable to use 'country' parameter")
    }
    if(length(country) > 0) {
        country = toupper(country) # fix compat with incidence
        if(any(!country %in% .Share$COUNTRY_CODES)) {
            stop(paste("Unknown country codes :", paste(country[!country %in% .Share$COUNTRY_CODES], collapse = ",")))
        }
        country = paste0("'",country,"'")
        where = c(where, paste("p.country IN(", paste(country, collapse = ','),")"))
    }
 }

 cc = c(cc, cols.sup)
 cc = paste(cc, collapse=',')
 if(length(where) > 0) {
    where = paste(' WHERE ', paste(where, collapse=' AND '), sep='')
 } else {
    where = ''
 }
 query = paste('SELECT s.id as person_id, ',cc,' from ',tb,' p left join survey_surveyuser s on p.global_id=s.global_id', j, where, sep='')
 if(debug) {
   cat(query, "\n")
 }
 r = dbQuery(query)
 if( is.data.frame(r) ) {
   n = names(r)
   n = survey_aliases(n, def, revert=T)
   names(r) <- n
 }
 attr(r,'survey') <- survey
 attr(r, 'db.table') <- db.table
 attr(r, 'season') <- ifelse(!is.null(season), season, NA) # If current season (or not requested) attribute set to NA
 r
}

#' load options of a question from the database, identified by its question_id
#' low level function
#' @param question_id int[]
#' @param translation_id id the translation to use
#' @family survey definition functions
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

#' Get the default language used for survey
#' Uses 'survey_default_language' variable that should be defined in platform file
#' @export
survey_default_language = function() {
  # survey_default_language should be defined in platfrom
  return(.Share$survey_default_language)
}

#' load a translation for a survey for a given language
#' @param survey survey name
#' @param language code (as used in the db)
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


#' Age of participants from a Year-month birth date
#' @param ym year-month vector
#' @param time reference time
#' @export
calc.age = function(ym, time) {
  y = as.numeric(as.character(substr(ym,1,4)))
  m = as.numeric(as.character(substr(ym,6, 7)))
  year = as.numeric(format(time, format="%Y"))
  month = as.numeric(format(time, format="%m"))
  cur = (year + (month/12) )
  round(cur - y + (m / 12), 2)
}

#' @noRd
flip.names <- function(x) {
  n = names(x)
  names(n) <- as.vector(x)
  n
}

#' Keep the last survey for each participant
#'
#' Some survey could have exactly the same timestamp
#' So using this function is safer than only using timestamp
#'
#' @param data data.frame with at least (timestamp,person_id, id) columns
#' @export
keep.last.survey = function(data) {
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

#' Get liist of available season names
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

#' Get list of participant id (person_id = survey_user_id)
#' List of participants registred in weekly at least once for a given season
#' @param season season number to get
#' @param use.season.dates restrict to season's starting & ending dates.
#' @export
survey_participant_season = function(season, use.season.dates=F) {
  h = season.def(season)
  w = ''
  if(use.season.dates) {
    dates = lapply(h$dates, function(x) { if(!is.null(x)) { as.Date(x) } else { Sys.Date()} })
    w = paste0("where \"timestamp\" >='", dates$start,"' and  \"timestamp\" <='", dates$end,"'")
  }
  query = paste0("SELECT distinct s.id as person_id from ",h$weekly," p left join survey_surveyuser s on p.global_id=s.global_id ",w)
  p = dbQuery(query)
  p$person_id
}

#' List of participants registred in weekly at least once in previous season (regarding given [season])
#' @param season season year
#' @param ids list of participants to keep
#' @param use.season.dates if TRUE restrist weekly scan to the official date of each season (@see historical.tables)
#' @param seasons list of seasons to scan, relatively to the given [season], for ex -1 = only previous
#' @export
survey_participant_previous_season = function(season, ids=NULL, use.season.dates=F, seasons=NULL) {
  all.seasons = as.numeric(names(.Share$historical.tables))
  if( is.null(seasons) ) {
    seasons = all.seasons
  } else {
    seasons = season + seasons # seasons are relative index to [season]
  }
  seasons = seasons[seasons < season] # exclude given
  seasons = seasons[seasons %in% all.seasons] # keep only valid seasons values
  if(isTRUE(.Share$epiwork.tables$weekly$single.table)) {
    use.season.dates = T
  }
  previous = c()
  for(s in seasons) {
    p = survey_participant_season(s, use.season.dates = use.season.dates)
    previous = unique(c(previous, p))
  }
  if( !is.null(ids) ) {
    previous = previous[ previous %in% ids]
  }
  previous
}

#' Load participants data
#' @param active.account logical only active user account if TRUE
#' @param ids list of survey_user ids
#' @export
survey_load_participants = function(active.account=NULL, ids=NULL) {

  where = c()
  join = c()

  if( isTRUE(active.account) ) {

    join = c(join, 'left join auth_user a on a.id=s.user_id')
    where = c(where, 'a.is_active=True')

  }

  if( !is.null(ids) ) {
    where = c(where, paste0('s.id in(', paste(ids, collapse = ','),')'))
  }

  where = if(length(where)> 0) paste0(' WHERE ', paste(where, collapse = ' AND ')) else ''
  join = if(length(join) > 0) paste(join, collapse = ' ')

  r = dbQuery('select s.id as person_id, s.global_id, s.user_id, deleted, last_login, date_joined from survey_surveyuser s ', join, where)

  class(r) <- c('gn_participants', class(r))

  r
}

#' Load historical data for a set of users
#' @param ids list of participants (survey_user.id or results from survey_load_participants)
#' @param survey survey shortname
#' @param cols list of columns to load
#' @export
#' @importFrom methods is
survey_load_results_historic = function(ids, survey, cols) {
  if(!requireNamespace("dplyr")) {
    stop("dplyr required to use this function")
  }
  years = sort(names(.Share$historical.tables), decreasing = TRUE)

  if( is(ids, 'gn_participants') ) {
    ids = ids$person_id
  }

  intakes = NULL

  for(year in years) {

    cat(year, "\n")

    if( !is.null(intakes) ) {
      ids = ids[ !ids %in% intakes$person_id ]
    }

    if(length(ids) == 0) {
      break()
    }

    ii = survey_load_results(survey, cols=cols, survey.users = ids, season = year)
    ii = keep.last.survey(ii)

    if(survey == "intake") {
      ii$code_com = as.character(ii$code_com)
      ii$occup.place.com = as.character(ii$occup.place.com)
      ii$vacc.date = as.Date(as.character(ii$vacc.date))
    }
    cat(nrow(ii),"\n")
    ii$season = year
    intakes = dplyr::bind_rows(intakes, ii)
  }
  intakes
}


#' Load last participation date for intake & weekly survey for each participants
#' @param ids list of participants
#' @param years list of season to scan, all if NULL
#' @export
survey_load_participations = function(ids, years=NULL) {

  if(!requireNamespace("dplyr")) {
    stop("dplyr required to use this function")
  }

  if(is.null(years)) {
    years = sort(names(.Share$historical.tables), decreasing = TRUE)
  }

  if( is(ids, 'gn_participants') ) {
    rr = ids
  } else {
    rr = survey_load_participants(ids=ids)
  }
  gids = paste0("'", paste(rr$global_id, collapse="','"),"'")
  participations = NULL

  `%>%` <- dplyr::`%>%`

  for(year in years) {

    h = season.def(year)

    ii = dbQuery("select global_id, timestamp from ", h$intake, " where global_id in (", gids,")")

    ww = dbQuery("select global_id, timestamp from ", h$weekly, " where global_id in (", gids,")")

    ii = ii %>%
          dplyr::group_by(global_id) %>%
          dplyr::summarise(intake=n())
    ww = ww %>%
            dplyr::group_by(global_id) %>%
            dplyr::summarise(weekly=n())

        ii = merge(ii, ww, by="global_id", all=TRUE)

    if(nrow(ii) > 0) {
      ii$season = year
    }

    participations = dplyr::bind_rows(participations, ii)
  }
  participations
}


