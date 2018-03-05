# Platform definition helpers
# These functions helps to create expected structures to describe platform characteristics

#' Load platform file containing definitions and specific functions into .Share environment
#'
#' Launch this function to init environment
#' It can be loaded automatically by setting 'autoload.platform' option. In this case
#' options should have been provided using options(ifn=list(...)) before the package is loaded
#'
#' @export
load_platform = function() {

  path = get_option('platform.path')
  platform = get_option('platform')

  if(is.null(path) ) {
    stop("Platform path should be defined with share.option()")
  }

  if(is.null(platform) ) {
    stop("platform id should be defined with share.option()")
  }

  file = get_r_file(paste0(path, platform), should.exists = TRUE)

  sys.source(file, envir=.Share)
}

#' Describe a survey of a platform
#'
#' Describe structure of a survey and the mapping from DB names &
#' coding to R names & factor levels
#'
#' @param name unique name of the survey
#' @param survey_id id of the survey in the database
#' @param table name of the default table storing response of the survey
#' @param mapping Mapping from DB names (in value) and R names for each survey variables.
#' @param labels list of labels
#' @param codes list of codes *deprecated*
#' @param recodes list of recoding
#' @param single.table boolean, TRUE if survey uses single table model
#' @param geo.column name of the variable (R names)
#' @param ... extra parameters to set for the survey
#'
#' @section mapping
#'
#' The mapping allows to automatically transform db names to R names (more meaningfull and error-proof) each times data are loaded using `survey_load_results()`
#'
#' @section recoding
#'
#' for each variable, it is possible to give a list(label1=code1, label2=code2) where `code1`, `code2` will be recoded to the corresponding label
#' The idea is to give a human meaningful label to each numeric code used to store response value for questions based on an option list.
#'
#' It is a good pratice to use variable style names (character + dot), without space and in english. Those labels could be translated
#' later to another language in the analysis output (graphs, tables & so on). This aimed to produced shareable and language agnostic
#' programs.
#' For a given questions, all options should also start with the same prefix to allow quick selection of theses labels based on patterns
#'
#' For example:
#'  main.activity = c('activity.fulltime'='0','activity.partial'='1','activity.self'='2', 'activity.student'='3','activity.home'='4','activity.unemployed'='5','activity.sick'='6','activity.retired'='7', 'activity.other'=8),
#'
#' Will describe recoding for the question named 'main.activity' (using attributed name, never the DB name).
#'
#' @seealso survey_recode
#'
#' @section labels
#' labels are named list of labels (a label is just a character string). It is used to identify list of recoding labels
#' @export
platform_define_survey <- function(name, survey_id, table, mapping, labels=NULL, codes=NULL, recodes=NULL, single.table = FALSE, geo.column=NULL, ...) {

  def = list(...)

  def$survey_id=survey_id
  def$table = table
  def$aliases = mapping

  if( !is.null(recodes) ) {
    nn = names(mapping)
    for(i in seq_along(recodes)) {
      n = names(recodes[i])
      if(! n %in% nn) {
         stop(paste0("recoding to a variable not declared in mapping '",n,'"'))
      }
      if(n %in% names(labels)) {
         stop(paste0("recoding '",n,'" is already declared in mapping'))
      }
      values = recodes[[i]]
      labels[[n]] <- names(values)
      codes[[n]] <- as.vector(values)
    }
  }

  def$labels = labels
  def$codes = codes

  if( !is.null(geo.column) ) {
    def$geo.column = geo.column
  }

  def$single.table = single.table

 .Share$epiwork.tables[[name]] <- def
}

describe_survey_recoding = function(survey, name, values) {

}

#' Create structure defining an geographic table system
#' @param levels list of levels with level name in names and column name in value, if it not a named vector, the use value as level
#' @param level.base name of the base level (if null, first level is used)
#' @param table name of the table containing all the levels (it can be the lowest level table as it should contains reference to all upper levels)
#' @param columns list of column name for each level in the form list([name]=[column_name])
#' @param hierarchies list of hierarchies list( hierarchy1=c(levels order), hierarchy2=(levels order), ...)
#' @param default.hierarchy name of the default hierarchy to be used to get upper or lower column
#' @export
platform_geographic_levels = function(levels,  level.base = NULL, table = 'geo_levels', columns = NULL, hierarchies=NULL, default.hierarchy='default') {

  lev = names(levels)
  if( is.null(lev) ) {
    lev = levels
  }

  if( is.null(level.base) ) {
    level.base = lev[1]
  } else {
    if(!level.base %in% lev) {
      stop("Base level is not in levels list")
    }
  }

  if( is.null(columns) ) {
    columns = paste0('code_', lev)
    names(columns) = lev
  } else {
    nn = names(columns)
    if( !all( nn %in% lev) ) {
      stop(paste("Some levels are not mapped to a column name : ", paste(nn[!nn %in% lev]), collapse = ','))
    }
  }

  if( is.null(hierarchies) ) {
    hierarchies = list( lev )
    names(hierarchies) <- default.hierarchy
  } else {
    Map(function(name, hh) {
      if(!all(hh %in% lev) ) {
        stop(paste(" hierachy ",name," some levels are not in defined levels ", paste(hh[!hh %in% lev], collapse = ',')))
      }
    }, names(hierarchies), hierarchies)
  }

  structure(
    lev,
    level.base = level.base,
    columns = columns,
    table = table,
    hierarchies = hierarchies,
    default.path = default.hierarchy,
    class="geo_levels"
  )
}

#' Create geographic tables description
#'
#' @param def either a geo.levels structure or a list of table description for each level (name of the level as the name of each entry)
#' @param default.title default column name for title
#' @return list()
#'
#' @details
#' \describe{
#'   \item{table}{name of the table to use}
#'   \item{column}{name of the column in the table containing the area id of the level}
#'   \item{title}{name of the column containing label of the area}
#' }
#'
#' @export
#' @importFrom methods is
platform_geographic_tables = function(def, default.title = "title") {
  if( is(def, "geo_levels") ) {
    columns = attr(def, "columns")
    tables = lapply(def, function(level) {
      list(
        table = paste0("geo_", level),
        title = default.title,
        column = columns[level]
      )
    })
  } else {
    tables = def
  }
  structure(tables, class="geo_tables")
}


#' Define historical data
#' For each season you can describe how data are organized
#'
#' @param season name of the season. By convention it is the year number of the starting year (2011 for 2011-2012,...)
#' @param dates list(start=, end=) starting & ending of each season, YYYY-MM-DD format
#' @param ... tables names for each survey containing data of the season in case of by-season storage model.
#'
#' @section data storage model
#' InfluezaNet data for a survey can be stored using 2 ways :
#'  - A single table contains data for all the seasons (if the survey didnt changed a lot)
#'  - A new table is created for each season for a given survey
#'
#' @examples
#' # Single table model
#' platfrom_season_history('2011', dates=list(start="2011-11-15", end="2012-04-15"))
#' platfrom_season_history('2012', dates=list(start="2012-11-15", end="2013-04-20"))
#'
#' # Multiple table model
#' platfrom_season_history('2011', dates=list(start="2011-11-15", end="2012-04-15"), weekly="pollster_results_weekly_2011")
#' platfrom_season_history('2012', dates=list(start="2012-11-15", end="2013-04-20"), weekly="pollster_results_weekly_2012")
#'
#' @export
platform_season_history <- function(season, dates, ...) {
  if( is.null(.Share$historical.tables) ) {
    .Share$historical.tables = list()
  }
  def = list(...)
  def$dates = dates
  .Share$historical.tables[[as.character(season)]] <- def
}

#' Define some platform options
#' @param ... list of options to set
#'
#' \describe{
#'   \item{first.season.censored}{left censor first season participants for some countries}
#'   \item{health.status}{structure of the health.status table in case of single table model for weekly}
#' }
#'
platform_options = function(...) {

  oo = list(...)

  if(!is.null(oo$first.season.censored)) {
    if( !is.logical(oo$first.season.censored) ) {
      stop("'first.season.censored' should be logical value")
    }
    .Share$first.season.censored = oo$first.season.censored[1]
  }

  # Health status table option (in case of single table model)
  if(!is.null(oo$health.status)) {
    # list(default="name of healt status table", id="name of column containing weekly id")
    .Share$health.status = oo$health.status
  }

}

#' Get the platform env
#'
#' Platform environment holds platform variables & definition structure.
#' @export
platform_env <- function() {
  .Share
}