# Platform definition helpers
# These functions helps to create expected structures to describe platform characteristics

#' List of not usable for data variable names in survey mapping
#'
PROTECTED_QUESTIONS = c('timestamp','channel','user')


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
#' @seealso \code{\link{survey_recode}}
#'
#' @section labels
#'
#' labels are named list of labels (a label is just a character string). It is used to manipulate list of names for various purposes : list for variables
#' for multi-valued questions (from "checkboxes"), a label provide a "name" to identify the list of all variables corresponding to one question.
#' It is also used to get the list of recoded labels for a qualitative question.
#'
#' One of common labels is "symptoms" in weekly survey definition, providing the list of the variables names for symptoms question.
#'
#' @export
platform_define_survey <- function(name, survey_id, table, mapping, labels=NULL, codes=NULL, recodes=list(), single.table = FALSE, geo.column=NULL, template=NULL, ...) {

  def = list(...)

  def$survey_id = survey_id
  def$table = table

  # Old recoding structure, codes & labels were separated.
  # recodes force label to be explicity associated with code, it is safer.
  if( !is.null(codes) ) {
    lapply(names(codes), function(name) {
        if(name %in% names(recodes)) {
          stop(paste0("'",name,"' is already defined in recodes, duplicate in codes. Remove it"))
        }

        lab = labels[[name]]
        if( is.null(lab) ) {
          stop(paste0("Missing labels with codes for '",name,"'"))
        }
        recode = codes[[name]]
        if(length(lab) != length(recode) ) {
          stop(paste0("Labels for '",name,"' have not the same length of codes "))
        }
        names(recode) <- lab
        recodes[[name]] <<- recode
    })
  } else {
    # Import recodes labels to labels
    lapply(names(recodes), function(name) {
      if(name %in% names(labels)) {
        stop(paste0(" recoding '",name,'" is already defined in labels'))
      }
      labels[[name]] <<- recodes[[name]]
    })
  }

  if( !is.null(template) ) {
    if( is.null(survey_templates[[template]]) ) {
      stop(paste0("Unknown template '", template,'"'))
    }
    def$template_name = template
    template = survey_templates[[template]]

    r = check_survey_template(template, mapping, recodes, only.errors = TRUE)
    if(length(r) > 0) {
      print(r)
      cond = simpleError("Survey checking errors, conflicts with template")
      attr(cond, "errors") <- r
      stop(cond)
    }

    # Update template mapping with new  & redefined ones
    mapping = merge.list(mapping, template$aliases)

    # Merge recodes
    rr = list()
    nn = unique(names(recodes), names(template$recodes))
    for(name in nn) {
        rr[[name]] = merge.list(recodes[[name]], template$recodes[[name]])
    }
    recodes = rr
    rm(rr)
  }

  # Check if any mapping to protected names
  n = names(mapping)
  if(any(n %in% PROTECTED_QUESTIONS)) {
    stop(paste0("Cannot define mapping to protected names : ", paste(n[n %in% PROTECTED_QUESTIONS ], collapse = ",")))
  }

  def$aliases = mapping

  def$labels = labels
  def$recodes = recodes

  if( !is.null(geo.column) ) {
    def$geo.column = geo.column
  }

  def$single.table = single.table

 .Share$epiwork.tables[[name]] <- def
}

#' Check if a survey definition is compatible with a survey template
#' @param template template name
#' @param mapping new question mapping to test
#' @param recodes new recoding to test
#' @param only.errors boolean if TRUE only report errors
#' @return list()
#' @export
check_survey_template <- function(template, mapping, recodes, only.errors=TRUE) {

  results = list()

  raise_question = function(type, value, problem, message) {
    results[[length(results) + 1]] <<- list(type=type, value=value, problem=problem, message=message, context="mapping")
    invisible()
  }

  check_list_mapping(mapping, template$aliases, raise_question, only.errors=only.errors)

  for(recode_name in names(recodes)) {
    if(is.null(template$recodes[[recode_name]])) {
      next()
    }

    raise_recode = function(type, value, problem, message) {
      results[[length(results) + 1]] <<- list(type=type, value=value, problem=problem, message=message, context="recode", name=recode_name)
      invisible()
    }

    new = recodes[[recode_name]]
    old = template$recodes[[recode_name]]
    check_list_mapping(new, old, raise_recode, only.errors=only.errors)

  }
  invisible(structure(results, class="survey_error"))
}

print.survey_error = function(errors) {
  cat("Error merging survey with template\n")
  rr = lapply(errors, function(e) {
    n = ""
    if(e$context == "recode") {
      n = paste0(" ", e$name)
    }
    paste0(" - ", e$type," in ", e$context, n, " [", e$problem, "] : ", e$message)
  })
  rr = paste(unlist(rr), collapse = "\n")
  cat(rr)
  cat("\n")
}


#' Check if two named list are compatible to merge
#' \itemize {
#'   \item only one value is mapped to a name
#'   \item new can add new name with new value, not already in old
#'   \item new cannot redefine a value with a new name (unless allow_override, not currently supported)
#' }
#'
#' The checks are transmitted to a provided function raise()
#' @param new list()
#' @param old list()
#' @param raise function(type, value, problem, message)
#' @param only.errors only raise errors
check_list_mapping = function(new, old, raise, only.errors=TRUE) {

  # Be sure we compare on character string values
  new = lapply(new, as.character)
  old = lapply(old, as.character)

  n = names(new)

  new.entries = n[ !n %in% names(old)]

  # Check if a new redefine some values already defined in old
  # TODO: allow explict overriding for some values (using attribute for ex.)
  lapply(new.entries, function(name) {
    value = new[[name]]
    if(value %in% old) {
      raise(type="error", value=value, problem="override", paste0("'",name,"' redefine '",value,"'"))
    }
    if( !only.errors ) {
      raise(type="info", value=value, problem="new", paste0("new ",value," mapped to ", name))
    }
  })

  # Check redefined of old are mapped to the same value in new
  lapply(n[ n %in% names(old) ], function(name) {
    new.value = new[[name]]
    old.value = old[[name]]
    if(new.value != old.value) {
      raise(type="error", value=old.value, problem="conflict", paste0("'",name,"' not associated with same value (",new.value,") as template (",old.value,")"))
    }
  })

  tt = table(unlist(new))
  if( !all(tt == 1) ) {
    vv =names(tt[tt > 1])
    lapply(vv, function(v) {
      n = names(new[ new == v ])
      raise(type="error", value=v, problem="duplicate", paste0("Duplicate value for entries ", paste(n, collapse = ",")))
    })
  }
 invisible()
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