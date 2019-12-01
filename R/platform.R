# Platform definition helpers
# These functions helps to create expected structures to describe platform characteristics

#' List of not usable for data variable names in survey mapping
#'
PROTECTED_QUESTIONS = c('timestamp','channel','user')

#' Load platform file containing definitions and specific functions into .Share environment
#'
#' Launch this function to init the package environment
#' It can be loaded automatically by setting 'autoload.platform' option. In this case
#' options should have been provided using options(ifn=list(...)) before the package is loaded see \code{\link{share.option}()}
#'
#' @family platform
#' @export
load_platform = function() {

  path = get_option('platform.path')
  platform = get_option('platform')

  if(is.null(path) ) {
    rlang::abort("Platform path should be defined with share.option()")
  }

  if(is.null(platform) ) {
    rlang::abort("platform id should be defined with share.option()")
  }

  file = get_r_file(paste0(ending_slash(path), platform), should.exists = TRUE)

  sys.source(file, envir=.Share)

  validate_platform()
}

#' Describe a survey of a platform and register it
#'
#' Describe configuration of a survey and the mapping from DB names &
#' coding to R names & factor levels.
#' A call to this function is needed to register a survey in the package before to use other survey functions.
#' Usually it is done in the platform definition file, which is loaded by \code{\link{load_platform}()} function
#' @seealso [survey_definition()]
#' @family platform
#' @param name unique name of the survey
#' @param survey_id id of the survey in the database
#' @param table name of the default table storing response of the survey
#' @param mapping Mapping from DB names (in value) and R names for each survey variables.
#' @param labels list of labels
#' @param codes list of codes *deprecated*
#' @param recodes list of recoding
#' @param single.table boolean, TRUE if survey uses single table model
#' @param geo.column name of the variable (R names)
#' @param template name of the template to override
#' @param ... extra parameters to set for the survey
#'
#' @return invisibly return the survey_definition object
#'
#' @section Mapping:
#'
#' The mapping allows to automatically transform db names to R names (more meaningfull and error-proof) each times data are loaded using `survey_load_results()`
#'
#' @section Recoding:
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
#' @section Labels:
#'
#' labels are named list of labels (a label is just a character string). It is used to manipulate list of names for various purposes : list for variables
#' for multi-valued questions (from "checkboxes"), a label provide a "name" to identify the list of all variables corresponding to one question.
#' It is also used to get the list of recoded labels for a qualitative question.
#'
#' One of common labels is "symptoms" in weekly survey definition, providing the list of the variables names for symptoms question.
#'
#' @export
platform_define_survey <- function(name, survey_id=NULL, table=NULL, mapping=list(), labels=NULL, codes=NULL, recodes=list(), single.table = FALSE, geo.column=NULL, template=NULL, ...) {

  def = structure(list(...), class="survey_definition")

  def$survey_id = survey_id
  def$table = table

  r = create_survey_definition(mapping=mapping, labels=labels, codes=codes, recodes=recodes, template=template, only.errors = TRUE)

  errors = Filter(function(x) x$type == "error", r$checks)
  if( length(errors) > 0 ) {
    rlang::abort("Survey error importing templates", survey_errors=r$checks)
   }

  if( !is.null(template) ) {
    def$template_name = template
  }

  def$aliases = r$mapping

  def$labels = r$labels
  def$recodes = r$recodes
  def$checks = r$checks

  if( !is.null(geo.column) ) {
    def$geo.column = geo.column
  }

  def$single.table = single.table
  def$name = name

 .Share$epiwork.tables[[name]] <- def
 invisible(def)
}

#' Create a survey definition
#'
#' Create and check survey structure and import from template if provided.
#' This function is mainly used internally by [platform_define_survey()]. Unless you want to test definition you should not need this function.
#'
#' @family platform
#'
#' @param mapping list() variable (name) to DB column (value) mapping
#' @param labels list() named list of labels
#' @param codes list() of codes **deprecated**
#' @param recodes list() list of recoding
#' @param template character
#' @param only.errors if TRUE only check for errors
#' @return list(mapping, labels, recodes, checks)
#' @export
create_survey_definition <- function( mapping, labels=NULL, codes=NULL, recodes=list(), template=NULL, only.errors=TRUE) {
  # Old recoding structure, codes & labels were separated.
  # recodes force label to be explicity associated with code, it is safer.
  if( !is.null(codes) ) {
    lapply(names(codes), function(name) {
      if(name %in% names(recodes)) {
        rlang::abort(paste0("'",name,"' is already defined in recodes, duplicate in codes. Remove it"))
      }

      lab = labels[[name]]
      if( is.null(lab) ) {
        rlang::abort(paste0("Missing labels with codes for '",name,"'"))
      }
      recode = codes[[name]]
      if(length(lab) != length(recode) ) {
        rlang::abort(paste0("Labels for '",name,"' have not the same length of codes "))
      }
      names(recode) <- lab
      recodes[[name]] <<- recode
    })
  } else {
    # Import recodes labels to labels
    lapply(names(recodes), function(name) {
      if(name %in% names(labels)) {
        rlang::abort(paste0(" recoding '",name,'" is already defined in labels'))
      }
      labels[[name]] <<- recodes[[name]]
    })
  }

  checks = NULL

  # Resolve recode_alias by finding the recoding in another entry either in the mapping list or in the template if provided
  resolve_alias = function(rr, template=NULL) {
    lapply(rr, function(r) {
      if(is(r, "recode_alias")) {
        map = rr[[r]]
        if(is.null(map) & !is.null(template)) {
          map = template[[r]]
        }
        if(is.null(map)) {
          rlang::abort(paste0("Unable to find recode name for '", r,'"', recodes=rr, template=template))
        }
        r = map
      }
      r
    })
  }


  if( !is.null(template) ) {
    if( is.null(survey_templates[[template]]) ) {
      rlang::abort(paste0("Unknown template '", template,'"'))
    }

    template = survey_templates[[template]]

    template$recodes = resolve_alias(template$recodes)

    checks = check_survey_template(template, mapping, recodes, only.errors = only.errors)

    # Update template mapping with new  & redefined ones
    mapping = merge_by_value(mapping, template$aliases)

    # Merge recodes
    rr = list()
    nn = unique(c(names(recodes), names(template$recodes)))

    recodes = resolve_alias(recodes, template)

    for(name in nn) {
      new_recodes = merge_by_value(recodes[[name]], template$recodes[[name]])
      if( !check_unique(new_recodes) ) {
        rlang::abort(paste0("Values are not unique for recode ", name), values=new_recodes)
      }
      rr[[name]] = structure(new_recodes, class="survey_recode")
    }
    recodes = rr

    # Merge labels
    labels = merge_list(labels, template$labels)

  } else {
    recodes = resolve_alias(recodes)
  }

  # Check if any mapping to protected names
  n = names(mapping)
  if(any(n %in% PROTECTED_QUESTIONS)) {
    rlang::abort(paste0("Cannot define mapping to protected names : ", paste(n[n %in% PROTECTED_QUESTIONS ], collapse = ",")))
  }

  list(
    mapping = structure(mapping, class="survey_mapping"),
    labels = structure(labels, class="survey_labels"),
    recodes = recodes,
    checks = checks
  )

}

#' Check if list values are unique
#' @keywords internal
#' @param x vector or list of values to check for unicity
check_unique = function(x) {
  if(is.list(x)) {
    x = unlist(x)
  }
  all(table(x) == 1)
}

#' Merge two named lists by their values (survey recoding)
#'
#' Internal function to be used for survey recoding definition.
#' Only keep in old values that are not already in new
#' @keywords internal
#' @param new vector or list of values
#' @param old vector or list of values
merge_by_value = function(new, old) {
  if( is.list(new) ) {
    new = unlist(new)
  }
  if( is.list(old) ) {
    old = unlist(old)
  }
  old = old[!old %in% new]
  new = c(new, old)
  new = as.list(new)
  if(length(old) > 0) {
    attr(new, "inherited") = names(old)
  }
  new
}

#' Check if a survey definition is compatible with a survey template
#' @param template template name
#' @param mapping new question mapping to test
#' @param recodes new recoding to test
#' @param only.errors boolean if TRUE only report errors
#' @return list()
#' @family [platform_define_survey()]
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

#' Print checks results
#'
#' @param x list() errors generated by \code{\link{create_survey_definition}}
#' @param ... extra parameters (for print interface compatibility)
#' @export
print.survey_error = function(x, ...) {
  if( length(x)== 0 ) {
    cat("No error")
    return()
  }
  cat("Error merging survey with template\n")
  rr = lapply(x, function(e) {
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

#' print survey recoding
#' @param x list() recode mapping
#' @param ... extra parameters (print interface compatibility)
#' @export
print.survey_recode <-function(x, ...) {
  inherited = attr(x, "inherited")
  cat("Variable recodings (label = db value):\n")
  Map(function(label, value) {
    cat(" - ", sQuote(label),'=',sQuote(value), if(label %in% inherited) " (inherited)", "\n")
  }, names(x), as.vector(x))
}

#' print survey variable mapping
#' @param x list() variable mapping
#' @param ... extra parameters (print interface compatibility)
#' @export
print.survey_mapping <-function(x, ...) {
  inherited = attr(x, "inherited")
  cat("Variable mapping (variable = db name):\n")
  Map(function(label, value) {
    cat(" - ", sQuote(label),'=',sQuote(value), if(label %in% inherited) " (inherited)", "\n")
  }, names(x), as.vector(x))
}

#' print survey labels mapping
#' @param x list() variable mapping
#' @param ... extra parameters (print interface compatibility)
#' @export
print.survey_labels <-function(x, ...) {
  inherited = attr(x, "inherited")
  cat("Labels :\n")
  Map(function(label, value) {
    cat(" - ", sQuote(label),'=', paste(sQuote(value), collapse=","), if(label %in% inherited) " (inherited)", "\n")
  }, names(x), as.vector(x))
}


#' print survey definition
#' @param x list() recode mapping
#' @param ... extra parameters (print interface compatibility)
#' @export
print.survey_definition <-function(x, ...) {
  cat("Survey", sQuote(x$name))
  if(!is.null(x$template_name)) {
    cat(" inherits survey template", sQuote(x$template_name))
  }
  if(!is.null(x$table)) {
    cat(" table:", sQuote(x$table))
    if(x$single.table) {
      cat(" using single table model")
    }
  }
  if(!is.null(x$survey_id)) {
    cat(" Survey db id:", sQuote(x$survey_id))
  }
  cat("\n")
  if(!is.null(x$aliases)) {
    print(x$aliases)
  }
  if(!is.null(x$labels)) {
    print(x$labels)
  }
  if(!is.null(x$recodes)) {
    cat("Recodings:\n")
    print(x$recodes)
  }
  if(!is.null(x$checks)) {
    cat("Checks\n")
    print(x$checks)
  }
  nn = names(x)
  nn = nn[! nn %in% c('aliases','labels','name','template_name','table','single.table','survey_id','recodes','checks')]
  if(length(nn) > 0) {
    print(x[nn])
  }
}


#' Check if two named list are compatible to merge
#'
#' To be compatible with template, mapping should follow these rules:
#' \itemize{
#'   \item{only one value is mapped to a name}
#'   \item{new can add new name with new value, not already in old}
#'   \item{new cannot redefine a value with a new name (unless allow_override, not currently supported)}
#' }
#'
#' Overriding is allowed wraping the value with \code{\link{override}} function
#'
#' @examples
#'\dontrun{
#' new = list("var1"="Q1", "var2"="Q2", "var3"="Q3", "var4"="Q1", "var5"="Q4")
#' old = list("var0"="Q0", "var1"="Q1", "var2"="Q4")
#' check_list_mapping(new, old, raise=raise)
#' # Returs list of errors
#'
#' new = list("var1"="Q1", "var3"=override("Q2"))
#' old = list("var2"="Q2")
#' check_list_mapping(new, old, raise=raise) # Only throw warning
#' }
#' # The checks are transmitted to a provided function \code{\link{raise()}}
#' @param new list()
#' @param old list()
#' @param raise function(type, value, problem, message)
#' @param only.errors only raise errors
check_list_mapping = function(new, old, raise, only.errors=TRUE) {

  # Be sure we compare on character string values, keep values attributes
  convert <- function(x) {
    a = attributes(x)
    x = as.character(x)
    attributes(x) <- a
    x
  }

  new = lapply(new, convert)
  old = lapply(old, convert)

  n = names(new)

  new.entries = n[ !n %in% names(old)]

  # Check if a new redefine some values already defined in old
  lapply(new.entries, function(name) {
    value = new[[name]]
    if(value %in% old) {
      # Allow explicit override for some values if it is explicit, althought not recommanded
      override = attr(value, "allow_override")
      type = ifelse(isTRUE(override), "warn", "error")
      raise(type=type, value=value, problem="override", paste0("'",name,"' redefine '",value,"'"))
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
#'
#' @param levels list of levels with level name in names and column name in value, if it not a named vector, the use value as level
#' @param level.base name of the base level (if null, first level is used)
#' @param table name of the table containing all the levels (it can be the lowest level table as it should contains reference to all upper levels)
#' @param columns list of column name for each level in the form list([name]=[column_name])
#' @param hierarchies list of hierarchies list( hierarchy1=c(levels order), hierarchy2=(levels order), ...)
#' @param default.hierarchy name of the default hierarchy to be used to get upper or lower column
#' @param country handle country column
#' @param define if TRUE set the geo.levels during the call, set to FALSE if you dont want to change configuration
#' @family platfom
#' @export
platform_geographic_levels = function(levels,  level.base = NULL, table = 'geo_levels', columns = NULL, hierarchies=NULL, default.hierarchy='default', country=FALSE, define=TRUE) {

  lev = names(levels)
  if( is.null(lev) ) {
    lev = levels
  }

  if( is.null(level.base) ) {
    level.base = lev[1]
  } else {
    if(!level.base %in% lev) {
      rlang::abort("Base level is not in levels list")
    }
  }

  if( is.null(columns) ) {
    columns = paste0('code_', lev)
    names(columns) = lev
  } else {
    nn = names(columns)
    if( !all( nn %in% lev) ) {
      rlang::abort(paste("Some levels are not mapped to a column name : ", paste(nn[!nn %in% lev]), collapse = ','))
    }
  }

  if( is.null(hierarchies) ) {
    hierarchies = list( lev )
    names(hierarchies) <- default.hierarchy
  } else {
    Map(function(name, hh) {
      if(!all(hh %in% lev) ) {
        rlang::abort(paste(" hierachy ",name," some levels are not in defined levels ", paste(hh[!hh %in% lev], collapse = ',')))
      }
    }, names(hierarchies), hierarchies)
  }

  geo = structure(
    lev,
    level.base = level.base,
    columns = columns,
    table = table,
    hierarchies = hierarchies,
    default.path = default.hierarchy,
    join.country = country,
    class="geo_levels"
  )

  if(define) {
    .Share$geo.levels = geo
  }
  invisible(geo)
}

#' Create geographic tables description
#'
#' @param def list of table description for each level (name of the level as the name of each entry), if NULL a default table structure is created
#' @param default.title default column name for title
#' @param define if TRUE set the table configuration during the call
#' @return list()
#'
#' @details
#' \describe{
#'   \item{table}{name of the table to use}
#'   \item{column}{name of the column in the table containing the area id of the level}
#'   \item{title}{name of the column containing label of the area}
#' }
#' @family platfom
#'
#' @export
platform_geographic_tables = function(def=NULL, default.title = "title", define=TRUE) {
  if( is.null(def) ) {
    def = geo_definition()
    tables = lapply(def, function(level) {
      list(
        table = paste0("geo_", level),
        title = default.title,
        column = geo_column(level)
      )
    })
  } else {
    tables = def
    nn = names(def)
    levels = geo_definition()
    if( !all(levels %in% nn) ) {
      m = levels[!levels %in% nn]
      rlang::abort(paste("Some levels are not described in a geographic table", paste(m, collapse = ",")))
    }
  }
  geo = structure(tables, class="geo_tables")
  if(define) {
    .Share$geo.tables = geo
  }
  invisible(geo)
}

#' Define historical data
#'
#' For each season you can describe how data are organized
#'
#' @param season name of the season. By convention it is the year number of the starting year (2011 for 2011-2012,...)
#' @param dates list(start=, end=) starting & ending of each season, YYYY-MM-DD format
#' @param ... tables names for each survey containing data of the season in case of by-season storage model.
#'
#' @section Data storage model:
#' InfluezaNet data for a survey can be stored using 2 ways :
#'  - A single table contains data for all the seasons (if the survey didnt changed a lot)
#'  - A new table is created for each season for a given survey
#'
#' @examples
#' \dontrun{
#' # Single table model
#' platfrom_season_history('2011', dates=list(start="2011-11-15", end="2012-04-15"))
#' platfrom_season_history('2012', dates=list(start="2012-11-15", end="2013-04-20"))
#'
#' # Multiple table model
#' platfrom_season_history('2011',
#'      dates=list(start="2011-11-15", end="2012-04-15"),
#'      weekly="pollster_results_weekly_2011"
#' )
#' platfrom_season_history('2012',
#'      dates=list(start="2012-11-15", end="2013-04-20"),
#'      weekly="pollster_results_weekly_2012"
#')
#' }
#' @family platfom
#' @export
platform_season_history <- function(season, dates, ...) {

  season = parse_season(season)

  if( is.null(.Share$historical.tables) ) {
    .Share$historical.tables = list()
  }

  def = list(...)
  def$dates = dates

  check_date = function(v, after=NULL) {
    if(is.na(v)) {
      return()
    }
    d = as.Date(v)
    if(is.na(d)) {
      rlang::abort(paste0(deparse(substitute(v)), "Unable to parse date '", v,"'"))
    }
    if(!is.null(after) ) {
      after = as.Date(after)
      if(d < after) {
        rlang::abort(paste(deparse(substitute(v)), "should be after", after))
      }
    }
  }

  check_date(dates$start)
  check_date(dates$end, after=dates$start)

  def = structure(def, class="season_definition")

  .Share$historical.tables[[as.character(season)]] <- def
}

#' Define some platform options
#' @param ... list of options to set
#'
#' \describe{
#'   \item{first.season.censored}{left censor first season participants for some countries}
#'   \item{health.status}{structure of the health.status table in case of single table model for weekly}
#'   \item{debug.query}{debug SQL queries}
#' }
#' @family platfom
#' @export
platform_options = function(...) {

  oo = list(...)

  # Import true/false single value
  import_flag = function(name) {
    if(hasName(oo, name) ) {
      v = oo[[name]]
      if(!is.logical(v)) {
        rlang::abort(paste0(sQuote(name)," should be logical value"))
      }
      if(length(v) > 1) {
        rlang::abort(paste0(sQuote(name)," only expect one value"))
      }
      .Share[[name]] = v
    }
  }

  flags = c("first.season.censored", "debug.query", "use.country")
  for(name in flags) {
    import_flag(name)
  }

  # Health status table option (in case of single table model)
  if(!is.null(oo$health.status)) {
    # list(default="name of healt status table", id="name of column containing weekly id")
    .Share$health.status = oo$health.status
  }

  if(!is.null(oo$complete.intake) ) {
    .Share$complete.intake = oo$complete.intake
  }

}

#' Get the platform env
#'
#' @param name name of the value to get in the platform envirnoment
#'
#' @return if name is NULL, returns the full environment, if not get the named element in the environment
#'
#' Platform environment holds platform variables & definition structure.
#' @family platfom
#' @export
platform_env <- function(name=NULL) {
  if(is.null(name)) {
    .Share
  } else {
    .Share[[name]]
  }
}

#' Post loading function to validate platform info
#' @keywords internal
validate_platform =function() {

  if(isTRUE(.Share$first.season.censored)) {
    if(is.null(.Share$get_first_season_country)) {
      rlang::abort("get_first_season_country() should be defined with the platform option `first.season.censored`. Please define this function in first.season.censored ")
    }
  }

}

#' Check if the platform is allowed to use "country" value
#'
#' raise an error if the platform is not configured to use country value
#'
#' @return TRUE if country is definied and platform can use country
can_use_country = function(country) {
  if(!is.null(country)) {
    if( !isTRUE(platform_env("use.country")) ) {
      rlang::abort("Cannot use `country`for this platform not configured")
    }
    return(TRUE)
  }
  return(FALSE)
}
