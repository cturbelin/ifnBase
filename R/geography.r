##
# Geographic informations

#' Get geographic structure description
#' @export
geo_definition <- function(name=NULL) {
  if(!is.null(name)) {
    return(attr(.Share$geo.levels, name))
  }
  .Share$geo.levels
}

#' Create a geographic level object
#'
#' The available levels depends on the platform configuration
#'
#' @param level geographic level name (for example "nuts1", "nuts2")
#' @param hierarchy name of a level hierarchy
#' @param ... extra information to store with level information
#' @export
geo_level = function(level, hierarchy = NULL, ...) {
  if(!is.null(hierarchy)) {
    h = geo_hierarchy(hierarchy)
    if(!level %in% h) {
      stop(paste0("level '", level,"' is not in hierarchy ", hierarchy))
    }
  }
  structure(level, class="geo.level", hierarchy=hierarchy, ...)
}

#' Get geographic column name for a given level
#' @param geo geographic level name
#' @export
geo_column <- function(geo) {
  columns = geo_definition("columns")
  def = columns[[geo]]
  if( is.null(def) ) {
    stop("Unknown geograpic level, update 'geo.levels' columns attribute list to define them")
  }
  unlist(def)
}

#' Get Base geographic tables
#' It should contains all levels, describe all upper levels from the base level
#' @export
geo_base_table = function() {
  geo_definition("table")
}

#' Get base level of geographic tables (lowest level)
#'
#' @export
geo_base_level = function() {
  geo_definition("level.base")
}

#' Get information needed to make a join with geo tables
#' from base geographic level to any other level
#' @return list()
#'
#' \describe{
#'  \item{table}{name of the table associating all levels}
#'  \item{base.column}{name of the column for the lowest level}
#'  \item{join.country}{TRUE if the join should include the "country" column (european data or cross country site)}
#' }
#'
get_geo_join = function() {
  base.level = geo_base_level()
  list(
    table = geo_base_table(),
    base.level = base.level,
    base.column = geo_column(base.level),
    join.country = geo_definition("join.country")
  )
}


#' Get a hierarchy path of geographic levels
#'
#' A hierarchy path is a list of ordered geographic levels from the lowest to the uppest
#'
#' Several hierarchy could be defined to handle several kind of levels aggregation for example
#'
#' Depending of the context several kind of geographic levels can be used
#' Inside a hierarchy, all levels should be totaly nested (all sublevel should be mapped to ONE upper level)
#' @seealso geo.levels
#' @param hierarchy name of the hierarchy path to use
#'
#' @export
geo_hierarchy <- function(hierarchy=NULL) {
  hh = geo_definition("hierarchies")
  if( is.null(hierarchy) ) {
    hierarchy = geo_definition("default.path")
  }
  hh[[hierarchy]]
}

#' Get geographic table definition for a given geographic level
#' @param level geographic level name
#' @export
geo_level_table <- function(level) {
  .Share$geo.tables[[level]]
}

#' Navigate through geographic levels (get the upper level, the lower level, ...)
#' @param geo the current level
#' @param side "upper" to get the upper level, "lower" to get the lower one
#' @param hierarchy geographic levels hierarchy name
#' @export
geo_level_nav <- function(geo, side, hierarchy=NULL) {
  if( is.null(hierarchy) ) {
    if( is(geo, "geo.level") ) {
      hierarchy = attr(geo, "hierarchy")
    }
  }
  h = geo_hierarchy(hierarchy)
  side = switch(side, "upper"=1, "lower"=-1, side)
  i = match(geo, h) + side
  if(i <= 0 || i > length(h)) {
    return(NA)
  }
  return(h[i])
}

#' Load areas defined at a given geographic level
#' @param geo geographic level code (ex "nuts1")
#' @param type type of geographic level (if needed)
#' @param columns list of extra columns to fetch (if the geographic table has extra columns...)
#' @export
load_geo_zone <- function(geo, type="metro", columns=c()) {
 def = geo_level_table(geo)
 if( is.null(def) ) {
	stop("Unknown geograpic level, update 'geo.tables' list to define them")
 }
 col.geo = geo_column(geo)
 if( is.null(col.geo) ) {
   # level is not defined in geo.levels
   # So it is not handled in hierarchy
   # use column name
   cols = def$column
 } else {
   # Use column alias in geo_levels for the given geographic level (sometimes different in the table describing this level)
   cols = paste(def$column, 'as', col.geo)
 }
 if( !is.null(def$title) ) {
   cols = c(cols, paste(def$title, 'as "title"'))
 }
 if( !is.null(def$zones) ) {
    zz = def$zones
    n = names(zz)
    cols = c(cols, paste0(zz,' as "zone.', n, '"'))
 }
 cols = c(cols, columns)
 data = dbQuery(paste0('select ', paste(cols, collapse=','), ' from ', def$table))
 if( !is.null(type) ) {
   data = data[ geo_is_type(geo, type, data[, col.geo]), ]
 }
 data
}

#' Load global population for a geographic level and a census year
#'
#' Population table should follow the structure described in geography vignette
#'
#' @param geo geographic level
#' @param year year of the population (estimation of the population for the year)
#' @export
load_population <- function(geo, year) {
 dbQuery('select * from pop_', geo, ' where year=',year)
}

#' Allow to define "type" of geographic levels & area
#'
#' For example, it can be used to differenciate some code, corresponding to overseas area to mainland codes
#' The use of the function is not standardized yet.
#'
#' TODO: should be a more generic function allowing to query "feature" about
#' Not exported currently, only to be used by platform defined functions by default returns TRUE for all queried area
#'
#' @param geo geographic levelv
#' @param type feature to test for level's codes
#' @param code list of code of the level's area to test for the feature given in type
#' @return logical vector of length of code
geo_is_type = function(geo, type, code) {
  if( is.null(.Share$geo_is_type) ) {
    return( rep(TRUE, length(code)) )
  }
  .Share$geo_is_type(geo, type, code)
}

#' Load popuplation by age-group & sex
#'
#' Load popuplation data by 5-year (or multiple) age group & sex
#'
#' @param geo geographic level, if null, return by age only population
#' @param year year of the population
#' @param age.breaks used to create age categories (from 5-year categories)
#' @param version version of the file to use as data source
#' @param ... extra parameter
#' @return data.frame('all','male','female','age.cat') for the given year
#' @export
load_population_age <- function(geo, year, age.breaks=NULL, version=NULL, ...) {
  if( is.null(.Share$load_population_age) ) {
    stop("Not implemented for this platfrom, please add this function in platform definition file")
  }
  .Share$load_population_age(geo=geo, year=year, age.breaks = age.breaks, version=version, ...)
}
