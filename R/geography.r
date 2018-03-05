##
# Geographic informations

#' Create a geographic level object
#'
#' The available levels depends on the platform configuration
#'
#' @param geo.name geographic level name (for example "nuts1", "nuts2")
#' @param hierarchy name of a level hierarchy
#' @param ... extra information to store with level information
#' @export
geo_level = function(geo.name, hierarchy = NULL, ...) {
  structure(geo.name, class="geo.level", hierarchy=hierarchy, ...)
}

#' Get geographic column name for a given level
#' @param geo geographic level name
#' @export
get_geo_column <- function(geo) {
  def = attr(.Share$geo.levels, "columns")[[geo]]
  if( is.null(def) ) {
    stop("Unknown geograpic level, update 'geo.levels' columns attribute list to define them")
  }
  def
}

#' Get a hierarchy path of geographic levels
#'
#' Depending of the context several kind of geographic levels can be used
#' Inside a hierarchy, all levels should be totaly nested (all sublevel should be mapped to ONE upper level)
#' @seealso geo.levels
#' @param hierarchy name of the hierarchy path to use
#'
#' @export
get_geo_hierarchy <- function(hierarchy=NULL) {
  hh = attr(.Share$geo.levels, "hierarchies")
  if( is.null(hierarchy) ) {
    hierarchy = attr(.Share$geo.levels, "default.path")
  }
  hh[[hierarchy]]
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
  h = get_geo_hierarchy(hierarchy)
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
 def = .Share$geo.tables[[geo]]
 if( is.null(def) ) {
	stop("Unknown geograpic level, update 'geo.tables' list to define them")
 }
 col.geo = get_geo_column(geo)
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
 column = get_geo_column(geo)
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
