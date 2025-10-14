##
# Geographic informations
# Set of geographic functions to navigate

#' Get geographic structure description
#' @param name geographic level name
#'
#' @family geography
#'
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
#' @family geography
#' @export
geo_level = function(level, hierarchy = NULL, ...) {
  if(!is.null(hierarchy)) {
    h = geo_hierarchy(hierarchy)
    if(!level %in% h) {
      rlang::abort(paste0("level '", level,"' is not in hierarchy ", hierarchy), params=list(level=level, hierarchy=hierarchy))
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
    rlang::abort(paste0("Unknown geograpic level '", geo,"'"))
  }
  unlist(def)
}

#' Get Base geographic tables
#' It should contains all levels, describe all upper levels from the base level
#' @family geography
#' @export
geo_base_table = function() {
  geo_definition("table")
}

#' Get base level of geographic tables (lowest level)
#' @family geography
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
#' @family geography
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
#' @seealso [geo_levels()]
#' @param hierarchy name of the hierarchy path to use
#' @family geography
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
#' @family geography
#' @export
geo_level_table <- function(level) {
  .Share$geo.tables[[level]]
}

#' Navigate through geographic levels (get the upper level, the lower level, ...)
#' @param geo the current level
#' @param side "upper" to get the upper level, "lower" to get the lower one
#' @param hierarchy geographic levels hierarchy name
#' @family geography
#' @export
geo_level_nav <- function(geo, side, hierarchy=NULL) {
  if( is.null(hierarchy) ) {
    if( is(geo, "geo.level") ) {
      hierarchy = attr(geo, "hierarchy")
    }
  }
  h = geo_hierarchy(hierarchy)
  if(is.character(side)) {
    if(!side %in% c("upper","lower")) {
      rlang::abort(paste0("unknown side value '", side,"'"))
    }
    if(side == "upper") {
      side = 1L
    }
    if(side == "lower") {
      side = -1L
    }
  } else {
    side = as.integer(side)
  }
  if(!is.integer(side)) {
    rlang::abort("wrong side value, should be integer")
  }
  i = match(geo, h)
  if( is.na(i) ) {
    rlang::abort(paste0("Unknown level '",geo,"'"))
  }
  i = i + side
  if(i <= 0 || i > length(h)) {
    return(NA)
  }
  return(h[i])
}

#' Normalize with geographic codes for a level after it has been loaded by the database.
#'
#' Some database driver coerce column data to another type (char to int) which is can cause problems
#' with some code with mixed alphanumeric and numeric codes (sometimes to int, sometimes to char), cast
#' to int drop zerofilled codes making the code unuseable.
#'
#' This function try to find a normalizer function associated with provided levels. Normalizer functions are
#' defined with the geographic levels using \code{\link{platform_geographic_levels}()}
#'
#'
#' @param data data to normalize
#' @param ... extra parameters for specific methods
#' @export
geo_normalize = function(data, ...) {
  UseMethod("geo_normalize")
}

#' Check if level exists
#' @param level character vector of level names
#' @param error if TRUE raise an error if level doesnt exists
geo_check_level = function(level, error=TRUE) {
  levels = geo_definition()
  e = !level %in% levels
  if(any(e)) {
    if(error) {
      rlang::abort(paste("Levels are unknown:", paste(sQuote(level[e]), collapse=',')))
    }
    return(FALSE)
  }
  return(TRUE)
}

#' @inherit geo_normalize
#' @param level character corresponding level name for the data
#' @export
geo_normalize.default <- function(data, level, ...) {
  normalizers = geo_definition("normalizers")
  geo_check_level(level)
  if(length(normalizers) == 0) {
    # Nothing to do
    return(data)
  }
  if(hasName(normalizers, level)) {
    norm = normalizers[[level]]
    return(norm(data))
  }
  data
}

#' @inherit geo_normalize
#' @param columns columns to normalize, named vector with level name as name for each column if levels is null
#' @param levels corresponding level for each column. if NULL will use names of `columns` parameter
#' @export
geo_normalize.data.frame = function(data, columns=NULL, levels=NULL, ...) {
  normalizers = geo_definition("normalizers")
  if(length(normalizers) == 0) {
    # Nothing to do
    return(data)
  }
  if(is.null(levels)) {
    if(is.null(columns)) {
      rlang::abort("Either columns or levels should be provided")
    }
    levels = names(columns)
  }
  if(is.null(columns)) {
    columns = sapply(levels, geo_column)
  }
  if(is.list(columns)) {
    columns = unlist(columns)
  }
  attrs = list()
  for(i in seq_along(columns)) {
    column = columns[i]
    level = levels[i]
    if(!hasName(normalizers, level)) {
      next()
    }
    if(!hasName(data, column)) {
      warning(paste("no column", column, " in data"))
      next()
    }
    norm = normalizers[[level]]
    data[[column]] = norm(data[[column]])
    attrs[[level]] = column
  }
  attr(data, "geo_normalized") <- attrs
  data
}


#' Load areas defined at a given geographic level
#' @param geo geographic level code (ex "nuts1")
#' @param type type of geographic level (if needed)
#' @param columns list of extra columns to fetch (if the geographic table has extra columns...)
#' @family geography
#' @export
load_geo_zone <- function(geo, type="metro", columns=c()) {
 def = geo_level_table(geo)
 if( is.null(def) ) {
	stop("Unknown geograpic level, use platform_geographic_tables to define them")
 }
 if(is.null(def[['table']])) {
   rlang::abort(paste0("level", sQuote(geo),' is not associated with a table'))
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
 # Columns to normalize
 to.normalize = col.geo
 names(to.normalize) <- geo
 if( !is.null(def$zones) ) {
    zz = def$zones
    n = names(zz)
    cols = c(cols, paste0(zz,' as "zone.', n, '"'))
    to.normalize = c(to.normalize, zz)
 }
 cols = c(cols, columns)
 data = dbQuery(paste0('select ', paste(cols, collapse=','), ' from ', def$table))
 if( !is.null(type) ) {
   data = data[ geo_is_type(geo, type, data[, col.geo]), ]
 }
 data = geo_normalize(data, columns=to.normalize)
 data
}

#' Load global population for a geographic level and a census year
#'
#' Population table should follow the structure described in geography vignette
#'
#' @param geo geographic level
#' @param year year of the population (estimation of the population for the year)
#' @param country country (only on country enabled platform)
#' @family geography
#' @export
load_population <- function(geo, year, country=NULL) {
  loader = .Share$population.loader
  if( !is.null(.Share$load_population) ) {
    loader = .Share$load_population
  } else {
    if( is.character(loader) ) {
      loader = switch(loader,
        db = load_population.db,
        age= load_population.age,
        rlang::abort(paste0("unknown population loader type ", sQuote(loader)))
      )
    }
  }

  if(!is.function(loader)) {
    rlang::abort("Paste population loader is not a function", loader=loader)
  }
  data = loader(geo, year, country)
  if(nrow(data) > 0) {
    data = geo_normalize(data, levels=geo)
  }
  data
}

#' Load population database implementation
#' @rdname load_population
load_population.db = function(geo, year, country) {
  def = geo_level_table(geo)
  if( is.null(def) ) {
    rlang::abort(paste0("Unknwon geographic level ", sQuote(geo)))
  }
  if( is.null(def[['population_table']] )) {
    table = paste0("pop_", geo)
  } else {
    table = def$population_table
  }
  dbQuery('select * from ', table, ' where "year"=', year)
}

#' Load population using by sex & age-group population
#' @family geography
#' @rdname load_population
load_population.age = function(geo, year, country) {
  pop = load_population_age(geo, year, country=country)
  col_geo = geo_column(geo)
  groups = c('year', col_geo)
  if(!is.null(country)) {
    groups = c(groups,'country')
  }
  pop = aggregate(list(population=pop$all), pop[, groups], sum)
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
#' @family geography
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
#' @family geography
#' @export
load_population_age <- function(geo, year, age.breaks=NULL, version=NULL, ...) {
  loader = .Share$population.age.loader
  if( !is.null(.Share$load_population_age) ) {
    loader = .Share$load_population_age
  }
  if(is.null(loader)) {
    rlang::abort("No age-group population loader found for this platform")
  }
  if(is.character(loader)) {
    loader_type = loader
    loader = function(...) {
      load_population_age.impl(loader_type = loader_type, ...)
    }
  }

  if(!is.function(loader)) {
    rlang::abort("Population loader must be a function", loader=loader)
  }
  loader(geo=geo, year=year, age.breaks = age.breaks, version=version, ...)
}

#'
#' Aggregate population by age-group after data are loaded
#' @inheritParams load_population_age
#' @param pop loader population
#' @param col_geo geographic column containing the geographic level code
#' @param overall logical compute overall population (population by age, at the uppest geo level)
#'
#' @noRd
aggregate_pop_age = function(pop, year, geo, age.breaks=NULL, type=NULL, col_geo, overall, .attr=NULL) {
  # Restrict on type if necessary
  if( !is.null(type) ) {
    pop = pop[ geo_is_type(geo, type, pop[, col_geo]), ]
  }

  if( !is.null(age.breaks) ) {
    pop$age.max[is.na(pop$age.max)] = pop$age.min[is.na(pop$age.max)]
    pop$cat.min = ifnBase::cut_age(pop$age.min, age.breaks)
    pop$cat.max = ifnBase::cut_age(pop$age.max, age.breaks)
    stopifnot( all(pop$cat.min == pop$cat.max))
    pop = swMisc::replace_names(pop, "age.cat"="cat.min")
    pop = aggregate(as.list(pop[, c('all','male','female')]), as.list(pop[,c('age.cat', col_geo)]),  sum)
  }

  if( overall ) {
    if(is.null(age.breaks)) {
      column = 'age.min'
    } else {
      column = 'age.cat'
    }
    pop = aggregate(as.list(pop[, c('all','male','female')]), pop[ , column, drop=FALSE],  sum)
  }

  if(!is.null(.attr)) {
    for(n in names(.attr)) {
      attr(pop, n) <- .attr[[n]]
    }
  }

  pop
}

#' Load Population by age
#' @inherit load_population_age
#' @param loader_type type of loader to use
#' @param country country to get population (only for platform handling country column, see \code{\link{platform_options}})
#' @param type feature of geographic zone to test using \code{\link{geo_is_type}()}
#' @details
#' 3 loaders are availables :
#' \describe{
#'  \item{db}{Database loader}
#'  \item{file}{File for all the platform}
#'  \item{country_file}{File by country, this require the platform variable "country" to be defined to the country to use}
#' }
#'
load_population_age.impl <- function(loader_type, geo, year, age.breaks=NULL, type=NULL, country=NULL,...) {

  overall = F # if true make the overall sum by age (used when geo is null)

  .attr = list( # Attributes to pass to final data.frame
    "geo"= geo,
    "loader_type"=loader_type
  )

  if( is.null(geo) ) {
    ll = geo_hierarchy()
    geo = ll[length(ll)] # consider the last upper level
    overall = T
  }

  col_geo = geo_column(geo)

  if(!loader_type %in% c('db','file','country_file')) {
    rlang::abort(paste0("Unknown age population loader ", sQuote(loader_type)))
  }

  # Check if country param can be used for this platform
  # Only when the platform handles in its table an extra "country" column
  # This is only for the european database.
  use.country = can_use_country(country)

  use.lastyear = isTRUE(attr(year, "max_available"))

  if(loader_type == "db") {

    if(use.lastyear) {
      op_year = '<='
    } else {
      op_year = '='
    }

    query = paste0('select "',col_geo,'", "age_min" as "age.min", "age_max" as "age.max", "all","male","female" from pop_age5_',geo,' where "year"', op_year ,year)
    if(use.country) {
      query = paste0(query, ' and ', db_equal("country", country))
    }
    pop = dbQuery(query)
  }

  if(loader_type %in% c('file', "country_file")) {
    if(loader_type == "file") {
      file = paste0('pop/pop_age5_', geo)
    } else {
      if(!is.null(country)) {
        rlang::abort("country param is not useable with country_file as loader for age-group population")
      }

      # By default population files will be in
      # share/data/pop/ and name [country]_pop_age5_[level]
      # country
      country = .Share$country
      if(is.null(country)) {
        rlang::abort("country_file as population loader requires `country` to de defined in platform file")
      }

      file = paste0('pop/',country,'_pop_age5_', geo)

     }

    file = share.data.path(paste0(file, '.csv'))
    if(!file.exists(file)) {
      rlang::abort(paste0("File ", sQuote(file)," doesnt exists"))
    }

    .attr$file = file

    pop = utils::read.csv2(file)

    if(nrow(pop) > 0) {

      if(loader_type == "country_file") {
        if(geo == "country") {
          # In country file the "country" column is missing
          pop[[col_geo]] = country
        }
        if(hasName(pop, "age_min")) {
          pop = swMisc::replace_names(pop, "age.min"="age_min")
        }
      }
    }

  }

  if(use.lastyear) {
      max.year = max(pop$year[ pop$year <= year ])
      pop = pop[ pop$year == max.year, ]
      if(max.year < year) {
        rlang::warn(paste0("Population by age for year ",year," is not available, using population from year ", max.year))
      }
      .attr$max.year = year
  } else {
      pop = pop[ pop$year %in% year, ]
  }

  .attr$year = year

  if(nrow(pop) == 0) {
    rlang::warn(paste("No population for year", year,"at", geo, " level"))
    return(pop)
  }

  aggregate_pop_age(pop, geo=geo, year=year, age.breaks=age.breaks, type=type, col_geo=col_geo, overall = overall, .attr = .attr)

}

#' Allow the population year to be the last available
#'
#' This function wrap the requested year number adding attribute to inform the population
#' loader that it can provide population of previous years if the requested year is not available
#' This allow to use an older population if the population data source is not up to date yet.
#'
#' @param year requested year
#' @export
max_year_available=function(year) {
  attr(year, "max_available") <- TRUE
  year
}
