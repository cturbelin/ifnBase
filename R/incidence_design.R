# This library aims at providing set of function to compute incidence from InfluenzaNet data
#
# Several functions are provided:
#
# - IncidenceRS2014
# - IncidenceDailyRS2014
#
# - (weekly, intake, syndroms, params, design, output=NULL, ...) : Estimator Function interface
# weekly, intake, syndroms, params, design, output=NULL
# intake = last intake for each participant with necessary columns for stratification
# syndroms = list of indicators column for each "syndrom" for which estimate an incidence
# params  = parameters for a given estimator
# design  = design stratification definition (@see design.incidence)
# output  = kind of output to get inc=incidence (national level), zlow="lower geographic level (z)", "age" age-specific incidence
#         age categories should be in "age.cat" column in intake df.
#         It could be an character vector or a structure with more option returned by output.incidence
#
# Helpers:
#
# - design_incidence() : define a stratification structre
# - output_incidence() : define an output type and extra parameters for output results


#' Returns list of columns needed of weekly survey to compute incidence
#' @export
get_columns_for_incidence = function() {
  c('timestamp', get_symptoms_aliases(), 'fever.sudden','highest.temp','same.episode','sympt.start','fever.start','sympt.sudden')
}

#' Create a structure usable by incidence computation functions to describe the stratification to use (parameter design)
#'
#' Embed all data & metadata about stratification
#' Only handle strata on age and geo level for instance
#' By convention age group column will be names "age.cat"
#'
#' @param age.categories list of breaks
#' @param year.pop year of the population to fetch
#' @param geo geographic level id (see geography & geo.levels in platform config.). could be character or structure return by geo_level function
#' @param geo_column name of the geographic column  (value return by strata.call), geo_level's column name by default
#' @param geo_area id of geographic area to keep at the geographic level
#' @param ... other parameters to include in the design
#' @export
design_incidence = function(age.categories, year.pop, geo, geo_column=NULL, geo_area=NULL, ...) {

  if( is.null(geo_column) ) {
    geo_column = geo_column(geo)
  }

  if( is.null(geo_column) ) {
    stop(paste("Unknown geo level ", geo))
  }

  strata = geo_column

  pop = NULL
  if( !is.null(age.categories) ) {
    strata = c(strata, 'age.cat')
    pop = load_population_age(geo=geo, year=year.pop, age.breaks=age.categories)
    if( is.null(geo) ) {
      # if geo is null, then there is no "geographic" dimension (only one geo strata)
      # fake it with a geo_column with a single value
      # The same should be done in the user's data
      pop[, geo_column] = 1
    }
  } else {
    if( !is.null(year.pop) ) {
      pop = load_population(geo=geo, year=year.pop)
    }
  }
  if( !is.null(pop) ) {
    n = names(pop)
    n[ n == 'all' ] <- 'population'
    names(pop) <- n

    if( !is.null(geo_area) ) {
      pop = pop[ pop[[geo_column]] %in% geo_area, ]
    }

  }

  data = list(
    age.categories = age.categories,
    population = pop, # population  at geographic level for stratification
    strata = strata,
    geo_level = geo,
    geo_column=geo_column,
    year.pop = year.pop
  )
  l = list(...)
  if(length(l) > 0) {
    for(n in names(l)) {
      data[[n]] = l[[n]]
    }
  }
  structure(data, class="design.incidence")
}

#' Define structure containing incidence results output
#' "output" parameter of estimate_incidence_* functions could be either a character vector with the name of each desired
#' output type (inc, zlow, age)
#' or a structure return by this function which allow to pass some extra parameters to the output function (@see calc_adjusted_incidence)
#' @param types types of measures computed
#' @param conf.int confidence interval computed
#' @param adjust adjusted measures
#' @param ... data sets
#' @export
output_incidence = function(types, conf.int=T, adjust=T, ...) {
  data = list(types=types)
  l = list(...)
  if(length(l) > 0) {
    for(n in names(l)) {
      data[[n]] = l[[n]]
    }
  }
  ss = structure(data, class="output.incidence")
  attr(ss, 'conf.int') <- conf.int
  attr(ss, 'calc.adj') <- adjust
  ss
}



#' Compute confidence interval for rate
#' using poisson distribution law
#' @param inc data.frame from incidence estimator
#' @param v column name containing syndrom count
#' @param type crude to create rate column name
#' @param unit if rate need to be rescale
calc.conf.int = function(inc, v, type='crude', unit=1) {
  d = epitools::pois.exact(inc[, v], inc$active)
  cc = c('upper','lower')
  d = d[, cc]
  d$upper = d$upper * unit
  d$lower = d$lower * unit
  names(d) <- paste(v, type, cc, sep='.')
  d
}


#' Compute crude & adjusted incidence from a count data.frame
#' This function is shared by all incidence computation methods, once active participants & syndrom counts are provided
#'
#' Ouput columns are ([syndrom] is the column with logical value for presence of one syndrom in the weekly data, for example "ili"):
#'  - active : count of active participants
#'  - [syndrom] : count for this syndrom (or variable)
#'  - [syndrom].crude : crude incidence (without adjustment, count/active participant ratio)
#'  - [syndrom].adj : Adjusted incidence (taking into account of stratification design)
#'  - [syndrom].crude.(upper|lower) : upper & lower bound of CI95 for crude incidence (Exact poisson CI)
#'  - [syndrom].adj.(upper|lower) : upper & lower boud of CI95 for ajusted incidence (DKES method)
#'
#' @param count.week data.frame(), count for a week. Count of active participants is in "active" column
#' @param design design structure (defining strata and reference data like population in each strata)
#' @param syndroms char() list of column names for each syndrom count
#' @param output char() list of requested output "inc"=global incidence, "zlow"=lower geo level incidence, "age" by age incidence,
#' @export
calc_adjusted_incidence = function(count.week, design, syndroms, output) {

  if(nrow(count.week) == 0) {
    return(list())
  }

  if(class(output) == "character") {
    output.obj = output_incidence(output)
  } else {
    if(!"output.incidence" %in% class(output)) {
      stop("output should be a character vector or a output.incidence object")
    }
    output.obj = output
  }

  conf.int = attr(output.obj, 'conf.int')
  calc.adj = attr(output.obj, 'calc.adj')
  output = output.obj$types

  calc_inc_strata = function(count, column.strata, column.prop) {

    if(calc.adj && conf.int) {
      count$active.weight = count[ , column.prop ] / count$active
    }

    if( length(column.strata) > 1 ) {
      g = as.list(count[, column.strata])
    } else {
      g = list(count[, column.strata])
      names(g) <- column.strata
    }

    if( calc.adj ) {

      for(v in syndroms) {
        v.adj = paste0(v,'.adj')
        count[, v.adj ] = count[, column.prop] * as.vector(count[, v]) / count$active
        if(conf.int) {
          # Estimated variance of the weighted counts is sum(wi^2 * count) for a strata
          # see ref below
          v.w2 = paste0(v, '.w2')
          count[, v.w2 ] = count$active.weight^2 * count[, v]
        }
      }

      v = paste0(syndroms,'.adj')
      if(conf.int) {
        v.w2 = paste0(syndroms,'.w2')
      } else {
        v.w2 = c()
      }

      inc.adj = aggregate(as.list(count[, c(v, v.w2)]), g, sum, na.rm=T)
    }
    inc.crude = aggregate(as.list(count[, c(syndroms, 'active' )]), g, sum, na.rm=T)
    inc.crude[ paste0(syndroms,'.crude') ] = inc.crude[syndroms] / inc.crude$active

    if(conf.int) {
      for(v in syndroms) {
        d = calc.conf.int(inc.crude, v, unit=1, type="crude")
        inc.crude = cbind(inc.crude, d)
      }
    }

    if( calc.adj ) {

      inc.crude = cbind(inc.crude, inc.adj)
      if(conf.int) {
        # Now compute adjusted confidence interval
        # Based on DKES estimator Fay & Feuer, 1997, Stat In Med (16) p791-801
        # Based on Poisson confidence interval using a rescaled distribution
        # y = weighted rate, xi = count in strata; wi = weight in strata; Ni = Sample size in (i)th strata;
        # x = sum(xi); popi = population in strata i; popT = Total population (all strata = sum(popi))
        # Up(x) & Lp(x) 1/2* Chisq quantile for respectively (1 - a / 2, DF=2(x+1)) and (a/2, DF=2x)
        # wi = 1/Ni * (popi / popT)
        # v = sum(wi^2 * xi)
        # U(y) = y + ( sqrt(v) / sqrt(x) ) * ( Up(x) - x)
        # L(y) = y + ( sqrt(v) / sqrt(x) ) * ( Lp(x) - x)

        conf.level = .95
        N. = 1 - ((1 - conf.level)/2)

        for(v in syndroms) {
          v.w2 = paste0(v, '.w2') # Variance of count
          v.adj = paste0(v, '.adj')
          x = inc.crude[, v] # Total Counts

          x.up = 0.5 * stats::qchisq(1 - N., 2 * (x + 1), lower.tail= F)

          inc.crude[, paste0(v.adj, '.upper') ] = inc.crude[, v.adj] + ( sqrt( inc.crude[, v.w2]) / sqrt(x) ) * (x.up - x)

          x.low = 0.5 * stats::qchisq(N., 2 * x, lower.tail=F)

          inc.crude[, paste0(v.adj, '.lower') ] = inc.crude[, v.adj] + ( sqrt( inc.crude[, v.w2]) / sqrt(x) ) * (x.low - x)
        }
      }
    }
    inc.crude
  }

  # result data
  r = list()

  # No stratification
  # compute crude incidence for each row (assuming that are independent count)
  if( is.null(design) ) {
    if( any(output != "inc") ) {
      warning("design is NULL, cannot compute output type other than overall incidence")
    }
    for(v in syndroms) {
      count.week[, paste0(v, '.crude') ] = count.week[, v ] / count.week$active
      if(conf.int) {
        d = calc.conf.int(count.week, v, unit=1, type="crude")
        count.week = cbind(count.week, d)
      }
    }
    r$inc = count.week
    return(r)
  }

  # List of stratification columns
  strata = design$strata

  # population by age-group at the geo level
  population = design$population

  pop.total = sum(population$population)

  population$prop.pop.all = population$population / pop.total # Proportion of a given age-group in overall population (i.e  (prop pop in geo levels) * (pop age-group in each level) )

  count.week = merge(count.week, population[, c(strata,'prop.pop.all', 'population')], by=strata, all=T)

  # Incidence calculation by strata with adjustment
  # count = count data.frame (syndrom + active), column.strata = strata column, column.prop= column with adjustment factor
  if( !is.null(count.week$yw ) ) {
    group = "yw"
  } else {
    group = c()
  }

  if("inc" %in% output) {
    # Overall incidence
    count.week$zone = 1
    r$inc = calc_inc_strata(count.week, column.strata=c("zone", group), column.prop='prop.pop.all')
  }

  if( any( c('zlow','age') %in% output ) ) {
    # population in each geographic area (at lower level)
    pop.zlow = aggregate(stats::as.formula(paste("population ~ ", design$geo_column)), data=population, sum)
    names(pop.zlow) <- c(design$geo_column, 'pop.zlow')
    count.week = merge(count.week, pop.zlow, by=design$geo_column, all.x=T)
  }

  if("zlow" %in% output) {
    count.week$prop.age.zlow = count.week$population / count.week$pop.zlow # proportion of an age-group in each geographic area (at the lower level)
    r$zlow = calc_inc_strata(count.week, column.strata=c(group, design$geo_column), column.prop='prop.age.zlow')
  }
  if("age" %in% output) {
    count.week$prop.zlow = count.week$pop.zlow / pop.total # proportion of each geographic level in overall population
    r$age = calc_inc_strata(count.week, column.strata=c(group, "age.cat"), column.prop='prop.zlow')
  }
  if("count" %in% output) {
    r$count = count.week
  }
  r
}
