# Strata stats helpers

#' Define participants weight from general population to compute adjusted frequency
#'
#' Weights can be stratified and grouped : weights are computed by strata over each group
#' For example : strata=age, gender and group= week, will allow to compute weekly frequency adjusted by age and gender
#'
#' Strata can be either 'age.cat' or 'gender', since provided population are by age and gender. To have more strata will need to have
#' general population data for each strata.
#'
#' @param age.categories age breaks vector (strictly increasing and unique values)
#' @param year.pop year of the population to load
#' @param groups list of weekly column to use as weight group
#' @param strata list of intake columns to use as stratification to compute weight.. Must be one of pop (age.cat or gender)
#' @param weekly weekly data
#' @param intake intake data
#' @return data.frame() with columns (person_id, `groups...`, `strata...`, weight)
#' @export
create_participant_weight = function(age.categories, year.pop, groups, strata, weekly , intake) {

  requireNamespace("tidyr")
  requireNamespace("survey")

  design.strata = design_incidence(age.categories, year.pop, geo=NULL, geo_column="code_pay")

  # Compute strata population
  pop = tidyr::pivot_longer(design.strata$population[, c('age.cat','male','female')], cols=c('male','female')) %>%
    dplyr::rename("gender"="name","pop"="value") %>%
    dplyr::mutate(
      total_pop=sum(pop),
      prop_pop=pop / total_pop
    )

  pop = dplyr::mutate_at(pop, c('age.cat','gender'), as.character)

  ww = weekly %>% dplyr::group_by(!!!syms(groups), person_id) %>% dplyr::summarize(n=1)

  ww = merge(ww, intake[, c('person_id', strata)], by="person_id", all.x=TRUE)

  pop.strata = ww %>%
    dplyr::group_by(!!!syms(groups), !!!syms(strata)) %>%
    dplyr::summarize(n_part=sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(!!!syms(groups)) %>%
    dplyr::mutate(total_part=sum(n_part))

  pop.strata = merge(pop.strata, pop, by=strata, all.x=TRUE)

  pop.strata = pop.strata %>%
    dplyr::mutate(
      n_expected = total_part * prop_pop,
      weight = n_expected / n_part
    )

  # Weight of participants by week
  part.weight = merge(ww, pop.strata[, c(strata,groups, 'weight')], by=c('yw', strata), all.x=TRUE)

  part.weight = part.weight %>% dplyr::select(-n)

  attr(part.weight, "pop.strata") <- pop.strata

  part.weight
}

#' Create a design for adjustment computation
#' @param data dataset to adjust on
#' @param col_weight name of the column containing weight
#' @return survey::svydesign object
#' @export
design_weight = function(data, col_weight="weight") {
  requireNamespace("survey")
  survey::svydesign(~1, data=data[!is.na(data[[col_weight]]), ], weights=stats::as.formula(paste("~", col_weight)))
}

#' Compute weighted proportion by group for a logical variable
#' @param design survey design [survey::svydesign()], see [design_weight()]
#' @param var variable on which to compute frequency, must be a logical variable
#' @param by grouping variable name to compute total on
#' @param ci.method method to use to compute adjusted proportion confidence interval
#' @return data.frame()
#'    with columns
#'    \describe{
#'     \item{by...}{group name of this column is the passed value to the `by` parameter}
#'     \item{n_adj}{Count of true values in each group}
#'     \item{n_weight}{Weight sum of the true value in the group}
#'     \item{total_weight}{Total weight in the group}
#'     \item{total_adj}{Total count of non missing value in the group}
#'     \item{prop_adj}{Adjusted proportion on strata}
#'     \item{prop_adj_low}{Adjusted proportion lower bound of CI95\%}
#'     \item{prop_adj_up}{Adjusted proportion upper bound of CI95\%}
#'     \item{variable}{Variable name}
#'    }
#'
#' @export
prop_by_weighted = function(design, var, by, ci.method="beta") {
  i = !is.na(design$variables[[var]])
  if(sum(i) == 0) {
    return(NULL)
  }
  dd = survey::svyby(as.formula(paste("~", var)), as.formula(paste("~", by)), design=design[i], survey::svyciprop, vartype="ci", method=ci.method)
  names(dd) <- c(by, 'prop_adj', 'prop_adj_low', 'prop_adj_up')
  dd$variable = var

  tt = design$variables[i,] %>%
    dplyr::group_by(!!sym(by)) %>%
    dplyr::filter(!is.na(!!sym(var))) %>%
    dplyr::summarize(
      n_adj=sum(!!sym(var)),
      total_adj=n(),
      n_weight=sum(ifelse(!!sym(var), weight, 0)),
      total_weight=sum( weight )
    )

  dd = merge(dd, tt, by=by, all=TRUE)

  dd
}

#' Compute frequency by group of a qualitative variable of a data.frame()
#'
#' Compute frequency of each level (or value) of the variable, by another grouping variable(s)
#'
#' @param data data.frame()
#' @param variable variable to compute frequency on (for each level)
#' @param by grouping variables (final output)
#' @param complete if TRUE missing levels in groups will be completed with zero frequency
#' @return data.frame()
#'   Returned columns:
#'   \describe{
#'     \item{level}{level of the variable}
#'     \item{n}{frequency of the level in the group}
#'     \item{n_total}{Total of non missing value in for the variable in the group}
#'     \item{prop_raw}{Proportion of the level}
#'     \item{group...}{One column of each group, with the value of the current group}
#'   }
#' @export
freq_var_by = function(data, variable, by='yw', complete=TRUE) {
  # How group data at the lowest level (week + adjusting strata)
  groups = c(by)

  freq = data %>%
    dplyr::group_by(!!!syms(by), !!sym(variable)) %>%
    dplyr::summarize(
      n=n()
    ) %>%
    dplyr::rename("level"=!!sym(variable)) %>%
    dplyr::ungroup()
  freq$level = as.character(freq$level)

  missing = freq %>% dplyr::filter(is.na(level))

  freq = freq %>% dplyr::filter(!is.na(level))

  if(complete) {
    # Complete missing levels
    m = unique(freq[, by, drop=FALSE])
    m = merge(m, data.frame(level=unique(freq$level)), all=TRUE)
    if(nrow(m) > 0 && nrow(freq) > 0) {
      freq$miss.level = TRUE
      freq = merge(freq, m, all=TRUE, by=c(by, 'level'))
      i = is.na(freq$miss.level)
      freq$n[i] = 0
    }
  }

  # n_total = total number of observation across all responses (for all by groups)
  freq = freq %>%
    dplyr::group_by(!!!syms(by)) %>%
    dplyr::mutate(
      n_total = sum(n)
    ) %>%
    dplyr::ungroup()

  freq = dplyr::mutate(freq,
        prop_raw = n / n_total
  )

  cols = c(by, 'level','n', 'n_total','prop_raw')
  freq = freq[, cols]
  attr(freq, "groups") <- groups
  attr(freq, "missing") <- missing
  freq
}

#' Frequency of several variables by group
#' @param vars list of variables to compute frequency on
#' @param data data.frame() data
#' @param by name of grouping variable
#'
#' @return data.frame()
#'   Returned columns:
#'   \describe{
#'     \item{variable}{variable name},
#'     \item{level}{level of the variable}
#'     \item{n}{frequency of the level in the group}
#'     \item{n_total}{Total of non missing value in for the variable in the group}
#'     \item{prop_raw}{Proportion of the level}
#'     \item{group...}{One column of each group, with the value of the current group}
#'   }
#'   An attribute "missing" contains missing count for each variable by group
#' @export
freq_vars_by = function(data, vars, by) {
  names(vars) <- vars
  missing = NULL
  ff = lapply(vars, function(variable) {
    d = freq_var_by(data, variable, by=by)
    m = attr(d, "missing")
    if(nrow(m) > 0) {
      m$variable = variable
      missing <<- dplyr::bind_rows(missing, m)
    }
    d
  })
  ff = dplyr::bind_rows(ff, .id="variable")
  ff$variable = factor(ff$variable)
  attr(ff, "missing") <- missing
  ff
}

#' Compute frequency of a set of logical variables by group
#' Adjusted frequency can be computed by providing a design object with computed weights
#' @param data data.frame containing data
#' @param vars list of variable names to compute frequency on. They must be logical
#' @param by grouping variable
#' @param design `svydesign` object, if provided will compute adjusted frequency using the design (data should contains the weight column as described in design)
#' @export
freq_bool_by = function(data, vars, by, design=NULL) {

  if(is.null(design) && is(data, "survey.design")) {
    design = data$variables
  }

  ff = freq_vars_by(data, vars, by)
  missing = attr(ff, "missing")
  ff = ff %>% dplyr::filter(!is.na(level) & level == "TRUE")
  ff$variable = factor(ff$variable)
  if(!is.null(design)) {
    names(vars) <- vars
    adj = dplyr::bind_rows(lapply(vars, function(var) {
      prop_by_weighted(design=design, var=var, by=by)
    }))
    if(nrow(adj) > 0) {
      ff = merge(ff, adj, by=c(by, "variable"), all.x=TRUE)
    }
  }
  if(!is.null(missing) && nrow(missing) > 0) {
    missing = dplyr::rename(missing, n_missing=n) %>% dplyr::select(!!sym(by), "variable", "n_missing")
    ff = merge(ff, missing, by=c(by, "variable"), all.x=TRUE)
    ff = ff %>% dplyr::mutate(n_missing=ifelse(is.na(n_missing), 0, n_missing))
  }
  ff
}
