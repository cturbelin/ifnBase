#' return first argument which is not NA
#' @param x vector of values
#' @export
coalesce <- function(x) {
  x[ min(which(!is.na(x))) ]
}


#' Create syndrom columns from factor encoded column
#'
#' Create one boolean column for each level of the given column
#'
#' @param weekly weekly data.frame()
#' @param column column containing syndrom name
#' @export
create_syndrom_columns = function(weekly, column) {
  syndroms = levels(weekly[[column]])
  names(syndroms) = syndroms

  for(i in 1:length(syndroms)) {
    n = names(syndroms)[i]
    weekly[, syndroms[i] ] = ifelse( weekly[[column]] == n, TRUE, FALSE)
  }
  attr(weekly, "syndroms") <- syndroms
  weekly
}

#' Default set parameters replace default value if exits in params
#' @noRd
#' @param params list of parameters
#' @param def.params default parameters list
default.params = function(params, def.params) {
  for(p in names(def.params)) {
    v = params[[p]]
    if( is.null(v) ) {
      v = def.params[[p]]
    }
    params[[p]] = v
  }
  params
}


#' Compute delay between 2 surveys for each participant
#' weekly should be ordered by person_id then timestamp !
#' @param weekly data.frame with at least (person_id, [time.col]) columns
#' @param time.col column containg time value to compute delay from
#' @export
calc_weekly_delay = function(weekly, time.col) {
  unlist(tapply(weekly[, time.col], list(weekly$person_id), function(x) { c(NA, diff(x))} ))
}

#' Compute order of weekly response
#' @param weekly weekly data.frame()
#' @export
calc_weekly_order = function(weekly) {
  weekly = weekly[ order(weekly$person_id, weekly$timestamp),]
  weekly$order = unlist( lapply(tapply(weekly$person_id, weekly$person_id, length), function(n) { 1:n }))
  weekly
}


#' Create a step counter
#'
#' Simple interface to hold a step counter
#'
#' @return list
create_step_tracker = function() {

  steps = list()

  list(
    add = function(name, n) {
      if(is.data.frame(n)) {
        n = nrow(n)
      }

      steps[name] <<- n
    },
    get_steps = function() {
      steps
    }

  )
}

create_profiler = function() {
  times = data.frame()
  last.time = Sys.time()

  list(
    track = function(point, step=NULL) {
      time = Sys.time() - last.time
      r = list(time=time, point=point)
      if(!is.null(step)) {
        r$step = step
      }
      times <<- dplyr::bind_rows(times, r)
      last.time <<- Sys.time()
    },
    get = function() {
      times
    }
  )
}

#' Verticalize incidence data after computation
#'
#' Incidence data are organized horizontally, estimation for each syndrom are in columns (with suffixes .upper, .lower, .w2)
#' This function reshape to a vertical storage format
#' Output will be [syndrome, type, value, upper, lower]
#'
#'
#' @seealso \code{\link{calc_adjusted_incidence}}
#'
verticalize_incidence = function(inc, ids, syndroms) {

  # data data.frame
  # id.vars id columns
  # vars list of vars column [var].[mesure]
  # measures list of measures
  # v.name : name of column containing variable name in vertical data
  # var.measure : name of the column containing the value of the column with variable name (without measure)
  extract_columns = function(data, id.vars, vars, measures, v.name, var.measure=NULL) {
    dd = lapply(vars, function(variable) {
      if(length(measures) > 0) {
        columns = paste(variable, measures, sep='.')
      } else {
        columns = c()
      }

      if(!is.null(var.measure)) {
        columns = c(columns, variable)
      }

      n = columns[columns %in% names(data)]
      d = data[, c(id.vars, n), drop=FALSE]

      nn = names(d)
      if(!is.null(var.measure)) {
        nn[ nn == variable ] = var.measure
      }
      nn = gsub(paste0("^", variable,"\\."),"", nn)
      names(d) <- nn
      d[, v.name] = variable
      d
    })
    dd = dplyr::bind_rows(dd)
    dd
  }

  # Verticalize syndroms
  data = extract_columns(inc, syndroms,id.vars=ids, measures=c('crude','adj', 'crude.lower','crude.upper','adj.upper','adj.lower'), v.name="syndrom", var.measure="count")

  # Verticalize estimation type
  data = extract_columns(data, id.vars=c(ids, 'syndrom'), c('count','crude','adj'),  c('upper','lower'), v.name="type", var.measure = "value")

  active = extract_columns(inc, id.vars=c(ids), 'active', c(), v.name="syndrom", var.measure = "value")

  active$type = "count"
  active$upper = NA
  active$lower = NA

  data = dplyr::bind_rows(data, active)
  data
}
