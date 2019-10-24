#' return first argument which is not NA
#' @param x vector of values
#' @export
coalesce <- function(x) {
  x[ min(which(!is.na(x))) ]
}


#' Create syndrom columns
#'
#' Transform syndrom name is one column to multiple column (one for each levels of the colum)
#'
#' @param weekly weekly data.frame()
#' @param column column containing syndrom name
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
#'
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
calc.weekly.delay = function(weekly, time.col) {
  unlist(tapply(weekly[, time.col], list(weekly$person_id), function(x) { c(NA, diff(x))} ))
}

#' Compute order of weekly response
#' @param weekly weekly data.frame()
calc.weekly.order = function(weekly) {
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
      steps[name] <<- n
    },
    get_steps = function() {
      steps
    }

  )
}
