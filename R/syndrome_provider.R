
#' Syndrome provider base class
#'
#' Define interface to build a SyndromeProvider
#'
#' This class compute "syndrome" definition from weekly & intake data
#' A syndrome will be computed as a logical value attributed for each weekly survey, with value TRUE if the survey comply with the given syndrome definition
#'
#' A syndrome provider can compute several syndromes, each one applying for a definition
#'
#' compute() should return a data.frame() with one logical column for each computed syndrome definition (use names carefully to avoid names collisions, avoid using
#' influenzanet base syndrome names : ili, cold, allergy, gastro )
#'
#' @export
SyndromeProvider <- R6Class("SyndromeProvider", public = list(

  #' @description
  #' Compute syndrome
  #' @param weekly weekly data
  #' @param intake intake data
  #' @return data.frame() with id column from weekly and extra column for each syndrome
  compute = function(weekly, intake) {

  },

  #' @description
  #' Compute age for each weekly using intake
  #' @param weekly weekly data
  #' @param intake intake data
  compute_age = function(weekly, intake) {
    ages = aggregate(age ~ person_id, data=intake, min) # consider only one age by person (the first given)
    ages$age[ ages$age < 0 | ages$age > 120] <- NA
    weekly = merge(weekly, ages, by='person_id', all.x=T)
    weekly
  },

  #' Common function, compute sudden symptomes using both variables
  #' @param r weekly data
  is_sudden = function(r) {
      (!is.na(r$sympt.sudden) & r$sympt.sudden) | (!is.na(r$fever.sudden) & r$fever.sudden)
  },

  #' @description
  #' Get available syndromes names
  #' @return character()
  syndromes = function() {

  }

 ) # public
) # R6Class

#' Test if object is a syndrome provider object
#' @param x object to test
#' @param accept_class if TRUE class will be accepted, not only instance
is_syndrome_provider = function(x, accept_class=FALSE) {
  if(R6::is.R6(x)) {
    h = class(x)
    return("SyndromeProvider" %in% h)
  }
  if(accept_class) {
    if(R6::is.R6Class(x) ) {
      h = c()
      r = x$get_inherit()
      while(!is.null(r)) {
        h = c(h, r$classname)
        r =  r$get_inherit()
      }
      return("SyndromeProvider" %in% r)
    }
  }
  return(FALSE)
}


