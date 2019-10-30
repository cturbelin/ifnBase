
#' Syndrome provider base class
SyndromeProvider <- R6Class("SyndromeProvider", public = list(

  compute = function(weekly, intake) {

  },

  compute_age = function(weekly, intake) {
    ages = aggregate(age ~ person_id, data=intake, min) # consider only one age by person (the first given)
    ages$age[ ages$age < 0 | ages$age > 120] <- NA
    weekly = merge(weekly, ages, by='person_id', all.x=T)
    weekly
  },

  # Common function
  # @param r weekly data
  is_sudden = function(r) {
      (!is.na(r$sympt.sudden) & r$sympt.sudden) | (!is.na(r$fever.sudden) & r$fever.sudden)
  }

))

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
