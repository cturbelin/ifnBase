#' Syndrome provider base class for historical Influenzanet syndromic classifier
#' @export
SyndromeProviderIfn <- R6Class("SyndromeProvider", public = list(

  #' @field version of definition to use
  version = NULL,

  #' @field regroup syndroms using prettier labels
  regroup = TRUE,

  #' @param version classifier version to use, default is 2012
  #' @param regroup use grouped version of syndromes for 2012 if TRUE (default)
  initialize = function(version=2012, regroup=TRUE) {
    if(!version %in% c(2011, 2012)) {
      rlang::abort(paste("Unknown Influenzanet syndromes version ", sQuote(version)))
    }
    if(!is.logical(regroup)) {
      rlang::abort("regroup must be logical")
    }
    self$version = version
    self$regroup = regroup
  },

  #' @description
  #' Compute syndrome
  #' @param weekly weekly data
  #' @param intake intake data
  #' @return data.frame() with id column from weekly and extra column for each syndrome
  compute = function(weekly, intake) {

    requires.columns = c("sorethroat", "cough", "dyspnea", "diarrhea", "nausea", "vomiting",
                         "abdopain", "rhino", "sneeze", "highest.temp", "wateryeye", "sympt.cause",
                         "no.sympt", "sympt.sudden", "fever.sudden", "fever", "chills",
                         "asthenia", "headache", "pain")

    if(!all(hasName(weekly,requires.columns))) {
      n = requires.columns[ !requires.columns %in% names(weekly), ]
      rlang::abort("Missing columns to be able to compute definition ", paste(sQuote(n), collapse = ","))
    }

    if(self$version == 2011) {
      r = syndromes_influenzanet_2011(weekly, as.levels=TRUE)
    }
    if(self$version == 2012) {
      r = syndromes_influenzanet_2012(weekly, as.levels=TRUE)
      if(self$regroup) {
        r = regroup.syndrome(r)
        r = factor(r)
      }
    }
    n = levels(r)
    names(n) <- n
    d = lapply(n, function(v) {
      r == v
    })
    d$id = weekly$id
    d
  }

 ) # public
) # R6Class
