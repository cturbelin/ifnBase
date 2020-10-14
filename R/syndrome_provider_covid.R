

#' Syndrome Provider with COVID-19 like definitions
#'
#' To run need weekly with get_columns_for_incidence() column list
#' @examples
#' \dontrun{
#'  season = 2017
#'  weekly = survey_load_results("weekly", get_columns_for_incidence(), season=season)
#'  intake = survey_load_results("weekly", c('timestamp','date.birth'))
#'  weekly = recode_weekly(weekly)
#'  intake = recode_intake(intake)
#'  provider = SyndromeProviderCovid$new()
#'  r = provider$compute(weekly, intake)
#' }
#'
#' @export
SyndromeProviderCovid <- R6Class("SyndromeProviderCovid",  inherit=SyndromeProvider,

public = list(

  #' @field pain.age.limit age under which pain & headache will exluded from defintion
  pain.age.limit = NULL,

  #' @field use.sudden logical, use sudden feature to build the syndrome
  use.sudden = TRUE,

  #' @field definitions character vector of definitions to output
  definitions = NULL,

  #' @description
  #' instanciate object
  #' @param pain.age.limit age to take into account of subjectives symptoms
  #' @param use.sudden use sudden appearance of symptoms in the definitions, if FALSE will be considered as always sudden
  initialize = function(pain.age.limit=5, use.sudden=TRUE) {
    self$pain.age.limit = pain.age.limit
    self$use.sudden = use.sudden
  },

  #' @description
  #' Get available syndromes names
  #' @return character()
  syndromes = function() {
    c('covid.ecdc')
  },

  #' @description
  #' Update definitions list to compute
  #' @param definitions character vector of definition name to compute
  check_definitions = function(definitions) {
    available = self$syndromes()

    if(is.null(definitions)) {
      definitions = available
    }

    if(any(!definitions %in% available)) {
      n = definitions[!definitions %in% available]
      rlang::abort(paste0("Unknown definition", paste(n, collapse = ",")))
    }

    definitions

  },

  #' @description
  #' Compute definitions for all syndromes
  #' @param weekly weekly data
  #' @param intake intake data with at least 'person_id', 'age' column
  #' @param definitions character vector of definition to use
  #' @param use.sudden logical if TRUE use is_sudden, otherwise consider it's always TRUE
  #' @return data.frame with each computed syndrome in column, and "id" column from weekly
  compute = function(weekly, intake, definitions=NULL) {

    if(!is.null(definitions)) {
      rlang::warn("Use definitions in constructor")
      self$update_definitions(definitions)
    }

    definitions = self$check_definitions(definitions)

    # New symptoms to be used in following definitions
    new.sympts = c(SYMPT_SMELL, SYMPT_TASTE)

    use.sudden = self$use.sudden
    use.new = all(hasName(weekly, new.sympts))

    weekly = self$compute_age(weekly, intake)

    subjectives = c(SYMPT_HEADACHE, SYMPT_PAIN, SYMPT_CHESTPAIN, SYMPT_NAUSEA, SYMPT_ASTHENIA)
    if(use.new) {
      subjectives = c(subjectives, new.sympts)
    }

    # Check if true, false otherwise including missing
    is_true = function(x) {
      !is.na(x) & x
    }

    # Combine several symptoms with OR operator
    or = function(...) {
      n = unlist(rlang::list2(...))
      apply(weekly[, n], 1, any, na.rm=TRUE)
    }

    if(use.sudden) {
      sympt.sudden = is_true(weekly[["sympt.sudden"]])
    } else {
      sympt.sudden = TRUE
    }

    use_subjective =  !is.na(weekly[["age"]]) & weekly[["age"]] > self$pain.age.limit
    d = list()

    d$id = weekly$id # As the weekly has been merged, rows are not in the same order.

    if('covid.ecdc' %in% definitions) {

      if(use.new) {
        sensor = sympt.sudden & use_subjective & or(SYMPT_SMELL, SYMPT_TASTE)
      } else {
        sensor = FALSE
      }

      d$covid.ecdc = or(SYMPT_COUGH, SYMPT_FEVER, SYMPT_DYSPNEA) | sensor
    }

    d
  }
 ) # Public
)

