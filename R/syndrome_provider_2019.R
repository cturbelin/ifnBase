#' Syndrome Provider with 2019 revised ILI & Ari definitions
#'
#' To run need weekly with get_columns_for_incidence() column list
#' @examples
#' \dontrun{
#'  season = 2017
#'  weekly = survey_load_results("weekly", get_columns_for_incidence(), season=season)
#'  intake = survey_load_results("weekly", c('timestamp','date.birth'))
#'  weekly = recode_weekly(weekly)
#'  intake = recode_intake(intake)
#'  provider = SyndromeProviderRS2019$new()
#'  r = provider$compute(weekly, intake)
#' }
#'
#' @export
SyndromeProviderRS2019 <- R6Class("SyndromeProviderRS2019",  inherit=SyndromeProvider,

public = list(

  #' @field pain.age.limit age under which pain & headache will exluded from defintion
  pain.age.limit = NULL,

  #' @field use.sudden logical, use sudden feature to build the syndrome
  use.sudden = TRUE,

  #' @field definitions character vector of definitions to output
  definitions = NULL,

  #' @description
  #' instanciate object
  #' @param pain.age.limit age to take into account of pain & heacache
  #' @param definitions list of definitions to use
  #' @param use.sudden use sudden appearance of symptoms in the definitions, if FALSE will be considered as always sudden
  initialize = function(pain.age.limit=5, definitions=NULL, use.sudden=TRUE) {
    self$pain.age.limit = pain.age.limit
    self$use.sudden = use.sudden
    self$update_definitions(definitions)
  },

  #' @description
  #' Update definitions list to compute
  #' @param definitions character vector of definition name to compute
  update_definitions = function(definitions) {
    available = c('ili.rs', 'ili.f', 'ili.minus', 'ili.minus.fever','ili.who','ili.ecdc', 'ari.plus', 'ari')

    if(is.null(definitions)) {
      definitions = available
    }

    if(any(!definitions %in% available)) {
      n = definitions[!definitions %in% available]
      rlang::abort(paste0("Unknown definition", paste(n, collapse = ",")))
    }

    self$definitions = definitions

  },


  #' @description
  #' Compute definitions for all syndromes
  #' @param weekly weekly data
  #' @param intake intake data with at least 'person_id', 'age' column
  #' @param definitions character vector of definition to use
  #' @param use.sudden logical if TRUE use is_sudden, otherwise consider it's always TRUE
  #' @return data.frame with each computed syndrome in column, and "id" column from weekly
  compute = function(weekly, intake, definitions=NULL, use.sudden=NULL) {

    if(!is.null(definitions)) {
      rlang::warn("Use definitions in constructor")
      self$update_definitions(definitions)
    }

    definitions = self$definitions

    if(!is.null(use.sudden)) {
      rlang::warn("`use.sudden` should be used in constructor")
      self$use.sudden = use.sudden
    }

    use.sudden = self$use.sudden

    weekly = self$compute_age(weekly, intake)

    codes_fever_level_38 = c(3, 4, 5)
    codes_fever_level_39 = c(4, 5)

    if(is.factor(weekly$highest.temp)) {
      lev = levels(weekly$highest.temp)
      if(all(lev %in% FEVER.CODES)) {
        lev = lev[!lev %in% FEVER.CODES]
        rlang::abort(paste("highest.temp doesnt has the right levels", paste(sQuote(lev), collapse = ',')))
      }

      fever_level_38 = names(FEVER.CODES %in% codes_fever_level_38)
      fever_level_39 = names(FEVER.CODES %in% codes_fever_level_39)

    } else {
      fever_level_38 = codes_fever_level_38
      fever_level_39 = codes_fever_level_39
    }

    pain.age.limit = self$pain.age.limit

    has_pain = function(r)  {
      ifelse( !is.na(r$age) & r$age <= pain.age.limit, TRUE, r$pain)
    }

    fever_or_level = function(r, level) {
      has_level = (!is.na(r$highest.temp) & r$highest.temp %in% level)
      r$fever | has_level
    }

    fever_with_level = function(r, level) {
      (!is.na(r$highest.temp) & r$highest.temp %in% level)
    }

    any_of = function(r, cols) {
      apply(r[, cols], 1, any)
    }

    r = weekly

    respi_nose = any_of(r, c('sorethroat','cough','dyspnea', 'sneeze' ,'rhino'))
    respi_short =any_of(r, c('sorethroat','cough','dyspnea'))

    pain = has_pain(r)
    # Always TRUE if child < pain.age.limit
    pain_or_headache = pain | r$headache

    if(use.sudden) {
      sudden = self$is_sudden(r)
    } else {
      sudden = rep(TRUE, nrow(weekly))
    }

    d = list()

    d$id = weekly$id # As the weekly has been merged, rows are not in the same order.

    if('ili.rs' %in% definitions) {
      d$ili.rs = sudden & fever_with_level(r, fever_level_39) & pain & respi_nose
    }

    if('ili.f' %in% definitions) {
      d$ili.f = sudden & fever_or_level(r, fever_level_39) & pain & respi_nose
    }

    if('ili.minus' %in% definitions) {
      d$ili.minus = sudden & fever_or_level(r, fever_level_38) & pain_or_headache & respi_short
    }

    if('ili.minus.fever' %in% definitions) {
      d$ili.minus.fever = sudden & r$fever & pain_or_headache & respi_short
    }

    if('ili.who' %in% definitions) {
      d$ili.who = sudden & fever_with_level(r, fever_level_38) & pain_or_headache & (r$cough | r$sorethroat)
    }

    # Pain or Headache considered as false if age under pain.age.limit
    # So pain and Headache are not evaluated if age under pain.age.limit
    general_ari = r$fever | r$chills | r$asthenia | ifelse( !is.na(r$age) & r$age <= pain.age.limit, FALSE, r$pain | r$headache)

    # ILI proposed by ECDC (close to ARI)
    if('ili.ecdc' %in% definitions) {
      d$ili.ecdc = sudden & general_ari & respi_short
    }

    if('ari.plus' %in% definitions) {
      d$ari.plus = sudden & general_ari & (respi_nose | r$sputum)
    }

    if('ari' %in% definitions) {
      d$ari = sudden & general_ari & (r$cough | r$rhino | r$sneeze)
    }
    d
  }
 ) # Public
)

