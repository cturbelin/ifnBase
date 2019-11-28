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
SyndromeProviderRS2019 <- R6Class("SyndromeProviderRS2019", , inherit=SyndromeProvider,

public = list(

  #' @field pain.age.limit age under which pain & headache will exluded from defintion
  pain.age.limit = NULL,

  #' @description
  #' instanciate object
  #' @param pain.age.limit age to take into account of pain & heacache
  initialize = function(pain.age.limit=5) {
    self$pain.age.limit = pain.age.limit
  },

  #' @description
  #' Compute definitions for all syndromes
  #' @param weekly weekly data
  #' @param intake intake data with at least 'person_id', 'age' column
  #' @param definitions character vector of definition to use
  #' @return data.frame with each computed syndrome in column, and "id" column from weekly
  compute = function(weekly, intake, definitions=NULL) {

    available = c('ili', 'ili.f', 'ili.minus', 'ili.minus.fever','ili.who','ari.ecdc', 'ari.plus', 'ari')

    if(is.null(definitions)) {
      definitions = available
    }

    if(any(!definitions %in% available)) {
      n = definitions[!definitions %in% available]
      stop(paste0("Unknown definition", paste(n, collapse = ",")))
    }

    weekly = self$compute_age(weekly, intake)

    fever_level_38 = 3
    fever_level_39 = 4

    pain.age.limit = self$pain.age.limit

    has_pain = function(r)  {
      ifelse( !is.na(r$age) & r$age <= pain.age.limit, TRUE, r$pain)
    }

    fever_or_level = function(r, level) {
      has_level = (!is.na(r$highest.temp) & r$highest.temp >= level)
      r$fever | has_level
    }

    fever_with_level = function(r, level) {
      (!is.na(r$highest.temp) & r$highest.temp >= level)
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
    sudden = self$is_sudden(r)

    d = list()

    d$id = weekly$id # As the weekly has been merged, rows are not in the same order.

    if('ili' %in% definitions) {
      d$ili = sudden & fever_with_level(r, fever_level_39) & pain & respi_nose
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

    if('ari.ecdc' %in% definitions) {
      d$ari.ecdc = sudden & general_ari & respi_short
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

