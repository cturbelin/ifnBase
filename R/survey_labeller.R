
#' Survey Labeller Class
#'
#' Class to manipulate survey 'labels' (list of labels or columns)
#' Labeller object expose several methods useful to manipulate labels set
#' @seealso Survey section \link{concepts}
#' @family survey
#'
#' @export
SurveyLabeller <- R6::R6Class("SurveyLabeller",
  public = list(

    #' @field labels List of values to be used a labels
    labels = NULL,

    #' @field glob pattern if the label is implicitly defined by a pattern
    glob=NULL,

    #' @param survey survey name
    #' @param name labels set name in the survey definition
    initialize=function(survey, name) {
      def = survey_definition(survey)
      labels = def$labels[[name]]
      if(is.null(labels)) {
        rlang::abort(sprintf("Unknown labels '%s' for '%s'", name, survey))
      }
      if(grepl("*", labels, fixed=TRUE)) {
        self$glob = labels
        pattern = glob2rx(labels)
        exclude = attr(labels, "exclude")
        n = names(def$aliases)
        labels = n[ grep(pattern,n) ]
        if(!is.null(exclude)) {
          labels = labels[ !labels %in% exclude ]
        }
      }
      self$labels = labels
    },

    #' Get list of labels
    #' @param .data only keep labels present in names of this dat
    list = function(.data=NULL) {
      labs = self$labels
      if(!is.null(.data)) {
        labs = labs[labs %in% names(.data)]
      }
      labs
    },
    #' Remove common prefix labels (only works with glob pattern defined labels like 'vacc.flu.*')
    #' @return function to remove prefix, can be use as a labeller function (in ggplot2 for example)
    unprefix=function() {
      if(is.null(self$glob)) {
        rlang::abort("This labeller is not based on glob pattern")
      }
      pattern = glob2rx(self$glob)
      function(x) {
        gsub(pattern, "", x)
      }
    }

  ) # Public
)

#' Get a labeller object for a given name in the survey
#' Will to replace \code{\link{survey_labels}()} returns a \code{\link{SurveyLabeller}} object
#' @family survey
#'
#' @export
survey_labeller = function(survey, name) {
  SurveyLabeller$new(survey, name)
}
