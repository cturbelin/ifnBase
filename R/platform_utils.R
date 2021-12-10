# Utility function to defined platform files

#' create a recoding alias
#'
#' The recoding will be proceeded using the name provided. This avoid to copy paste the recode mapping
#' by reusing the one defined in the same survey.
#'
#' @param name name of the variable to use the recoding from
#' @family platform-helpers
#' @export
recode_alias = function(name) {
  structure(name, class="recode_alias")
}

#' Allow override of mapping or recoding from template
#' @param data value to flag as overrided
#' @family platform-helpers
#' @export
override = function(data) {
  attr(data, "allow_override") <- TRUE
  data
}

#' Define availability of variables
#' @param value database name
#' @param seasons either a vector of season or an rlang::quosure (see details)
#'
#' @details
#' If seasons is a quosure it will be evaluated in the survey_load_results environment.
#' It must be evaluate as a single logical result
#' Especially the expression can evaluate the season
#'
#' @family platform-helpers
#' @export
variable_available = function(value, seasons) {
  if(is.vector(seasons)) {
    seasons = parse_season(seasons, accept.several=TRUE)
  } else {
    if(!rlang::is_quosure(seasons)) {
      rlang::abort("seasons must be seasons number list or a rlang::quosure")
    }
    r = rlang::eval_tidy(seasons, data=list(season=2012, country="FR"))
    if(!is.logical(r) && length(r) == 1) {
      rlang::abort(paste("variable_available  for ",sQuote(value),"must be evaluated as ONE logical value"))
    }
  }
  attr(value, "available") <- seasons
  value
}

#' Define a label for survey variables, labels are used to identify a subset of variables using a name
#'
#' The name is useable with \code{\link{survey_labels}} to get the list of the variables, see Survey section of \code{\link{concepts}}
#'
#' A label can be defined as a list of variables names or as a glob pattern (like 'myvar.*') if variables
#' are named using a convenient naming convention.
#' @param labels list of label or glob pattern to identify variables matching the list
#' @param exclude list of names to exclude after creating the list with a pattern
#' @family platform-helpers
#' @return character
var_labels = function(labels, exclude=NULL) {
  if(!is.null(exclude)) {
    attr(labels, "exclude") <- exclude
  }
  labels
}
