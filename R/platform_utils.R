# Utility function to defined platform files



#' create a recoding alias
#'
#' The recoding will be proceeded using the name provided. This avoid to copy paste the recode mapping
#' by reusing the one defined in the same survey.
#'
#' @param name name of the variable to use the recoding from
#' @export
recode_alias = function(name) {
  structure(name, class="recode_alias")
}

#' Allow override of mapping or recoding from template
#' @param data value to flag as overrided
#' @export
override = function(data) {
  attr(data, "allow_override") <- TRUE
  data
}
