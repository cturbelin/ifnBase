# Utility functions (not exported)

#' @noRd
flip.names <- function(x) {
  n = names(x)
  names(n) <- as.vector(x)
  n
}

#' @noRd
#' Check if a value is integer value (by default positive)
#' @param value value to test
#' @param min minimal value
check_int = function(value, min=1) {
  name = deparse(substitute(value))
  if(!is.numeric(value)) {
    stop(paste(name, "should be a numeric"))
  }

  value = as.integer(value)
  if(!is.na(min)) {
    if(value <= min) {
      stop(paste(name,"should be number over min"))
    }
  }
  value
}
