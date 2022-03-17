# Utility functions (not exported)

#' Flip names and values in a vector
#'
#' @noRd
flip.names <- function(x) {
  n = names(x)
  names(n) <- as.vector(x)
  n
}

#' Check if a value is integer value (by default positive)
#' @param value value to test
#' @param min minimal value
#' @noRd
check_int = function(value, min=1) {
  name = deparse(substitute(value))
  if(!is.numeric(value)) {
    rlang::abort(paste(name, "should be a numeric"))
  }

  value = as.integer(value)
  if(!is.na(min)) {
    if(value <= min) {
      rlang::abort(paste(name,"should be number over min"))
    }
  }
  value
}

with_abort <- function(expr) {
  if(packageVersion("rlang") >= numeric_version("1.0")) {
    rlang::try_fetch(
      expr,
      simpleError = function(cnd) {
        rlang::abort(
          conditionMessage(cnd),
          call = conditionCall(cnd)
        )
      }
    )
  } else {
    rlang::with_abort(expr)
  }
}
