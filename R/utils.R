# Utility functions (not exported)

#' @noRd
flip.names <- function(x) {
  n = names(x)
  names(n) <- as.vector(x)
  n
}
