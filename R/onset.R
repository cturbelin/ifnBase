# Onset column computation
#


#' Onset definition used for episodes
#'
#' Create an expression to compute onset design for episodes
#'
#' See \code{\link{compute_onset}}
#' @return quosure
#' @param delay_symptome_max integer max delay to use `fever.start` or `sympt.start` instead of survey date
#' @export
episode_onset_design = function(delay_symptome_max=15) {
  check_int(delay_symptome_max)
  # compute onset expression
  quo(
    dplyr::if_else(!is.na(fever.start) & (date - fever.start) < delay_symptome_max, fever.start,
      dplyr::if_else(!is.na(sympt.start) & (date - sympt.start) < delay_symptome_max, sympt.start, date)
    )
  )
}

#' Base onset design, used for incidence computation
#'
#' See \code{\link{compute_onset}}
#'
#' @return quosude expression used to compute onssed
#'
#' @export
base_onset_design = function() {
  quo(
    dplyr::if_else(!is.na(fever.start), fever.start, dplyr::if_else(!is.na(sympt.start), sympt.start, date))
  )
}

#' Compute the onset date from a design object
#'
#' Onset is the date on which a survey is considered as incident in incidence and episode computation (when the disease started)
#'
#' Evaluate the `design` expression to compute the onset column.
#' The expression is evaluated using weekly data and quosure environment;
#'
#' if design is FALSE, does nothing, return the weekly. This can be used if the `onset` column is already available in
#' the provided weekly data.frame.
#'
#' @param weekly weekly data
#' @param design design object,
#'
#' @return weekly
#' @export
compute_onset = function(weekly, design) {

  if(is.logical(design) && identical(design, FALSE)) {
    return(weekly)
  }

  weekly$onset = rlang::eval_tidy(design, data=weekly, env=rlang::quo_get_env(design))

  weekly
}
