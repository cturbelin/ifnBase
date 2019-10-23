# Statistical utilities
#

#' Compute frequency table for multiple options matrix
#' @param .data data.frame with multiple values
#' @param digits number of digits to keep in
#' @param na.rm logical remove NA if true
#' @param translate use `i18n()` on labels if TRUE
#' @return data.frame() with count and prop columns
#' @export
multiple_freq = function(.data, digits=0, na.rm=T, translate=T) {
  ii = apply(.data, 2, sum, na.rm=na.rm)
  ii = ii[ order(ii, decreasing=T) ]
  iip = round(100 * ii / nrow(.data), digits)
  ii = rbind(count=ii, prop=iip)
  if(translate) {
    colnames(ii) <- i18n(colnames(ii))
  }
  class(ii) <- "mfreq"
  return( ii )
}


#' Frequency for multiple options by group
#' @param .data data.frame
#' @param vv list of multiple options column name
#' @param group group for aggregation for each option
#' @return list(freq, prop)
#' @export
multiple_xfreq = function(.data, vv, group) {
  agg = .data[, group, drop=FALSE]
  ii = aggregate(as.list(.data[, vv]), agg, sum)
  total = apply(ii[, vv], 2, sum)
  prop = ii
  for(v in vv) {
    prop[, v] = 100 * ii[,v] / total[v]
  }
  structure(
    list(freq=ii, prop=prop),
    class="multiple_xfreq"
  )
}
