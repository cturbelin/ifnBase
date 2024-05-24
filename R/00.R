#' InfluenzaNet Data Analysis Base Package
#'
#' This package provides base set of function to help & standardize analysis of an
#' InfluezaNet database.
#' It can be used to produce online data analysis or for research projects.
#'
#' @importFrom grDevices dev.off rainbow png postscript
#' @importFrom graphics axTicks axis polygon rect segments points
#' @importFrom methods is
#' @importFrom stats aggregate as.formula
#' @importFrom utils glob2rx str tail hasName packageVersion
#' @importFrom swMisc merge_list ending_slash get_r_file
#' @importFrom rlang abort `:=`
#' @importFrom R6 R6Class
#' @import dplyr
#' @import ggplot2
"_PACKAGE"

#' .Share is a local configuration environment holding local values
#' And platform specific stuffs
#' @noRd
.Share = new.env()

# Hold translations
.Share$i18n = list()

#' Get package option from name
#' From 'ifn' options() entry
#' @param name option name, if missing, all options are returned
#' @seealso share.option
#'
#' @export
get_option <- function(name = NULL) {
  o = base::getOption("ifn")
  if (is.null(name)) {
    o
  } else {
    o[[name]]
  }
}


#' get options for the ifn environment
#'
#' Options for the package are stored in an options() entry named 'ifn'. It should be a list
#'
#' Packages options describe the running package environment
#'
#' \describe{
#'  \item{graph}{ named list with default graph options list(width, height, type)}
#'  \item{platform}{unique id of the platform, ISO name of the country and 'eu' for european}
#'  \item{platform.path}{path to platform definition files}
#'  \item{autoload.platform}{if TRUE launch load_platform() when package is loaded}
#'  \item{db_driver}{name of the DB driver to use. Actually 'RPostgreSQL' or 'RODBC'}
#'  \item{db_dsn}{Database connexion string (or list). for ROBDC list with 'file' entry will load DSN from the file}
#'  \item{base.out.path}{output path used by default init.path function}
#' }
#'
#'
#' @param ... list of values to get or set
#' @return list()
#' @export
share.option <- function(...) {
  opts = list(...)
  oo = base::getOption("ifn")
  if (is.null(oo)) {
    oo = list()
  }
  if (length(opts) == 0) {
    return(oo)
  }
  nn = names(opts)
  # No names, then list of opts to return
  if (is.null(nn)) {
    if (length(opts) == 1) {
      return(oo[[unlist(opts)]])
    }
    return(oo[unlist(opts)])
  }
  # Value with names, replace
  for (i in seq_along(opts)) {
    n = nn[i]
    o = opts[[i]]
    oo[[n]] <- o
  }

  if("base.out.path" %in% nn) {
    oo$base.out.path = ending_slash(oo$base.out.path)
  }
  base::options("ifn" = oo)
}

utils::globalVariables(c('person_id', 'timestamp'))

