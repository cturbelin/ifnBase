#' InfluenzaNet Analysis Base Package
#' @name ifnBase
#' @docType package
NULL

#' .Share is a local configuration envirnment holding local values
#' And platform specific stuffs
#' @noRd
.Share = new.env()

.Share$i18n = new.env(parent = emptyenv())

#' Get package option from name
#' From 'ifn' options() entry
#' @seealso output_options
#' @export
get_option <- function(name = NULL) {
  o = getOption("ifn")
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
#' \describe{
#'  \item{graph}{ named list with default graph options list(width, height, type)}
#'  \item{platform}{unique id of the platform, ISO name of the country and 'eu' for european}
#'  \item{platform.path}{path to platform definition files}
#'  \item{autoload.platform}{if TRUE launch load_platform() when package is loaded}
#'  \item{DB_DRIVER}{name of the DB driver to use. Actually 'RPostgreSQL' or 'RODBC'}
#'  \item{DB_DSN}{Database connexion string (or list). for ROBDC list with 'file' entry will load DSN from the file}
#'  \item{base.out.path}{output path used by default init.path function}
#' }
#'
#'
#' @param ... list of values to get or set
#' @return list()
#' @export
share.option <- function(...) {
  opts = list(...)
  oo = getOption("ifn")
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
  base::options("ifn" = oo)
}

#
