# Path helpers  xxx.path(file) return file in the given path for xxxx

to_path = function(path_name, file) {
  path = get_option(path_name)

}

#' Get file in lib path
#' @family path-functions
#' @param file file name to load
#' @export
share.lib.path <- function(file) {
  paste0(get_option("share.lib.path"), file)
}

#' return the path of a file in cache files
#' @family path-functions
#' @param file file name to load
#' @export
share.cache.path <- function(file='') {
  paste0(get_option('share.cache'), file)
}

#' path of a file in data
#' @family path-functions
#' @param file file name to load
#' @export
share.data.path <- function(file='') {
  paste0(get_option('share.data'), file)
}

#' path of a file in share/ directory
#' @family path-functions
#' @param file file name to load
#' @export
share_path <- function(file='') {
 paste0(get_option('share.path'), file)
}

#' @rdname share_path
#' @export
share.path = share_path

#' Path to platform definition files
#'
#' Platform files are located in the path defined by the option "platform.path" (see \code{\link{share.option}})
#' Usually in share/platform in the root of the workspace (see \code{\link{concepts}}).
#'
#' @family path-functions
#' @param file character file name to get in the 'platform' directory
#' @param platform logical if TRUE add platform id to the path
#'
#' @return character path of the file in platform files location.
#'
#' @export
platform_path = function(file='', platform=FALSE) {
  path = get_option('platform.path')
  if(platform) {
    path = paste0(path, share.option("platform"), '/')
  }
  paste0(path, file)
}
