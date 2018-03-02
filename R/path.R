# Path functions are helpers
# To standardize R projects regardless of installation

#' Define an ouput path for files, usable by my.path()
#' by default, the path is added to the global output path (defined in OUT_PATH), unless the parameter full.path is TRUE
#' @param char p path
#' @param logical full.path, if TRUE p is considered as an absolute path
#' @family path functions
#' @export
init.path <- function(p, full.path=F) {
  base.path = get_option('base.out.path')
  path = ifelse(as.logical(full.path), p, paste0(base.path, p))
  .Share$out.path <- path # update current output path
  .Share$out.path.sep <- ifelse( any(grep("/$", path) == 1) , "", "/") # check for directory separator
  if( !file.exists(path) ) {
    dir.create(path, recursive=T)
  }
  return(path)
}

#' Return the path of a file in the current ouput path
#' @family path functions
#' @export
my.path <- function(...) {
  paste0(.Share$out.path, .Share$out.path.sep, ...)
}

# Path helpers  xxx.path(file) return file in the given path for xxxx

#' Get file in lib path
#' @param file file name to load
#' @export
share.lib.path <- function(file) {
  paste0(get_option("share.lib.path"), file)
}

#' return the path of a file in cache files
#' @param file file name to load
#' @export
share.cache.path <- function(file='') {
  paste0(get_option('share.cache'), file)
}

#' path of a file in data
#' @param file file name to load
#' @export
share.data.path <- function(file='') {
  paste0(get_option('share.data'), file)
}

#' path of a file in share/ directory
#' @param file file name to load
#' @export
share.path <- function(file='') {
 paste0(get_option('share.path'), file)
}

platform_path = function(file='') {
  paste0(get_option('platform.path'), file)
}