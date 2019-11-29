# Path functions are helpers
# To standardize R projects regardless of installation

#' Define an path for files, usable by my.path()
#'
#' \code{init.path()} and \code{\link{my.path}()} are dedicated to manage path to files used by scripts to make them independant from the
#' actual physical location of the files, which depends on where (on which machine, account) the script is running.
#'
#' by default, the path is added to the global output path (`base.out.path` option, see \code{\link{share.option}} ),
#' unless the parameter full.path is TRUE
#' @param p chr path
#' @param full.path logical if TRUE p is considered as an absolute path, replace all the current path
#' @family path-functions
#' @export
init.path <- function(p, full.path=F) {
  if(isTRUE(full.path)) {
    path = p
    attr(path, "full") <- TRUE
    .Share$path.suffix = NULL
  } else {
    # get output path
    .Share$path.suffix = p
    path = NULL
  }
  update_out_path(path)
  return(path)
}

update_out_path = function(path=NULL) {
  if(is.null(path)) {
    path = create_path()
  }
  .Share$out.path <- path # update current output path
  .Share$out.path.sep <- ifelse( any(grep("/$", path) == 1) , "", "/") # check for directory separator
  if( !file.exists(path) ) {
    dir.create(path, recursive=T)
  }
  return(path)
}

is_out_full_path = function() {
  isTRUE(attr(.Share$out.path, "full"))
}


#' Register a path prefix
#'
#' Each path prefix will be added to `base.out.path` separated by '/' to create a full path for output
#'
#' Prefix mechanism allow to define constant path at several levels.
#' For example :
#' output path for global workspace (base.out.path)
#' output path for a project (that will complete base.out.path)
#' output path in a script of the project
#'
#' @examples
#' # base.out.path = '/my/path/'
#' add_path_prefix('project', 'my_project') # A project prefix to the path
#' \dontrun{
#' init.path('output') # Add output to the current path
#' }
#' my.path() # => '/my/path/my_project/output'
#' @export
#'
#' @family path-functions
#'
#' @param name name of the prefix
#' @param prefix path to add to the prefix
add_path_prefix = function(name, prefix) {
  .Share$path.prefix[[name]] <- prefix
  if(is_out_full_path()) {
    rlang::warn("Current path has been defind by full path, wont override it.")
  } else {
    update_out_path()
  }
}

#' Internal function create the path from the current paths prefix & suffix
create_path = function() {
  path = get_option('base.out.path')
  if(length(.Share$path.prefix) > 0) {
    path = paste0(path, paste0(.Share$path.prefix, collapse='/'))
  }
  path = ending_slash(path)
  if(length(.Share$path.suffix) > 0) {
    path = paste0(path, .Share$path.suffix)
  }
  path = ending_slash(path)
  path
}

#' Get the current paths defined by \code{\link{init.path}()} or \code{\link{add_path_prefix}()}
#' @family path-functions
#' @export
get_current_paths = function() {
 paths = list(
   base = get_option('base.out.path'),
   prefixes = .Share$path.prefix,
   suffix = .Share$path.suffix,
   resolved = my.path()
 )
 structure(paths, class="paths_definition")
}

#' Print path definition
#'
#' Print the path definitions return by \code{\link{get_current_paths}}
#'
#' @family path-functions
#' @param x paths_definitions object
#' @param ... extra params (print interface)
#' @export
print.paths_definition = function(x, ...) {
 cat("Registred paths\n")
 cat(" - base = ", x$base ,"\n")
 r = 'base'
 if(length(x$prefixes)) {
   Map(function(name, p) {
     cat(" -", name, "=", p,"\n")
   }, names(x$prefixes), x$prefixes)
   r = c(r, names(x$prefixes))
 }
 if(hasName(.Share, "path.suffix")) {
   cat(" - suffix = ", .Share$path.suffix, " (last value passed to init.path())\n")
   r = c(r, 'suffix')
 }
 if(is_out_full_path()) {
   cat("(!) Full path has been used at last init.path() call, paths config is ignored\n")
 } else {
   cat("Resolved by : ", paste(paste0("[",r,"]"), collapse=' / '), "\n"  )
 }
 cat("Current : ", x$resolved,"\n")
 cat("\n")
}

#' Return the path of a file in the current ouput path
#' @family path-functions
#' @param ... characters string to used (will be concatenated)
#' @export
my.path <- function(...) {
  paste0(.Share$out.path, .Share$out.path.sep, ...)
}

# Path helpers  xxx.path(file) return file in the given path for xxxx

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
#'
#' @return character path of the file in platform files location.
#'
#' @export
platform_path = function(file='') {
  paste0(get_option('platform.path'), file)
}
