
#' Load a share library
#'
#' This function is only usefull if the project comply to the project organization described in vignette #' \code{vignette("workspace", package = "ifnBase")}
#'
#' @param file character vector of name(s) of the library to load (name of the .r file, without extension)
#' @param force bool, if TRUE force the loading of the lib, regardless of the cache
#' @param platform, look for a library in share/platform/[libname].[platform].R
#' @param optional if TRUE dont raise an error if the library doesnt exist. By default an error is thrown.
#'
#' @export
share.lib <- function(file, force=F, platform=F, optional=F) {

  if( !is.null(.Share$internal.libs) ) {
    ff = file[file %in% .Share$internal.libs]
    if(length(ff) > 0) {
      warnings("Unable to use ", paste(ff, collapse = ', '), 'as share.lib name declared as internal')
      file = file[ !file %in% ff]
    }
  }

  # Check already loaded libs
  if(!force && any(file %in% .Share$loaded.libs)) {
    ff =  paste(file[file %in% .Share$loaded.libs], collapse=',')
    warning(paste(ff, " already loaded"))
  }

  if(platform) {
    lib.path = get_option('platform.path')
    prefix = paste0(get_option('platform'), '/')
  } else {
    lib.path = get_option("share.lib.path")
    prefix = ''
  }
  sapply(file, function(file)  {
      fn = paste0(prefix, file)
      path = get_r_file(paste0(lib.path, fn), should.exists=!optional)
      if(optional && !file.exists(path)) {
        return(invisible())
      }
      source(path)
      .Share$loaded.libs <- unique( c(.Share$loaded.libs, fn) )
  })
  invisible()
}
