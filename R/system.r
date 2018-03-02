
#' Load a share library
#' @param file(s) name of the library to load (name of the .r file, without extension)
#' @param force bool, if TRUE force the loading of the lib, regardless of the cache
#' @param platform, look for a library in share/platform/[libname].[platform].r
#' @param optional if TRUE dont raise an error if the library doesnt exist. By default an error is thrown.
share.lib <- function(file, force=F, platform=F, optional=F) {
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


