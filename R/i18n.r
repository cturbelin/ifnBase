# Simple Internationlization for R


#' Load i18n translations from a list of files
#'
#' for each file, only the start of the file name should be provided.
#' It is assumed that the translation file follow the pattern :
#'  [file-prefix].[language].r
#'
#' i18n is a list of variables (name is text id) and value is the translation
#'
#' @param ... list of file prefix to load (see description)
#' @param language language
#' @param debug show verbose messages when loading a file
#' @export
i18n_load <- function(..., language=NULL, debug=F) {
 language = ifelse(is.null(language), .Share$language, language)
 files = list(...)
 for(file in files) {
   if( grepl("/$",file) ) {
    f = paste0(file, language,'.r')
   } else {
     f = paste(file, language,'r', sep='.')
   }
   if( !file.exists(f) ) {
     #f = paste(file, i18n_language, 'r', sep='.')
     #warning(sprintf("using default language for i18n '%s'", file))
   }
   if( !file.exists(f) ) {
      warning(sprintf("i18n file '%s' doesnt exist", f))
   } else {
     if(debug) {
      message("loading ",f,"\n")
     }

     env = new.env(parent = baseenv())
     sys.source(file=f, envir = env)

     env = as.list(env)
     if( !is.null(env$i18n) ) {
       # Compat with old i18n files using a list named 'i18n' as root object
       # New should be a list of variables
       # text id should be valid names !
       r = env$i18n
       env = r
     }
     # Update i18n with new ones
     .Share$i18n = merge.list(env, .Share$i18n)
   }
 }
 invisible()
}

#' Translate a string
#' @param x object to translate
#' @return translation or the string itself it not found
#'
#' @usage i18n(x)
#'
#' @examples
#' i18n("my_string_to_translate")
#'
#' @export
i18n <- function(x) {
 UseMethod("i18n")
}

#' @rdname i18n
#' @export
i18n.default <- function(x) {
 n = names(.Share$i18n)
 i = match(x, n)
 f = !is.na(i)
 if( any(f) ) {
	x[ f ] = unlist(.Share$i18n[ i[f] ])
 }
 x
}

#' @rdname i18n
#' @export
i18n.factor = function(x) {
  levels(x) = i18n.default(levels(x))
  x
}

#' Inflexion for a count
#' return translated string according to count
#' @param count int count
#' @param single single text id if count <= 1
#' @param plural plural text id if count > 1
#' @export
i18n_inflect = function(count, single, plural) {
  ifelse(count > 1, i18n(plural), i18n(single))
}

#' Create a title from a set of character vectors
#' Each argument will be translated and then concatenated in a single string
#' Caution, this function collapse all arguments, and return ONE string
#' @param ... list of character string fragments to titlelize
#' @param translate bool use i18n for each fragment
#' @export
titlelize = function(..., translate=T) {
  ll = list(...)
  ll = sapply(ll, function(x) {
    if(translate) {
      x = i18n(x)
    }
    paste(x, collapse=' ')
  })
  ll = paste(ll, collapse=' ')
  n = nchar(ll)
  ll = paste(toupper(substr(ll, 1, 1)), substr(ll, 2, n), sep='')
  ll = gsub(" , ", ", ", ll) # correct comma syntax
  ll = gsub("  ", " ", ll) # correct multiple space
  return(ll)
}

#' translate names attribute
#' @param data data structure name to translate
#' @export
i18n_names = function(data) {
  names(data) <- i18n(names(data))
  data
}