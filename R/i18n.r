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
 if( is.null(language) ) {
   language = .Share$language
 }
 files = list(...)
 for(file in files) {
   if( grepl("/$", file) ) {
    f = swMisc::get_r_file(paste0(file, language))
   } else {
     f = swMisc::get_r_file(paste(file, language, sep='.'))
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
     i18n_set(!!!env)
   }
 }
 invisible()
}

#' Translate a string
#'
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
 for(i in seq_along(x)) {
   v = as.character(x[i])
   x[i] = .Share$i18n$get(v, v)
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
#'
#' return translated string according to count
#' @param count int count
#' @param single single text id if count <= 1
#' @param plural plural text id if count > 1
#' @export
i18n_inflect = function(count, single, plural) {
  ifelse(count > 1, i18n(plural), i18n(single))
}

#' Create a title from a set of character vectors
#'
#' Each argument will be translated and then concatenated in a single string
#' Caution, this function collapse all arguments, and return ONE string
#'
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
#'
#' @param data data structure name to translate
#' @export
i18n_names = function(data) {
  names(data) <- i18n(names(data))
  data
}

#' Register new translations from a list
#' @param ... named value for text to be translated
#' @export
i18n_set = function(...) {
  values = rlang::dots_list(..., .ignore_empty = "all", .homonyms = "error")
  nn = names(values)
  for(i in seq_along(values)) {
    key = nn[i]
    trans = values[[i]]
    if(key == "") {
      rlang::warn(paste("i18n doesnt have name value ", i, " with text ", dQuote(trans)))
    } else {
      .Share$i18n$set(key, trans)
    }
  }
}

