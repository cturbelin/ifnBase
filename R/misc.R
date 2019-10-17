#' Wrapper of message() for output
#' You car redirect by using sink() function
#' @param ... message (multiple will be concatenated)
#' @param ln add new line
#' @param level level if hierarchical levels handled (ignored if not)
#' Deprecated
#' @export
msg = function(..., ln=TRUE, level = 0) {
  message(..., appendLF = ln)
}

#' Test if x is an error class from try
#' @param x object to test
#' @export
is.error <- function(x) {
  is(x, "try-error")
}

#' Replace names with given pairlist where value are old names and key are new names
#'
#' replace.names(data.frame(), new.name=old.name,...)
#' ex replace.names(d, toto="x") replace column "x" by "toto"
#'
#' @param x data.frame
#' @param ... named list of column to rename (names are new names to assign)
#' @export
replace.names <- function(x, ...) {
  nn = list(...)
  if(is.list(nn[[1]])) {
    nn = nn[[1]]
  }
  if( !is.list(nn) ) {
    stop("replacements should be a list")
  }
  old.names = names(x)
  new.names = names(nn) # new names are key of args
  u = match(nn, old.names)
  if( any(is.na(u)) ) {
    warning(paste('unknown columns ', paste(nn[is.na(u)],collapse=',')))
  }
  f = match(old.names, nn)
  i = !is.na(f) # NA= unknown old names in new names
  f = f[i]
  if(length(f) > 0) {
    old.names[i] = new.names[f]
    names(x) <- old.names
  } else {
    warning("nothing to replace")
  }
  return(x)
}

#' Merge two lists (x + y not in x)
#'
#' Missing entries in x will be taken from y
#'
#'
#' from RCurl 0.94.0
#' @param x primary list to update
#' @param y secondary list, only entries not in x will be used
#' @export
merge.list <- function(x, y)
{
  if(length(x) == 0)
    return(y)

  if(length(y) == 0)
    return(x)

  i = match(names(y), names(x))
  i = is.na(i)
  if(any(i))
    x[names(y)[which(i)]] = y[which(i)]
  x
}

#' Get a random string of size n
#' Warning not for cryptographic use
#' @param n length of string to generate
#' @export
random_string = function(n) {
  paste0(sample(c(0:9, LETTERS, letters), n, replace=TRUE), collapse = '')
}


#' Return available columns in data.frame
#' @param data data.frame
#' @param columns list of column to extract
#' @param error if TRUE raise error if any non existent column, warning only if FALSE
#' @export
select_columns = function(data, columns, error=FALSE) {
  n = names(data)
  missing = columns[!columns %in% n]
  if(length(missing) > 0) {
    msg = paste("Missing columns:", paste(missing, collapse = ','))
    if(error) {
      stop(msg)
    }
    warning(msg)
  }
  columns = columns[columns %in% n]
  columns
}

#' Select column from a data.frame,
#' @param data data.frame
#' @param columns list of column to extract
#' @param error raise error if unknown column
#' @return data.frame
#' @export
select_df <- function(data, columns, error=FALSE) {
  columns = select_columns(data, columns, error=error)
  data[, columns, drop=FALSE]
}

#' get file name with good extension .r or .R
#' @noRd
#' @noMd
get_r_file = function(file, should.exists=FALSE) {
  f = paste0(file,".R")
  if( file.exists(f)) {
    return(f)
  }
  f = paste0(file,".r")
  if(should.exists && !file.exists(f)) {
    stop(paste0(f, "not found"))
  }
  return(f)
}

#' Ensure path has an ending /
ending_slash = function(x) {
  paste0(x,ifelse(grepl("/$", x),"","/"))
}

