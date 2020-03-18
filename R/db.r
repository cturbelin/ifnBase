## DB Library Wrapper

# Constants in options
# DB_DRIVER = type of DB driver (RODBC,RPostgreSQL, etc)
# DB_DSN = How to connect (string or list, depends on driver)

#' Connect or Reconnect to DB
#' @export
dbConnect <- function() {
  driver = get_option('db_driver')
  dsn = get_option('db_dsn')

  online = !is.null(driver) && !is.null(dsn) && driver %in% c('RPostgreSQL','RODBC', 'RSQLite')

  if(online) {
    init_func = get(paste0("dbConnect.", driver), mode="function", envir = topenv())
    query_func = get(paste0("dbQuery.", driver), mode="function", envir = topenv())

    .Share$dbQuery <- query_func

    init_func(dsn)
  } else {
    message("Unknown driver, using offline mode")
    .Share$dbQuery = function(...) {
      message("Offline driver nothing is done")
    }
  }
}

#' Run a query
#' @param ... query to execute coerced to character vector using paste0
#' @export
dbQuery <- function(...) {
  .Share$dbQuery(...)
}

dbConnect.RPostgreSQL <- function(dsn) {
  if( !requireNamespace("RPostgreSQL") ) {
    rlang::abort("RPostgreSQL needed to use this driver")
  }
  dbiConnect <- DBI::dbConnect

  args = list(
    RPostgreSQL::PostgreSQL()
  )

  for(n in names(dsn)) {
    args[[n]] = dsn[[n]]
  }

 .Share$dbHandle <- do.call(dbiConnect, args)
 .Share$dbHandle
}

dbQuery.RPostgreSQL <- function(..., show.errors = T, dbHandle=NULL) {
  if( is.null(dbHandle) ) {
    dbHandle = .Share$dbHandle
  }
  query = paste0(...)
  if(isTRUE(.Share$debug.query)) {
    debug_query(query)
  }
  stm <- DBI::dbGetQuery(dbHandle, query)
  if( is.data.frame(stm) || (is.logical(stm) && stm )) {
    return( stm )
  }
  # handle error
  .Share$dbLastError <- DBI::dbGetException(dbHandle)
  cat('PgSQL error', "\n")
  cat('query : ', query, "\n")
  str(.Share$dbLastError)
  return(-1)
}

dbConnect.RSQLite <- function(dsn) {
  if( !requireNamespace("RSQLite") ) {
    rlang::abort("RSQLite needed to use this driver")
  }
  dbiConnect <- DBI::dbConnect

  args = list(
    RSQLite::SQLite()
  )

  for(n in names(dsn)) {
    args[[n]] = dsn[[n]]
  }

  .Share$dbHandle <- do.call(dbiConnect, args)
  .Share$dbHandle
}

dbQuery.RSQLite <- function(..., show.errors = T, dbHandle=NULL) {
  if( is.null(dbHandle) ) {
    dbHandle = .Share$dbHandle
  }
  query = paste0(...)
  if(isTRUE(.Share$debug.query)) {
    debug_query(query)
  }
  stm <- DBI::dbGetQuery(dbHandle, query)
  if( is.data.frame(stm) || (is.logical(stm) && stm )) {
    return( stm )
  }
  # handle error
  .Share$dbLastError <- DBI::dbGetException(dbHandle)
  cat('SQLite error', "\n")
  cat('query : ', query, "\n")
  str(.Share$dbLastError)
  return(-1)
}

# Connect to DB
dbConnect.RODBC <- function(dsn) {
  if( !requireNamespace("RODBC") ) {
    rlang::abort("RODBC needed to use this driver")
  }
  .Share$dbLastError = 0
  .Share$dbHandle = NULL
  if( is.list(dsn) ) {
      if(!is.null(dsn$file)) {
        con <- RODBC::odbcDriverConnect(paste0("FILEDSN=", dsn$file))
      }
    } else {
      con <- RODBC::odbcConnect(dsn)
    }
  .Share$dbHandle <- con
  return(con)
}

dbQuery.RODBC <- function(..., show.errors = T, dbHandle=NULL) {
  if( is.null(dbHandle) ) {
    dbHandle = .Share$dbHandle
  }
  query = paste0(...)
  if(isTRUE(.Share$debug.query)) {
    debug_query(query)
  }
  stm <- RODBC::odbcQuery(dbHandle, query)
  if (stm == -1) {
    # handle error
    .Share$dbLastError <- RODBC::odbcGetErrMsg(dbHandle)
    cat('ODBC error',"\n")
    cat('query : ', query, "\n")
    cat(.Share$dbLastError)
    return(-1)
  } else {
    RODBC::sqlGetResults(dbHandle, errors = show.errors)
  }
}

#' @noRd
db_quote_var = function(x) {
  paste0('"', x, '"')
}

#' @noRd
db_quote_str = function(x) {
  paste0("'", x, "'")
}

#' Create an equal criteria
#' @param field name of field (unquoted)
#' @param value value to compare
#' @param quote if TRUE quote the value, NA = detect
#' @param several if TRUE accept to compare several values
db_equal = function(field, value, quote=NA, several=FALSE) {
  if(length(value) == 0) {
    rlang::abort("No value to compare")
  }
  if(is.na(quote)) {
    if(!is.numeric(value)) {
      value = db_quote_str(value)
    }
  }
  if(length(value) > 1) {
    op = paste0(' in(', paste(value, collapse=','), ')')
  } else {
    op = paste0('=', value)
  }
  paste0(db_quote_var(field), op)
}


#' get database connexion handle
#' @export
get_db_handle = function() {
  .Share$dbHandle
}

debug_query = function(query) {
  print(query)
}
