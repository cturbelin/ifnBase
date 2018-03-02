## DB Library Wrapper

# Constants in options
# DB_DRIVER = type of DB driver (RODBC,RMysQL, etc)
# DB_DSN = How to connect (string or list, depends on driver)

#' Connect or Reconnect to DB
#' @export
dbConnect <- function() {
  driver = get_option('db_driver')
  dsn = get_option('db_dsn')

  online = !is.null(driver) && !is.null(dsn) && driver %in% c('RPostgreSQL','RODBC')

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
#' @param ... query to execute
#' @export
dbQuery <- function(...) {
  .Share$dbQuery(...)
}

dbConnect.RPostgreSQL <- function(dsn) {
  library(RPostgreSQL)
  dbiConnect <- DBI::dbConnect
 .Share$dbHandle <- dbiConnect(PostgreSQL(), user=dsn$user, password=dsn$password,host=dsn$host, dbname=dsn$dbname)
 .Share$dbHandle
}

dbQuery.RPostgreSQL <- function(..., show.errors = T, dbHandle=NULL) {
  if( is.null(dbHandle) ) {
    dbHandle = .Share$dbHandle
  }
  query = paste0(...)
  stm <- dbGetQuery(dbHandle, query)
  if( is.data.frame(stm) || (is.logical(stm) && stm )) {
    return( stm )
  }
  # handle error
  .Share$dbLastError <- dbGetException(dbHandle)
  cat('PgSQL error', "\n")
  cat('query : ', query, "\n")
  str(.Share$dbLastError)
  return(-1)
}

# Connect to DB
dbConnect.RODBC <- function(dsn) {
  library(RODBC)
  .Share$dbLastError = 0
  .Share$dbHandle = NULL
  if( is.list(dsn) ) {
      if(!is.null(dsn$file)) {
        con <- odbcDriverConnect(paste0("FILEDSN=", dsn$file))
      }
    } else {
      con <- odbcConnect(dsn)
    }
  .Share$dbHandle <- con
  return(con)
}

dbQuery.RODBC <- function(..., show.errors = T, dbHandle=NULL) {
  if( is.null(dbHandle) ) {
    dbHandle = .Share$dbHandle
  }
  query = paste0(...)
  stm <- odbcQuery(dbHandle, query)
  if (stm == -1) {
    # handle error
    .Share$dbLastError <-odbcGetErrMsg(dbHandle)
    cat('ODBC error',"\n")
    cat('query : ', query, "\n")
    cat(.Share$dbLastError)
    return(-1)
  } else {
   sqlGetResults(dbHandle, errors = show.errors)
  }
}
