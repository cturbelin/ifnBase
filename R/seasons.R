
#' Get historical season definition
#'
#' Each season should be described in the platform using \code{\link{platform_season_history}}
#' historical.tables is a list with an entry for each season (season = year number of the first september in the season)
#' \describe{
#'  \item{intake}{table containing the intake survey data for the season}
#'  \item{weekly}{table containing the weekly survey data for the season}
#'  \item{health}{table/view containing the health status for each weekly for the season}
#'  \item{year.pop}{population year to use}
#'  \item{dates}{list(start, end), starting and ending date of the season}
#' }
#' This entries are defined
#' @param season season definition to get
#' @param silent boolean (not used.)
#' @family seasons
#' @return return the entry of config for the season (first year of the season)
#' @export
season_definition = function(season, silent=F) {
  h = .Share$historical.tables[[as.character(season)]]
  if( is.null(h) ) {
    rlang::abort(paste("Unknown season", season,"in historical tables"))
  }
  h
}

#' @export
#' @rdname season_definition
season.def <- season_definition

#' Get list of available season names
#'
#' @seealso \code{\link{concepts}}
#'
#' @family seasons
#' @return character vector of season names
#' @export
get_historical_seasons = function() {
  names(.Share$historical.tables)
}


#' Get Historical tables
#'
#' Historical tables is a list, with season year (first year of the season) as name and season definition
#' @seealso Season in \code{\link{concepts}}
#'
#' @family seasons
#' @seealso season.def
#' @export
get_historical_tables <- function() {
  .Share$historical.tables
}

#' Get dates of the season
#' @family seasons
#' @export
get_season_dates = function(season) {
  def = season_definition(season)
  d = def$dates
  d$start = as.Date(d$start)
  if(is.null(d$end) | is.na(d$end)) {
    d$end = Sys.Date()
  }
  d
}

#' Check if input is a season number
#' @param season value to check (number or character)
#' @return integer value of season number
#' @keywords internal
parse_season = function(season, accept.several=FALSE) {
  s = as.integer(season)
  if(!accept.several && length(season) > 1) {
    rlang::abort("Single season value expected")
  }

  if(any(is.na(s))) {
    rlang::abort("season must be a number")
  }
  ss = s < 2003
  if(any(ss)) {
    rlang::abort(paste0("season must be a year from 2000 to present year, given ",paste(s[ss], collapse = ',')))
  }
  max.season = calc_season(Sys.Date())
  ss = s > max.season
  if(any(ss)) {
    rlang::abort(paste0("season must be a year from 2000 to present year, given ",paste(s[ss], collapse = ',')))
  }
  s
}
