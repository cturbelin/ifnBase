# Platform independant ISO 8601 week functions
## Global Convention
## An ISO 8601 is represented as a YearWeek number (YYYYWW) = yearOfTheWeek*100 + weekNumberInTheYear

#' iso_yearweek(dates)
#'
#' Compute ISO 8601 Yearweek number (YYYYWW) from dates (Date class)
#'
#' @param dates Date values
#' @param yearweek value as number ( year of the week * 100 + week number, ex: 200515 is week 15 of 2005)
#' @export
iso_yearweek <- function(dates) {
  as.integer(format(dates, "%G%V"))
}

#' week_of_date(from,to)
#' get yearweek from a date interval date interval
#' @param from date (Date class)
#' @param to Date class
#' @return dataframe(dates, yearweek number, using the format YYYYWW, so Year * 100 + Week in the year)
#' @export
week_of_date <- function(from, to) {
  dates = seq(from = as.Date(from), to = as.Date(to), by = 1)
  weeks = iso_yearweek(dates)
  return(data.frame("date" = dates, "yw" = weeks))
}

#' Monday of the week of the date
#' @param d Date
#' @export
monday_of_date <- function(d) {
  w = as.integer(format(d, format = "%w"))
  w = ifelse(w == 0L, 7L, w) - 1L
  return(d - w)
}

#' Date of the first monday of the year
#' @param year int year
#' @return Date
#' @export
monday_of_year <- function(year) {
  NewYear = as.Date(paste(year, 1L, 1L), format = "%Y %m %d")
  WeekDay = as.integer(format(NewYear, "%w")) - 1 #Generate weekday index where Monday = 0
  WeekDay[WeekDay == -1] <- 6L
  YearStart = NewYear - WeekDay + 7L
  YearStart[WeekDay < 4L] = NewYear[WeekDay < 4L] - WeekDay[WeekDay < 4L]
  return(YearStart)
}

#' Date of the monday of a yearweek
#' @param yw integer yearweek number
#' @return Date
#' @export
monday_of_week <- function(yw) {
  if ( is.factor(yw) ) {
    yw = as.character(yw)
  }
  year = floor(as.integer(yw) / 100L)
  week = as.integer(yw) %% 100L
  d = monday_of_year(year) + ((week - 1L) * 7L)
  return(d)
}

#' Compute a "Week" stamp (like timestamp) : week number from 1970-01-01
#'
#' This provide a continuous index for week number which not depend on period bound (unlike make_week_index)
#' It is usefull to plot week based data.
#' @param yw yearweek value
#' @export
week_stamp <- function(yw) {
  monday = as.integer(monday_of_week(yw)) + 4L
  monday = monday %/% 7L
  class(monday) <- c('numeric', "weekstamp")
  monday
}

#' @noRd
Stamp2Week <- function(stamp) {
  monday = as.Date(stamp * 7L, origin = '1970-01-01')
}

#' Format a yearweek value to a human readable format
#' @param w vector of yearweek values
#' @param sep separator to use
#' @param century use century for year value
#' @export
format_week <- function(w, sep = 's', century = T) {
  if (length(w) == 0) {
    return(character())
  }
  if (is.factor(w)) {
    warning("week number as factor, format_week expect numeric values")
    w = as.character(w)
  }
  w = as.integer(w)
  y = w %/% 100L
  if (!century) {
    y = y %% 100L
  }
  w = sprintf(fmt = "%02d", w %% 100L)
  paste0(y, sep, w)
}

#' Make an index data.frame associating week index (wid) to a iso yearweek number
#' @param yw int vector of yearweek number
#' @param col.yw chr[1] name of the yw column in the resulting data.frame
#' @param col.idx char[1] name of the week index column in the resulting data.frame
#' @param stamp logical use weekstamp instead of order of the week (depends on the range of weeks)
#' @return data.frame(yw=int,wid=int)  yw=yearweek number, wid=index of the yw
#' @export
make_week_index = function(yw,
                         col.yw = 'yw',
                         col.idx = 'wid',
                         stamp = F) {
  ww = unique(yw)
  ww = ww[order(ww)]
  if (stamp) {
    idx = week_stamp(ww)
  } else {
    idx =  1:length(ww)
  }
  ww = data.frame(ww , idx)
  names(ww) <- c(col.yw, col.idx)
  ww
}

#' Compute a season index of each year week
#'
#' For each week compute the number of the week inside the "season", using an arbitrary start date for the season
#' Based on a fixed date in the year (to take account of the week lag for some years)
#' Conventionnaly we took the first of september of each year as the season's start
#'
#' @param inc data.frame() data
#' @param col.yw name of the column containing the yearweek number
#' @param col.stamp name of the column containing the week stamp of each yearweek (@see week_stamp), if not provided compute it
#' @param date.start month-day date to take as the start of each season
#' @param calc.index if TRUE add an index column
#' @return inc with 'season.year' (year of the start ) and 'season.index' (index of the week in the season) columns
#' @export
calc_season_fixed = function(inc,
                             col.yw = 'yw',
                             col.stamp = NULL,
                             date.start = '09-01',
                             calc.index = TRUE) {
  yw = inc[[col.yw]]

  # Find the season for each week
  # based on a fixed date (we use the monday of the week of this date)
  y = floor(yw / 100)
  # Monday of the week of the starting date
  start = monday_of_date(as.Date(paste(y, date.start, sep = '-'), format ="%Y-%m-%d"))
  date = monday_of_week(yw) # Monday of the week of each yearweek

  y = ifelse(date >= start, y, y - 1L) # Year of the season starting for each week

  if (calc.index) {
    start = as.Date(paste(y, date.start, sep = '-'), format = "%Y-%m-%d") # starting of the season for each week

    start = week_stamp(iso_yearweek(start)) # Stamp of the starting of the season

    # week_stamp of the yearweek
    if (!is.null(col.stamp)) {
      stamp = inc[[col.stamp]]
    } else {
      stamp = week_stamp(yw)
    }
    inc[, 'season.index'] = as.integer((stamp - start) + 1) # first index is 1
  }
  inc[, 'season.year'] = y
  inc
}

#' Return the season number of a yearweek or a Date
#'
#' Currently the season is defined from september to august of the next year and the season number is the year of the september month
#' (i.e. the first year of the season).
#'
#' @param d date to compute season number from
#'
#' @export
calc_season = function(d) {
  if("Date" %in% class(d)) {
    m = as.numeric(format(d, "%m"))
    y = as.numeric(format(d, "%Y"))
    return(ifelse(m >= 9, y, y - 1))
  }
  if( is.numeric(d) ) {
    # Yearweek number
    w = d %% 100
    y = floor(d / 100)
    return(ifelse(w >= 37, y, y - 1))
  }
  stop(paste("Not implemented for ", class(d)))
}
