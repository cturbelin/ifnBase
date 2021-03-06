% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seasons.R
\name{season_definition}
\alias{season_definition}
\alias{season.def}
\title{Get historical season definition}
\usage{
season_definition(season, silent = F)

season.def(season, silent = F)
}
\arguments{
\item{season}{season definition to get}

\item{silent}{boolean (not used.)}
}
\value{
return the entry of config for the season (first year of the season)
}
\description{
Each season should be described in the platform using \code{\link{platform_season_history}}
historical.tables is a list with an entry for each season (season = year number of the first september in the season)
\describe{
 \item{intake}{table containing the intake survey data for the season}
 \item{weekly}{table containing the weekly survey data for the season}
 \item{health}{table/view containing the health status for each weekly for the season}
 \item{year.pop}{population year to use}
 \item{dates}{list(start, end), starting and ending date of the season}
}
This entries are defined
}
\seealso{
Other seasons: 
\code{\link{get_historical_seasons}()},
\code{\link{get_historical_tables}()},
\code{\link{get_season_dates}()}
}
\concept{seasons}
