% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/platform.R
\name{platform_env}
\alias{platform_env}
\title{Get the platform env}
\usage{
platform_env(name = NULL)
}
\arguments{
\item{name}{name of the value to get in the platform envirnoment}
}
\value{
if name is NULL, returns the full environment, if not get the named element in the environment

Platform environment holds platform variables & definition structure.
}
\description{
Get the platform env
}
\seealso{
Other platfom: 
\code{\link{platform_geographic_levels}()},
\code{\link{platform_geographic_tables}()},
\code{\link{platform_options}()},
\code{\link{platform_season_history}()}
}
\concept{platfom}
