% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recode.r
\name{recode_weekly}
\alias{recode_weekly}
\title{Apply common recoding for weekly data}
\usage{
recode_weekly(
  weekly,
  health.status = TRUE,
  recode.temp = FALSE,
  all.variables = FALSE
)
}
\arguments{
\item{weekly}{data.frame weekly data (return by survey_load_results)}

\item{health.status}{if TRUE try to get the health status for each row as stored in the db.}

\item{recode.temp}{recode temperature variable (need to be false for incidence), if TRUE recode temperature variable}

\item{all.variables}{if TRUE all known variables will be recoded}
}
\value{
weekly data.frame with recoded values & extra column
}
\description{
Variable recoding (from database value to label) are defined in the survey description in the platform
definition file using
}
\seealso{
\code{\link{survey_load_health_status}}
}
