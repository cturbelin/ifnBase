% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/survey.r
\name{keep_last_survey}
\alias{keep_last_survey}
\title{Keep the last survey for each participant}
\usage{
keep_last_survey(data)
}
\arguments{
\item{data}{data.frame with at least (timestamp,person_id, id) columns}
}
\description{
Some survey could have exactly the same timestamp
So using this function is safer than only using timestamp
}
