% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/platform.R
\name{merge_by_value}
\alias{merge_by_value}
\title{Merge two named lists by their values (survey recoding)}
\usage{
merge_by_value(new, old)
}
\arguments{
\item{new}{vector or list of values}

\item{old}{vector or list of values}
}
\description{
Internal function to be used for survey recoding definition.
Only keep in old values that are not already in new
}
\keyword{internal}
