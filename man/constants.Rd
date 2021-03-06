% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recode.r
\docType{data}
\name{constants}
\alias{constants}
\alias{YES}
\alias{NO}
\alias{DONTKNOW}
\alias{YES_NO_DNK}
\title{Constants values}
\format{
An object of class \code{character} of length 1.

An object of class \code{character} of length 1.

An object of class \code{character} of length 1.

An object of class \code{character} of length 3.
}
\usage{
YES

NO

DONTKNOW

YES_NO_DNK
}
\description{
Some constants are provided to limit error by using some common values in programs
They are defined using UPPERCASE names to be recoginizable as constants
}
\details{
Beware to never redefine these objects in your code, or use ifnBase:: prefix to be sure of the value used.

\describe{
\item{YES}{is 'Yes' level used to recode many variables}
\item{NO}{is 'No' level}
\item{DONTKNOW}{'DNK' Dont know level}
}
}
\keyword{datasets}
