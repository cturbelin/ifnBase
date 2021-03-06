% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats.R
\name{multiple_freq}
\alias{multiple_freq}
\title{Compute frequency table for multiple options matrix}
\usage{
multiple_freq(.data, digits = 0, na.rm = T, translate = T)
}
\arguments{
\item{.data}{data.frame with multiple values}

\item{digits}{number of digits to keep in}

\item{na.rm}{logical remove NA if true}

\item{translate}{use `i18n()` on labels if TRUE}
}
\value{
data.frame() with count and prop columns
}
\description{
Compute frequency table for multiple options matrix
}
