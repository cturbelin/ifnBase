% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph.r
\name{graph.hook}
\alias{graph.hook}
\title{Declare a hook function called when graph.close() is called}
\usage{
graph.hook(fn, name = NULL)
}
\arguments{
\item{fn}{function to run as hook}

\item{name}{unique name for the hook to prevent multiple registration}
}
\description{
Declare a hook function called when graph.close() is called
}
