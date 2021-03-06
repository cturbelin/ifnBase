% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/episodes.R
\name{episode_compute_ecollan}
\alias{episode_compute_ecollan}
\title{Compute Episodes using Marie Ecollan's strategy}
\usage{
episode_compute_ecollan(weekly, syndrome.column, params, .progress = NULL)
}
\arguments{
\item{weekly}{data.frame of weekly data}

\item{syndrome.column}{name of the syndrome column to use}

\item{params}{episode_design}

\item{.progress}{progress_estimated style progress bar, nothing if NULL}
}
\description{
Compute Episodes using Marie Ecollan's strategy
}
\seealso{
Other episodes
ww : all weekly of one person_id ordered by date (!! Only one weekly by date)
.keys = dummy variable needed by dplyr::group_map(): 
\code{\link{episode_compute_souty}()}
}
\concept{episodes
ww : all weekly of one person_id ordered by date (!! Only one weekly by date)
.keys = dummy variable needed by dplyr::group_map()}
