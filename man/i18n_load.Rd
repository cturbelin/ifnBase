% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/i18n.r
\name{i18n_load}
\alias{i18n_load}
\title{Load i18n translations from a list of files}
\usage{
i18n_load(..., language = NULL, debug = F)
}
\arguments{
\item{...}{list of file prefix to load (see description)}

\item{language}{language}

\item{debug}{show verbose messages when loading a file}
}
\description{
for each file, only the start of the file name should be provided.
It is assumed that the translation file follow the pattern :
 [file-prefix].[language].r
}
\details{
i18n is a list of variables (name is text id) and value is the translation
}
