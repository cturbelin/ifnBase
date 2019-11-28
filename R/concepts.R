#' Package concepts
#'
#' Common concepts used in this package
#'
#' @section Seasons:
#' Considering the influenza we dont work with calendar year but with shifted year, centered on the winter season
#' Season names are by convention the year of the first septembre of the season
#' For example, season 2017 is from September of 2017 to August 2018.
#'
#' @section Platform:
#' A platform is a set of configurations directives to drive the analysis. It related to an InfluenzaNet platform but can be more
#' extended. The name only refer to the name of the file containing a set of configurations, usually stored in a common directory
#' (in the workspace/project proposed organization it's in share/platform/)
#'
#' For example platform "fr" can be the analysis configuration to analyse France platform's data,
#' "eu" is used to name the configuration for the analysis at the european level.
#'
#'
#' In some case you can have several alternatives platform definitions, each in a platform configuration file, to run analysis on different settings.
#'
#' @section Survey:
#'
#' In this package perspective, a survey is a name associated with a set of parameters, registred in the package
#' A survey has a unique name, known by the package, which whith you can manipulate the data.
#'
#' A survey can be associated with a table name in the database, and variable mapping (to recode database column name to
#' more meaningfull names), and a value to label mapping (recoding a value in the database to a label)
#'
#' The package also provide two survey templates for weekly and intake survey (see \code{\link{survey_template}}) respectively
#' named 'eu:weekly' and 'eu:intake'.
#'
#' A survey is a set of variables, but here `question` is used with the same meaning of "variable", one
#' question is, for this package, a variable with a set of response value (each one is a survey response from a participant).
#' The survey is defined by the set of output variables (not in the way it has been presented to the user).
#'
#' Some variables can be related to the same question (for example in case of multiple choice), this is handled by defining
#' a set of labels in the survey (list of variable names or grob like pattern).
#'
#' @name concepts
NULL
