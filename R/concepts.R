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
#' In this package perspective, a survey is a name associated with a set of parameters, registered to the package (using the platform file) by \code{\link{platform_define_survey}()}
#' A survey has a unique name, known by the package, you use to request and manipulate the data.
#'
#' A survey can be associated with a table name in the database (for the package to know fromwhere to fetch the data), and variable mapping (to recode database column name to
#' more meaningful names), and a recoding mapping ('recodes' entry, recoding a value in the database to an explicit  label). It can also have a set of 'labels' (see below)
#' to handle variable user defined variables lists.
#'
#' The package also provide two survey templates for weekly, intake, and vaccination survey (see \code{\link{survey_template}()}) respectively
#' named 'eu:weekly' and 'eu:intake', 'eu:vaccination'.
#'
#' For this package, a survey is a set of variables, some variables can be related to the same question in the survey (for example in case of multiple choice), the survey labels can
#' be used to handle this list (and do not repeat everywhere the harcoded list of variables names).
#'
#' A survey label is a named list of variable names (as character vector). When creating a survey definition they can be defined as an explicit list (all the names given) or implicitly
#' by using a glob pattern (like "vac.flu.*'). In this last case, the pattern will be used to create the full list using the available variables in the survey definition.
#' The function helper \code{\link{var_labels}()} can be used to defined a pattern with some exclusion (if all variables names matching the pattern are not to be included in the list)
#'
#' See \code{vignette("surveys", "ifnBase")}
#'
#' @section Participants:
#'
#' A participant is an individual subject of a survey response (either by filling by himself the survey or delegate to someone else). Each
#' survey response is connected to a participant (with the "global_id" column in the database).
#' This package doesnt use global_id but match it with survey_user table id, assigning one numerical id to each participant.
#'
#' An influenzanet user, is not a participant but an individual connected to an account, and several participant can be registered in one account
#' (the set of participants of an account is often called "household").
#'
#' @name concepts
NULL
