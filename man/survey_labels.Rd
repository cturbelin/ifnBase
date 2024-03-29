% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/survey.r
\name{survey_labels}
\alias{survey_labels}
\title{Return the "labels" for a given question variable name}
\usage{
survey_labels(survey, question)
}
\arguments{
\item{survey}{name of the survey}

\item{question}{name of the question variable, or if multichoice name of the question group (sometimes a more generic name)}
}
\value{
character vector of names
}
\description{
If it is a multichoice question, it will return the names of variable containing all the responses choices for this question
If it is a single choice question, it will return a list of language independent labels (but human meaningful)
}
\details{
These labels are defined in the survey  by \code{\link{platform_define_survey}} in the platform file
}
\seealso{
Other survey: 
\code{\link{SurveyLabeller}},
\code{\link{survey_aliases}()},
\code{\link{survey_definition}()},
\code{\link{survey_labeller}()},
\code{\link{survey_recode}()},
\code{\link{survey_recodings}()},
\code{\link{survey_rename_columns}()},
\code{\link{survey_variable_recoding}()},
\code{\link{survey_variables_like}()}
}
\concept{survey}
