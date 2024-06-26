% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/survey_loader.R
\name{survey_question_id}
\alias{survey_question_id}
\title{get the question_id (id in the question table in the influenzanet db) of a question}
\usage{
survey_question_id(survey, varname, language = survey_default_language())
}
\arguments{
\item{survey}{name of the survey}

\item{varname}{variable name of the question (db's column name or R alias of the column)}

\item{language}{language translation to use}
}
\value{
int
}
\description{
get the question_id (id in the question table in the influenzanet db) of a question
}
\seealso{
Other survey-load: 
\code{\link{survey_load_all}()},
\code{\link{survey_load_options}()},
\code{\link{survey_load_participants}()},
\code{\link{survey_load_participations}()},
\code{\link{survey_load_questions}()},
\code{\link{survey_load_results}()},
\code{\link{survey_load_results_historic}()},
\code{\link{survey_load_translation}()},
\code{\link{survey_options_for}()},
\code{\link{survey_participant_previous_season}()},
\code{\link{survey_participant_season}()}
}
\concept{survey-load}
