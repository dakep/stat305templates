% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/question-types.R
\name{mcquestion}
\alias{mcquestion}
\title{Multipe-Choice Question}
\usage{
mcquestion(
  ...,
  type = "learnr_checkbox",
  allow_retry = TRUE,
  random_answer_order = TRUE,
  correct = c("Perfect!", "Nice!", "Well done!", "Good job!", "Great!", "Correct!",
    "Exactly!")
)
}
\arguments{
\item{..., type, allow_retry, random_answer_order}{see \link[learnr:quiz]{learnr::question} for these parameters}

\item{correct}{a vector of strings to be displayed for correct answers. One of these strings is chosen at random.}
}
\description{
Wrapper around \link[learnr:quiz]{learnr::question} with sensible defaults for multiple choice questions.
}
