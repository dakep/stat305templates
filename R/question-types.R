#' Multipe-Choice Question
#'
#' Wrapper around [learnr::question] with sensible defaults for multiple choice questions.
#' @param ...,type,allow_retry,random_answer_order see [learnr::question] for these parameters
#' @param correct a vector of strings to be displayed for correct answers. One of these strings is chosen at random.
mcquestion <- function (..., type = 'learnr_checkbox', allow_retry = TRUE, random_answer_order = TRUE,
                        correct = c('Perfect!', 'Nice!', 'Well done!', 'Good job!', 'Great!', 'Correct!', 'Exactly!')) {
  learnr::question(...,
                   type = 'learnr_checkbox',
                   allow_retry = TRUE,
                   correct = sample(correct, 1),
                   random_answer_order = TRUE)
}
