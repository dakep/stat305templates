#' Initialize a text question which checks the answer against a regular expression
#'
#' @importFrom learnr question_ui_initialize
#' @importFrom shiny textInput
#' @export
question_ui_initialize.text_regexp <- function(question, value, ...) {
  textInput(question$ids$answer, label = question$question, placeholder = question$options$placeholder,
            value = value)
}

#' Validate a text question which checks the answer against a regular expression
#'
#' @importFrom learnr question_is_valid
#' @export
question_is_valid.text_regexp <- function(question, value, ...) {
  if (is.null(value)) {
    return(FALSE)
  }
  if (isTRUE(question$options$trim)) {
    return(nchar(str_trim(value)) > 0)
  } else{
    return(nchar(value) > 0)
  }
}

#' Check the answer to a text question against a regular expression
#'
#' @importFrom learnr question_is_correct mark_as
#' @importFrom shiny showNotification
#' @importFrom stringr str_trim str_detect
#' @export
question_is_correct.text_regexp <- function(question, value, ...) {
  if (nchar(value) == 0) {
    showNotification("Please enter some text before submitting", type = "error")
    req(value)
  }

  if (isTRUE(question$options$trim)) {
    value <- str_trim(value)
  }

  for (ans in question$answers) {
    if (!isTRUE(str_detect(value, pattern = ans$value))) {
      return(mark_as(FALSE, ans$message))
    }
  }

  mark_as(FALSE, NULL)
}
