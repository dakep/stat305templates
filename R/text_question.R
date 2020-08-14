#' Add a text question.
#'
#' @param question the question text. Markdown is supported.
#' @param label an optional label for the input element.
#' @param textarea if `TRUE` (default) uses a HTML *textarea* element for the input.
#'  If `FALSE`, uses an *input* element.
#' @param width the width of the input, e.g., '300px' or '100%'; see [shiny::validateCssUnit()].
#' @param height the height of the input, e.g., '300px' or '100%'; see [shiny::validateCssUnit()].
#'  Only has an effect if `textarea=TRUE`.
#' @param placeholder A character string giving the user a hint as to what can be entered into the control.
#'   Internet Explorer 8 and 9 do not support this option.
#'
#' @export
#' @importFrom htmltools h5 doRenderTags
text_question <- function (question, label = NULL, textarea = TRUE, title_container = h5, width = NULL,
                           height = NULL, placeholder = NULL) {
  txt_input <- list(
    id = random_ui_id(knitr::opts_current$get('label')),
    section = opts_current$get('stat305templates.in_section'),
    rendered_title = doRenderTags(title_container(render_markdown_as_html(question), class = 'panel-title')),
    textarea = isTRUE(textarea),
    label = label,
    placeholder = placeholder,
    width = width,
    height = height)
  class(txt_input) <- 'txt_input_question'
  return(txt_input)
}

#' Knitr text question print methods
#'
#' [knitr::knit_print] method for [txt_input_question].
#'
#' @inheritParams knitr::knit_print
#' @importFrom knitr knit_print
#' @importFrom shiny NS uiOutput textAreaInput textInput
#' @importFrom htmltools div tagAppendAttributes
#' @method knit_print txt_input_question
#' @rdname knit_print
#' @export
knit_print.txt_input_question <- function(x, ...) {
  ns <- NS(x$id)

  input_el <- if (x$textarea) {
    tagAppendAttributes(textAreaInput(ns('text'), label = x$label, width = '100%', height = x$height),
                        style = sprintf('width:%s;', x$width))
  } else {
    textInput(ns('text'), label = x$label, width = x$width)
  }

  ui <- div(id = ns('panel'), class = 'panel panel-default question-text',
            div(class = 'panel-heading', HTML(x$rendered_title)),
            div(class = 'panel-body', input_el, trigger_mathjax()))

  # regular knit print the UI
  knit_print(ui)
}
