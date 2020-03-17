#' Create a clicker question.
#'
#' By default, clicker questions are only rendered if the option `stat305templates.render_clicker_questions` is
#' `TRUE` or if it is a function which returns `TRUE`.
#'
#' @param title question title.
#' @param ... 5 answer options. They will be numbered 'A'-'E'.
#' @param body optional question body.
#' @param in_class_only if the question should only be shown 'in-class' or also when printing out the slides.
#' @param container_class additional HTML classes to add to the container.
#' @param btn_text text for the "show" button.
#' @param force_render should the question always be rendered?
#'
#' @importFrom ellipsis check_dots_unnamed
#' @importFrom checkmate assert_class
#' @importFrom htmltools h3 div tags tagList doRenderTags a
#' @export
clicker_question <- function(title, ..., body = NULL, in_class_only = FALSE, container_class = NULL,
                             force_render = FALSE, btn_text = "Show Clicker Question") {
  # Capture and validate answers.
  check_dots_unnamed()
  answers <- list(...)

  if (length(answers) != 5L) {
    warning('Clicker questions expect 5 answer options!')
  }

  lapply(answers, function(answer) {
    assert_class(answer, 'tutorial_question_answer')
  })


  container_class <- if (isTRUE(in_class_only)) {
    if (is.null(container_class)) {
      'in-class'
    } else {
      paste(container_class, 'in-class', sep = ' ')
    }
  } else if (is.null(container_class)) {
    ''
  }

  # can not guarantee that `label` exists
  label <- knitr::opts_current$get('label')
  q_id <- if (is.null(label)) {
    paste0('clicker-question-', as.hexmode(sample.int(1073741824L, 1L)))
  } else {
    label
  }

  answer_options <- tagList(lapply(answers, function (answer) {
    tags$li(render_question_text(answer$label))
  }))

  ns <- NS(q_id)

  rendered <- doRenderTags(tagList(h3(render_question_text(title)), div(render_question_text(body)),
                                   tags$ol(answer_options)), indent = FALSE)

  btn_rendered <- doRenderTags(a(render_question_text(btn_text), class = 'btn btn-primary show-clicker',
                                 `data-toggle` = 'collapse', `aria-expanded` = 'false',
                                 `aria-controls` = 'collapseClickerQuestion',
                                 href = paste('#', ns('answer_container'), sep = '')))

  return(structure(list(id = q_id, rendered = rendered, btn_rendered = btn_rendered,
                        force_render = force_render, container_class = container_class), class = 'clicker_question'))
}

#' Knitr clicker question print methods
#'
#' [knitr::knit_print] method for [clicker_question].
#'
#' @inheritParams knitr::knit_print
#' @importFrom knitr knit_print
#' @importFrom shiny NS
#' @importFrom htmltools div a
#' @method knit_print clicker_question
#' @rdname knit_print
#' @export
knit_print.clicker_question <- function(x, ...) {
  ns <- NS(x$id)
  ui <- div(id = ns('section'), class = paste('section level3 clicker', x$container_class, sep = ' '),
            uiOutput(ns('answer_button')), uiOutput(ns('answer_container'), class = 'collapse'), trigger_mathjax())

  # too late to try to set a chunk attribute
  # knitr::set_chunkattr(echo = FALSE)
  rmarkdown::shiny_prerendered_chunk('server', sprintf(
    'stat305templates:::clicker_prerendered_chunk(%s)', dput_object(x)))

  # regular knit print the UI
  knitr::knit_print(ui)
}

dput_object <- function (x) {
  conn <- textConnection('dput_object', 'w')
  on.exit({ close(conn) })
  dput(x, file = conn)
  paste0(textConnectionValue(conn), collapse = '')
}

#' @importFrom shiny callModule
clicker_prerendered_chunk <- function (question, ...) {
  render <- getOption('stat305templates.render_clicker_questions', TRUE)
  if (is.function(render)) {
    render <- render()
  }
  if (isTRUE(question$force_render) || isTRUE(render)) {
    callModule(clicker_question_server, question$id, question = question)
  }
  invisible(TRUE)
}

#' @importFrom shiny renderUI
#' @importFrom htmltools div tagList h3 tags
clicker_question_server <- function (input, output, session, question) {
  output$answer_container <- renderUI({
    trigger_mathjax(HTML(question$rendered))
  })
  output$answer_button <- renderUI({
    HTML(question$btn_rendered)
  })
}

#' @importFrom htmltools tagList HTML tags
trigger_mathjax <- function(...) {
  tagList(..., htmltools::tags$script(HTML("if (Tutorial.triggerMathJax) Tutorial.triggerMathJax()")))
}

#' @importFrom markdown markdownToHTML markdownExtensions
render_question_text <- function (text) {
  if (inherits(text, 'html')) {
    return(text)
  }
  if (inherits(text, "shiny.tag") || inherits(text, "shiny.tag.list")) {
    return(text)
  }
  if (!is.null(text)) {
    # convert markdown
    md <- markdownToHTML(text = text, options = c('use_xhtml', 'fragment_only', 'mathjax'),
                         extensions = markdownExtensions(), fragment.only = TRUE, encoding = 'UTF-8')
    # remove leading and trailing paragraph
    md <- sub('^<p>', '', md)
    md <- sub('</p>\n?$', '', md)
    return(HTML(md))
  }
  else {
    return(NULL)
  }
}
