#' Create a question with answers randomly taken from the pool of answers.
#'
#'
#' @param title question title.
#' @param ... answer options.
#' @param nr_answers number of incorrect answers to display. By default one correct answer and all incorrect answers
#'  are shown.
#' @param random_answer_order should the order of answers be randomized?
#' @param container_class additional HTML classes to add to the container.
#' @param show_only_with_section render the output only if the section is visible.
#' @param post_rendered optional function to post-process rendered text (e.g., to replace placeholders). The function
#'   is called with a single string marked as HTML.
#' @importFrom ellipsis check_dots_unnamed
#' @importFrom checkmate assert_class
#' @importFrom htmltools h5 doRenderTags
#' @export
question_pool <- function(title, ..., nr_answers, random_answer_order = TRUE, container_class = NULL,
                          post_rendered = NULL, title_container = h5,
                          show_only_with_section = TRUE) {
  # Capture and validate answers.
  check_dots_unnamed()
  answers <- list(...)

  is_correct <- unlist(lapply(answers, function(answer) {
    assert_class(answer, 'tutorial_question_answer')
    answer$correct
  }), recursive = FALSE, use.names = FALSE)
  answers <- split(answers, factor(is_correct, labels = c('not_correct', 'correct')))

  if (missing(nr_answers)) {
    nr_answers <- length(answers[['not_correct']])
  }

  if (length(answers[['correct']]) == 0L) {
    stop("at least one correct answer must be provided.")
  }

  if (length(answers[['not_correct']]) == 0L) {
    stop("at least one incorrect answer must be provided.")
  }

  container_class <- if (is.null(container_class)) {
    ''
  } else {
    container_class
  }

  q_id <- random_ui_id(knitr::opts_current$get('label'))

  rendered_title <- doRenderTags(title_container(render_markdown_as_html(title), class = 'panel-title'))

  return(structure(list(id = q_id, rendered_title = rendered_title, answers = answers, post_rendered = post_rendered,
                        random_answer_order = random_answer_order, nr_answers = nr_answers,
                        show_only_with_section = isTRUE(show_only_with_section), container_class = container_class),
                   class = 'question_pool'))
}

#' Knitr question pool print methods
#'
#' [knitr::knit_print] method for [question_pool].
#'
#' @inheritParams knitr::knit_print
#' @importFrom knitr knit_print
#' @importFrom shiny NS uiOutput
#' @importFrom htmltools div
#' @method knit_print question_pool
#' @rdname knit_print
#' @export
knit_print.question_pool <- function(x, ...) {
  x$section <- opts_current$get('stat305templates.in_section')
  ns <- NS(x$id)

  ui <- div(id = ns('panel'), class = paste('panel panel-default question-pool', x$container_class, sep = ' '),
            div(class = 'panel-heading', HTML(x$rendered_title)),
            div(class = 'panel-body',
                radioButtons(ns('answer_radios'), label = 'Answer', choices = c('N/A' = 'N/A'), selected = ''),
                trigger_mathjax()))
  # too late to try to set a chunk attribute
  # knitr::set_chunkattr(echo = FALSE)
  rmarkdown::shiny_prerendered_chunk('server', sprintf(
    'stat305templates:::.question_pool_prerendered_chunk(%s)', dput_object(x)))

  # regular knit print the UI
  knit_print(ui)
}

#' @importFrom shiny callModule
.question_pool_prerendered_chunk <- function (question, ...) {
  callModule(.question_pool_server, question$id, question = question)
  invisible(TRUE)
}

#' @importFrom shiny updateRadioButtons observeEvent
.question_pool_server <- function (input, output, session, question) {
  set.seed(get_session_data('master_seed', 1L, asis = TRUE) + 10L)
  correct_answer <- sample(question$answers[['correct']], 1L)[[1L]]
  incorrect_answers <- sample(question$answers[['not_correct']], question$nr_answers)
  rand_order <- sample.int(question$nr_answers + 1L)

  labels <- c(list(correct_answer$label), lapply(incorrect_answers, `[[`, 'label'))
  values <- c(correct_answer$value, sapply(incorrect_answers, `[[`, 'value'))

  if (!is.null(question$post_rendered)) {
    labels <- lapply(labels, question$post_rendered)
  }

  values <- values[rand_order]
  labels <- lapply(labels[rand_order], render_markdown_as_html)

  latest_valid_input <- 'N/A'

  if (question$show_only_with_section && !is.null(question$section)) {
    visible_sections <- .visible_sections(question$section)

    observeEvent(visible_sections[[question$section$sid]], {
      if (isTRUE(visible_sections[[question$section$sid]])) {
        # Going from not visible to visible
        updateRadioButtons(session, inputId = 'answer_radios', selected = latest_valid_input,
                           choiceValues = values, choiceNames = labels)
        remote_trigger_mathjax()
      } else {
        # Going from visible to not visible
        latest_valid_input <- isolate(input$answer_radios)
        updateRadioButtons(session, inputId = 'answer_radios', choices = c('N/A' = 'N/A'), selected = '')
      }
    })
  } else {
    updateRadioButtons(session, inputId = 'answer_radios', selected = latest_valid_input,
                       choiceValues = values, choiceNames = labels)
    remote_trigger_mathjax()
  }

  # observe(label = 'update radios', {
  #   visible_condition <- .visible_condition(question$section)
  #   if (!question$show_only_with_section || visible_condition()) {
  #     warning("Transitioning question pool ", question$id, " from not visible to visible. Latest valid input was ",
  #             latest_valid_input)
  #     # Going from not visible to visible
  #     updateRadioButtons(session, inputId = 'answer_radios', selected = latest_valid_input,
  #                        choiceValues = values, choiceNames = labels)
  #   } else {
  #     # Going from visible to not visible
  #     latest_valid_input <- isolate(input$answer_radios)
  #     warning("Transitioning question pool ", question$id, " from visible to not visible. Setting latest valid input to ",
  #             latest_valid_input)
  #     updateRadioButtons(session, inputId = 'answer_radios', choices = c('N/A' = 'N/A'), selected = '')
  #   }
  # })
}

