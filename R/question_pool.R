#' Create a question with answers randomly taken from the pool of answers.
#'
#'
#' @param title question title.
#' @param ... answer options.
#' @param nr_answers maximum number of answers to display. At least one correct answer will always be shown.
#' @param mc show as multiple choice.
#' @param random_answer_order should the order of answers be randomized?
#' @param container_class additional HTML classes to add to the container.
#' @param show_only_with_section render the output only if the section is visible.
#' @param post_rendered optional function to post-process rendered text (e.g., to replace placeholders). The function
#'   is called with a single string marked as HTML and evaluated in the **server context**.
#' @param correct_label if not `NULL`, change the answer option labels to hex numbers with the right-most digit
#'   being equal to the given number (by default 0xa). The length of the hex number is determined by the number of
#'   answer options.
#' @importFrom ellipsis check_dots_unnamed
#' @importFrom checkmate assert_class
#' @importFrom knitr opts_current
#' @importFrom htmltools h5 doRenderTags
#' @importFrom digest digest2int
#' @export
question_pool <- function(title, ..., nr_answers = 5, random_answer_order = TRUE, container_class = NULL,
                          mc = FALSE, post_rendered = NULL, title_container = h5, correct_label = 0xc,
                          show_only_with_section = TRUE) {
  # Capture and validate answers.
  check_dots_unnamed()
  answers <- list(...)

  total_nr_answers <- length(answers)

  is_correct <- unlist(lapply(answers, function(answer) {
    assert_class(answer, 'tutorial_question_answer')
    answer$correct
  }), recursive = FALSE, use.names = FALSE)
  answers <- split(answers, factor(is_correct, labels = c('not_correct', 'correct')))

  set.seed(digest2int(opts_current$get('label') %||% 'unnamed-chunk'))
  # adjust answer label
  if (!is.null(correct_label)) {
    if (!isTRUE(correct_label <= 0xf)) {
      stop('`correct_label` must be between 0x0 and 0xf')
    }
    sample_pool <- 0xf * (length(answers) %/% 16L + 1L)
    correct_vals <- as.hexmode(16L * sample.int(sample_pool, length(answers$correct)) + correct_label)
    wrong_vals <- as.hexmode(16L * sample.int(sample_pool, length(answers$not_correct)) +
                               sample(c(seq_len(correct_label),
                                        1L + correct_label + seq_len(16L - correct_label - 1L)) - 1,
                                      length(answers$not_correct), replace = TRUE))
    answers$correct <- mapply(answer = answers$correct, value = as.character(correct_vals),
                              FUN = function (answer, value) { answer$value <- value; answer }, SIMPLIFY = FALSE)
    answers$not_correct <- mapply(answer = answers$not_correct, value = as.character(wrong_vals),
                                  FUN = function (answer, value) { answer$value <- value; answer }, SIMPLIFY = FALSE)
  }

  nr_answers <- if (is.null(nr_answers)) {
    total_nr_answers
  } else {
    max(2, min(nr_answers, total_nr_answers))
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
                        random_answer_order = random_answer_order, nr_answers = nr_answers, mc = isTRUE(mc),
                        show_only_with_section = isTRUE(show_only_with_section), container_class = container_class),
                   class = 'question_pool'))
}


#' Knitr question pool print methods
#'
#' [knitr::knit_print] method for [question_pool].
#'
#' @inheritParams knitr::knit_print
#' @importFrom knitr knit_print
#' @importFrom shiny NS uiOutput radioButtons checkboxGroupInput
#' @importFrom htmltools div
#' @method knit_print question_pool
#' @rdname knit_print
#' @export
knit_print.question_pool <- function(x, ...) {
  x$section <- opts_current$get('stat305templates.in_section')
  ns <- NS(x$id)

  input_group <- if (isTRUE(x$mc)) {
    checkboxGroupInput(ns('answer_radios'), label = 'Answer', choices = c('N/A' = 'N/A'), selected = '')
  } else {
    radioButtons(ns('answer_radios'), label = 'Answer', choices = c('N/A' = 'N/A'), selected = '')
  }

  ui <- div(id = ns('panel'), class = paste('panel panel-default question-pool', x$container_class, sep = ' '),
            div(class = 'panel-heading', HTML(x$rendered_title)),
            div(class = 'panel-body', input_group, trigger_mathjax()))

  rmarkdown::shiny_prerendered_chunk('server', sprintf(
    'stat305templates:::.question_pool_prerendered_chunk(%s)', dput_object(x)))

  # regular knit print the UI
  knit_print(ui)
}

#' @importFrom shiny callModule
.question_pool_prerendered_chunk <- function (question, ...) {
  eval_env <- parent.frame()
  callModule(.question_pool_server, question$id, question = question, eval_env = eval_env)
  invisible(TRUE)
}

#' @importFrom shiny updateRadioButtons updateCheckboxGroupInput observeEvent
.question_pool_server <- function (input, output, session, question, eval_env) {
  set.seed(get_session_data('master_seed', 1L, asis = TRUE) + 10L)

  nr_answers_correct <- if (isTRUE(question$mc)) {
    sample.int(min(length(question$answers[['correct']]), question$nr_answers), 1L)
  } else {
    1L
  }

  sampled_answers <- c(sample(question$answers[['correct']], nr_answers_correct),
                       sample(question$answers[['not_correct']], question$nr_answers - nr_answers_correct))
  rand_order <- sample.int(question$nr_answers)


  labels <- lapply(sampled_answers, `[[`, 'label')
  values <- sapply(sampled_answers, `[[`, 'value')
  set_session_data(sprintf('question_pool-labels-%s', session$ns('answer_radios')),
                   setNames(labels, values), asis = TRUE)

  if (!is.null(question$post_rendered)) {
    labels <- lapply(labels, function (lbl) {
      eval(substitute(pr(lbl), list(pr = question$post_rendered, lbl = lbl)), envir = eval_env)
    })
  }

  values <- values[rand_order]
  labels <- lapply(labels[rand_order], render_markdown_as_html)

  latest_valid_input <- 'N/A'

  if (question$show_only_with_section && !is.null(question$section)) {
    visible_sections <- .visible_sections(question$section)

    observeEvent(visible_sections[[question$section$sid]], {
      if (isTRUE(visible_sections[[question$section$sid]])) {
        # Going from not visible to visible
        if (isTRUE(question$mc)) {
          updateCheckboxGroupInput(session, inputId = 'answer_radios', selected = latest_valid_input,
                                   choiceValues = values, choiceNames = labels)
        } else {
          updateRadioButtons(session, inputId = 'answer_radios', selected = latest_valid_input,
                             choiceValues = values, choiceNames = labels)
        }
        remote_trigger_mathjax()
      } else {
        # Going from visible to not visible
        latest_valid_input <- isolate(input$answer_radios)
        if (isTRUE(question$mc)) {
          updateCheckboxGroupInput(session, inputId = 'answer_radios', choices = c('N/A' = 'N/A'), selected = '')
        } else {
          updateRadioButtons(session, inputId = 'answer_radios', choices = c('N/A' = 'N/A'), selected = '')
        }
      }
    })
  } else {
    if (isTRUE(question$mc)) {
      updateCheckboxGroupInput(session, inputId = 'answer_radios', selected = latest_valid_input,
                               choiceValues = values, choiceNames = labels)
    } else {
      updateRadioButtons(session, inputId = 'answer_radios', selected = latest_valid_input,
                         choiceValues = values, choiceNames = labels)
    }
    remote_trigger_mathjax()
  }
}

