#' Create a series of sections, displayed sequentially.
#'
#' To begin a section, call [begin_section()] with the desired section, to end a section, call [end_section()].
#'
#' @param section,... names of sections, to be displayed in the given order
#' @param .group_id id for this group of sections.
#' @return a list of sections.
#'
#' @importFrom shinyjs useShinyjs
#' @importFrom shiny NS
#' @export
create_sections <- function (section, ..., .group_id) {
  section_names <- c(section, list(...))

  group_id <- if (missing(.group_id)) {
    'seq-section'
  } else {
    paste('seq-section', .group_id, sep = '_')
  }
  sections <- vector('list', length(section_names))
  names(sections) <- section_names

  for (i in seq_along(sections)) {
    next_section_id <- if (i < length(sections)) {
      as.character(i + 1L)
    } else {
      NULL
    }
    sections[[i]] <- structure(list(group_id = group_id, sid = as.character(i), next_sid = next_section_id,
                                    id = NS(group_id, i), title = render_markdown_as_html(section_names[[i]])),
                               class = 'sequential_section')
  }
  return(structure(sections, class = 'sequential_sections', gid = group_id))
}

#' Initialize sections for this session
#'
#' @param sections the sections object
#' @param hide should sections be hidden by default.
#' @importFrom shiny getDefaultReactiveDomain
#' @importFrom shinyjs useShinyjs
#' @export
init_sections <- function (sections, hide = TRUE, session = getDefaultReactiveDomain()) {
  assert_class(sections, 'sequential_sections')
  useShinyjs(html = TRUE)
  callModule(.init_sections_module, attr(sections, 'gid', TRUE), sections = sections, hide = hide)
}

#' Show a specific section.
#'
#' @param section section to show.
#' @param session shiny reactive domain.
#' @param hide_others hide all other sections.
#'
#' @importFrom shiny getDefaultReactiveDomain
#' @importFrom shinyjs hideElement showElement
#' @importFrom checkmate assert_class
#' @export
show_section <- function (section, hide_others = TRUE) {
  if (!inherits(section, 'sequential_section')) {
    section <- list(
      group_id = sub('^(.+)-[0-9]+$', '\\1', section),
      sid = sub('^.+-([0-9]+)$', '\\1', section)
    )
  }
  callModule(.show_section_module, section$group_id, sid = section$sid, hide_others = !isFALSE(hide_others))
}

#' Begin a section.
#'
#' This renders the section title.
#'
#' @param section section object.
#' @param container_class additional HTML class attribute
#' @param container a function to generate an HTML element to contain the section title.
#' @return section start elements.
#'
#' @importFrom htmltools h2
#' @export
begin_section <- function (section, container_class, title_container = h2) {
  section$rendered_title <- title_container(stat305templates:::render_markdown_as_html(section$title))
  section$additional_class <- if (missing(container_class) || is.null(container_class)) {
    ''
  } else {
    class
  }
  assert_class(section, 'sequential_section')
  class(section) <- c(class(section), 'sequential_section_begin')
  return(section)
}

#' End a section.
#'
#' This renders a button to submit the answers to the section.
#' When users click the button, a modal dialog is shown for the user to confirm they want to submit the section.
#'
#' @param section section object.
#' @param label submission button text.
#' @param confirm_dialog a modal dialog created with [shiny::modalDialog()] to display for confirming submission of the
#'    section. If `NULL`, or `FALSE`, no confirmation will be shown. This dialog needs to have at least one button
#'    with the `data-exam` attribute set to `continue`!
#' @param success_dialog a modal dialog created with [shiny::modalDialog()] to display after successful submission
#'    of the section. If `NULL`, or `FALSE`, no dialog will be shown.
#' @param error_dialog a modal dialog created with [shiny::modalDialog()] to display after successful submission
#'    of the section. If `NULL`, or `FALSE`, no dialog will be shown. This dialog needs to have at least one button
#'    with the `data-exam` attribute set to `reload`!
#' @param container a function to generate an HTML element to contain the button.
#' @return section end elements.
#'
#' @importFrom htmltools h2 div tags
#' @importFrom shiny modalDialog
#' @export
end_section <- function (label = "Submit Question", confirm_dialog, success_dialog, error_dialog,
                         btn_class = 'btn-primary btn-lg', container = div) {
  end <- list(
    container = container,
    btn_class = paste(btn_class %||% '', 'submit-section'),
    btn_label = label
  )

  end$confirm_dialog <- if (missing(confirm_dialog)) {
    modalDialog(title = "Are you sure you want to continue?",
                HTML("When you submit your answers to this section, you
                     <strong>can not come back to this section!</strong>"),
                footer = div(tags$button("No, I want to check my answers.", type = 'button',
                                         class = 'btn btn-default', `data-dismiss` = 'modal'),
                             tags$button("Yes, save answers and continue.", `data-exam` = 'continue',
                                         type = 'button', class = 'btn btn-danger')))
  } else if (isFALSE(confirm_dialog) || is.null(confirm_dialog)) {
    NULL
  } else {
    assert_class(confirm_dialog, 'shiny.tag')
    confirm_dialog
  }

  end$success_dialog <- if (missing(success_dialog)) {
    modalDialog(title = "Answers saved!", "Your answers have been saved successfully.",
                footer = modalButton('Continue.'), easyClose = TRUE)
  } else if (isFALSE(success_dialog) || is.null(success_dialog)) {
    NULL
  } else {
    assert_class(success_dialog, 'shiny.tag')
    success_dialog
  }

  end$error_dialog <- if (missing(error_dialog)) {
    modalDialog(title = "Answers could not be saved!",
                div(class = 'alert alert-danger',
                    tags$p("Your answers could not be saved. Please take not of your answers so far, encoded in the
                           following text:"),
                    tags$p(class = 'code')),
                footer = modalButton('Reload page'))
  } else if (isFALSE(error_dialog) || is.null(error_dialog)) {
    NULL
  } else {
    assert_class(error_dialog, 'shiny.tag')
    error_dialog
  }

  class(end) <- c(class(end), 'sequential_section_end')
  return(end)
}

## Print the section begin elements
## This opens a <div>, but does not close it! It is closed by the "end" section.
#' @inheritParams knitr::knit_print
#' @importFrom htmltools htmlPreserve
#' @importFrom knitr asis_output opts_chunk
#' @importFrom shiny NS
#' @method knit_print sequential_section_begin
#' @rdname knit_print
#' @export
knit_print.sequential_section_begin <- function (x, ...) {
  ns <- NS(x$group_id)
  opts_chunk$set('stat305templates.in_section' = x)
  asis_output(htmlPreserve(sprintf('<div id="%s" class="sequential-section %s">%s', ns(x$sid),
                                   x$additional_class, as.character(x$rendered_title))))
}

## Print the section end elements
## This closes the <div> that was opened by the "begin" section.
##
#' @inheritParams knitr::knit_print
#' @importFrom htmltools htmlPreserve div tags
#' @importFrom knitr asis_output opts_current opts_chunk
#' @importFrom shiny NS actionButton
#' @importFrom rmarkdown shiny_prerendered_chunk
#' @method knit_print sequential_section_end
#' @rdname knit_print
#' @export
knit_print.sequential_section_end <- function (x, ...) {
  section <- opts_current$get('stat305templates.in_section')
  if (is.null(section)) {
    stop("end_section() can only be called after begin_section()")
  }
  section_ns <- NS(c(section$group_id, section$sid))

  ui <- div(x$container(actionButton(section_ns('btn'), label = x$btn_label, class = x$btn_class,
                                     `data-errorDialogId` = section_ns('dialog-error'),
                                     `data-confirmDialogId` = section_ns('dialog-confirm'))),
            .add_id_to_dialog(x$confirm_dialog, section_ns('dialog-confirm')),
            .add_id_to_dialog(x$success_dialog, section_ns('dialog-success')),
            .add_id_to_dialog(x$error_dialog, section_ns('dialog-error')))

  x$container <- NULL

  shiny_prerendered_chunk('server', code = sprintf('stat305templates:::.submit_section_btn_prerendered_chunk(%s)',
                                                   dput_object(section)))

  opts_chunk$delete('stat305templates.in_section')

  asis_output(htmlPreserve(paste(as.character(ui), '</div>')))
}

.add_id_to_dialog <- function (dialog, id) {
  if (!is.null(dialog)) {
    dialog$attribs$id <- id
  }
  return(dialog)
}

.submit_section_btn_prerendered_chunk <- function (options) {
  global_session <- getDefaultReactiveDomain()
  callModule(.submit_section_btn_server, options$group_id, options = options,
             all_inputs = global_session$input, global_session = global_session)
  invisible(TRUE)
}

#' @importFrom shinyjs showElement hideElement
#' @importFrom shiny showModal observeEvent modalDialog modalButton reactiveValuesToList
#' @importFrom htmltools tags div
#' @importFrom shinyjs useShinyjs runjs toggleState
.submit_section_btn_server <- function (input, output, session, options, all_inputs, global_session) {
  full_ns <- NS(session$ns(options$sid))
  ns <- NS(options$sid)

  save_fun <- tryCatch(match.fun(getOption('stat305templates.section_submitted', NULL)),
                       error = function (...) { function (...) { return(TRUE); } })

  visible_sections <- .visible_sections()

  observeEvent(visible_sections[[options$sid]], {
    toggleState(ns('btn'), condition = isTRUE(isolate(visible_sections[[options$sid]])))
  })

  observeEvent(input[[ns('btn')]], {
    is_visible <- isTRUE(isolate(visible_sections[[options$sid]]))
    if (is_visible) {
      # Save all inputs
      success <- tryCatch(save_fun(isolate(reactiveValuesToList(all_inputs)),
                                   session$ns(options$next_sid), global_session),
                          error = function (e) {
                            warning(e)
                            return(FALSE)
                          })

      # Give feedback to user
      hideElement(selector = '.spin-container', asis = TRUE)
      runjs(sprintf('if (window.exam) window.exam.showDialog("%s", %s)',
                    full_ns('dialog-'), tolower(as.character(isTRUE(success)))))

      # Show next section
      .show_section_module(input, output, session, options$next_sid, hide_others = options$sid)
    } else {
      hideElement(selector = '.spin-container', asis = TRUE)
    }
  })
}

.visible_sections <- function (section) {
  if (missing(section)) {
    return(get_session_data('visible_sections', reactiveValues()))
  } else if (is.null(section)) {
    return(list())
  } else {
    get_session_data(NS(section$group_id, 'visible_sections'), reactiveValues(), asis = TRUE)
  }
}

#' @importFrom shinyjs runjs
.init_sections_module <- function (input, output, session, sections, hide = TRUE) {
  visible_sections <- get_session_data('visible_sections')

  if (is.null(visible_sections)) {
    visible_sections <- reactiveValues()
    set_session_data('visible_sections', visible_sections)
  }

  vis <- !isTRUE(hide)
  for (section in sections) {
    visible_sections[[section$sid]] <- vis
    hideElement(section$sid)
  }
  invisible(NULL)
}


## @param hide_others can be `TRUE`` (in which case all other sections are hidden), or a character (vector) of *scoped*
##   section ids
.show_section_module <- function (input, output, session, sid, hide_others) {
  visible_sections <- get_session_data('visible_sections', reactiveValues())

  if (isTRUE(hide_others)) {
    hide_others <- isolate(names(reactiveValuesToList(visible_sections)))
  }

  if (is.character(hide_others)) {
    # if (length(hide_others) > 1L) {
    #   hideElement(selector = paste(sprintf('#%s', session$ns(hide_others)), collapse = ','))
    # } else {
    #   hideElement(hide_others)
    # }
    for (scoped_sid in hide_others) {
      if (!isTRUE(scoped_sid == sid)) {
        hideElement(scoped_sid)
        visible_sections[[scoped_sid]] <- FALSE
      }
    }
  }

  # Now make the section visible.
  if (!is.null(sid)) {
    visible_sections[[sid]] <- TRUE
    showElement(sid)
    remote_trigger_mathjax()
  }
}
