#' Render the given R Markdown file *on the server*
#'
#' @param file path to the RMarkdown file
#' @param data a list of data to be made available to the Rmd file during rendering. Quoted items will be evaluated
#'   on the server before knitting the Rmd file.
#' @param show_only_with_section render the output only if the section is visible.
#' @param runtime,output_format,quiet arguments passed on to [rmarkdown::render()].
#' @return the UI output element
#'
#' @importFrom rmarkdown md_document
#'
#' @export
render_rmd_server <- function (file, data = list(), runtime = 'static', output_format, quiet = TRUE,
                               show_only_with_section = TRUE) {
  id <- random_ui_id()

  if (is.null(data)) {
    data <- list()
  }

  if (!is.list(data)) {
    stop("`data` must be a list.")
  }

  if (missing(output_format)) {
    output_format <- md_document(variant = 'markdown', md_extensions = '+tex_math_dollars')
  }

  return(structure(list(id = id, file = file, envir = data, runtime = runtime, output_format = output_format,
                        show_only_with_section = isTRUE(show_only_with_section), quiet = quiet),
                   class = 'rendered_rmd'))
}

#' Knitr question pool print methods
#'
#' [knitr::knit_print] method for [question_pool].
#'
#' @inheritParams knitr::knit_print
#' @importFrom knitr knit_print
#' @importFrom shiny NS uiOutput
#' @importFrom htmltools div
#' @method knit_print rendered_rmd
#' @rdname knit_print
#' @export
knit_print.rendered_rmd <- function(x, ...) {
  x$section <- opts_current$get('stat305templates.in_section')

  ns <- NS(x$id)
  ui <- uiOutput(ns('rmd_output'))
  rmarkdown::shiny_prerendered_chunk('server', sprintf(
    'stat305templates:::.render_rmd_server_prerendered_chunk(%s)', dput_object(x)))

  knit_print(ui)
}

#' @importFrom shiny callModule
.render_rmd_server_prerendered_chunk <- function (options, ...) {
  options$envir <- as.environment(lapply(options$envir, function (envvar) {
    if (is.expression(envvar) || is.language(envvar)) {
      return(local(eval(envvar)))
    }
    return(envvar)
  }))
  parent.env(options$envir) <- environment()

  warning("The master seed is set to ", get_session_data('master_seed', 1L))
  set.seed(get_session_data('master_seed', 1L) + 50L)
  rendered <- render(options$file, runtime = options$runtime, output_format = options$output_format,
                     envir = options$envir, output_dir = tempdir(), quiet = options$quiet)
  ui <- trigger_mathjax(includeMarkdown(rendered))

  callModule(.render_rmd_server_impl, options$id, options = options, ui = ui,
             username = getDefaultReactiveDomain()$username %||% 'anonymous')
  invisible(TRUE)
}

#' @importFrom shiny callModule includeMarkdown renderUI
#' @importFrom rmarkdown render
.render_rmd_server_impl <- function (input, output, session, options, ui, username) {
  visible_sections <- .visible_sections(options$section)
  output$rmd_output <- renderUI({
    if (!options$show_only_with_section || isTRUE(visible_sections[[options$section$sid]])) {
      ui
    } else {
      warning(sprintf("Attempt to show hidden content (username: %s)", username))
      div(class = 'alert alert-warning',
          "The content you are trying to view is hidden. This incident will be reported.")
    }
  })
}
