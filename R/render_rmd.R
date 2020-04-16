#' Render the given R Markdown file *on the server*
#'
#' @param file path to the RMarkdown file
#' @param data a list of data to be made available to the Rmd file during rendering. Quoted items will be evaluated
#'   on the server before knitting the Rmd file. Quoted expressions are evaluated in the **server context**.
#' @param show_only_with_section render the output only if the section is visible.
#' @param runtime,quiet arguments passed on to [rmarkdown::render()].
#' @param section_divs,md_extensions,df_print... passed on to [rmarkdown::html_fragment()].
#' @return the UI output element
#'
#' @importFrom rmarkdown html_fragment
#'
#' @export
render_rmd_server <- function (file, data = list(), runtime = 'static', quiet = TRUE,
                               section_divs = FALSE, md_extensions = '+tex_math_dollars', df_print = 'kable', ...,
                               show_only_with_section = TRUE) {
  id <- random_ui_id()

  if (is.null(data)) {
    data <- list()
  }

  if (!is.list(data)) {
    stop("`data` must be a list.")
  }

  # Force evaluation of output arguments!
  output_format_args <- c(list(section_divs = section_divs, md_extensions = md_extensions, df_print = df_print),
                          list(...))
  output_format <- substitute(do.call(rmarkdown::html_fragment, output_format_args))

  return(structure(list(id = id, file = file, envir = data, runtime = runtime, output_format_call = output_format,
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
#' @importFrom rmarkdown render
.render_rmd_server_prerendered_chunk <- function (options, ...) {
  eval_env <- parent.frame()
  options$envir <- as.environment(lapply(options$envir, function (envvar) {
    if (is.expression(envvar) || is.language(envvar)) {
      return(eval(envvar, eval_env))
    }
    return(envvar)
  }))
  parent.env(options$envir) <- eval_env

  tmp_dir <- tempfile('render_rmd')

  # The rmarkdown::render function leaves a folder in the tempdir() folder behind!
  # Manual hack to remove this folder
  dirs_before_render <- list.files(tempdir(), full.names = TRUE, include.dirs = TRUE)
  on.exit({
    unlink(setdiff(list.files(tempdir(), full.names = TRUE, include.dirs = TRUE), dirs_before_render),
           force = TRUE, recursive = TRUE)
  }, add = TRUE, after = FALSE)

  set.seed(get_session_data('master_seed', 1L) + 50L)
  rendered <- render(options$file, runtime = options$runtime, output_format = eval(options$output_format),
                     knit_root_dir = tmp_dir,
                     envir = options$envir, output_dir = tmp_dir, intermediates_dir = tmp_dir, quiet = options$quiet)
  ui <- trigger_mathjax(HTML(paste0(readLines(rendered, encoding = 'UTF-8'), collapse = '\n')))

  callModule(.render_rmd_server_impl, options$id, options = options, ui = ui,
             username = getDefaultReactiveDomain()$username %||% 'anonymous')
  invisible(TRUE)
}

#' @importFrom shiny callModule includeMarkdown renderUI
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
