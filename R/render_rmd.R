#' Render the given R Markdown file *on the server*
#'
#' @param file path to the RMarkdown file
#' @param data a list of data to be made available to the Rmd file during rendering. Quoted items will be evaluated
#'   on the server before knitting the Rmd file. Quoted expressions are evaluated in the **server context**.
#' @param precompiled_path path where precompiled HTML files are located (if `NULL`, don't use precompiled HTML files).
#'   See [precompile_rmd()] how to precompile HTML files.
#' @param show_only_with_section render the output only if the section is visible.
#' @param hasher function to compute the unique hash for the input data. Only used if `precompiled_path` is not `NULL`.
#' @param runtime,quiet arguments passed on to [rmarkdown::render()].
#' @param section_divs,md_extensions,df_print... passed on to [rmarkdown::html_fragment()].
#' @return the UI output element
#'
#' @importFrom rmarkdown html_fragment
#'
#' @export
render_rmd_server <- function (file, data = list(), runtime = 'static', quiet = TRUE, precompiled_path = NULL,
                               section_divs = FALSE, md_extensions = '+tex_math_dollars', df_print = 'kable', ...,
                               show_only_with_section = TRUE, hasher = NULL) {
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
                        precompiled_path = precompiled_path, hasher = hasher,
                        show_only_with_section = isTRUE(show_only_with_section), quiet = quiet),
                   class = 'rendered_rmd'))
}

#' Precompile HTML files
#'
#' @param file path to the RMarkdown file
#' @param data a list of data to be made available to the Rmd file during rendering. Quoted items will be evaluated
#'   on the server before knitting the Rmd file. Quoted expressions are evaluated in the **server context**.
#' @param precompiled_path path where precompiled HTML file should be saved.
#' @param hasher function to compute the unique hash for the input data.
#' @param show_only_with_section render the output only if the section is visible.
#' @param runtime,quiet arguments passed on to [rmarkdown::render()].
#' @param section_divs,md_extensions,df_print... passed on to [rmarkdown::html_fragment()].
#' @return the path to the prerendered HTML file
#'
#' @importFrom digest digest
#' @export
precompile_rmd <- function (file, data = list(), runtime = 'static', quiet = TRUE, precompiled_path, hasher = digest,
                            section_divs = FALSE, md_extensions = '+tex_math_dollars', df_print = 'kable', ...) {
  # Force evaluation of output arguments!
  output_format_args <- c(list(section_divs = section_divs, md_extensions = md_extensions, df_print = df_print),
                          list(...))
  output_format <- do.call(rmarkdown::html_fragment, output_format_args)

  eval_env <- parent.frame()
  .compile_rmd(file, data, precompiled_path, output_format, eval_env, runtime, quiet, precompiled_path = NULL,
               hasher = hasher)
}

#' @importFrom rmarkdown render
.compile_rmd <- function (file, data, output_dir, output_format, eval_env, runtime, quiet, hasher,
                          precompiled_path) {
  envir <- lapply(data, function (envvar) {
    if (is.expression(envvar) || is.language(envvar)) {
      return(eval(envvar, eval_env))
    }
    return(envvar)
  })

  output_file <- paste(sub('^(.+)\\..{1,4}$', '\\1', basename(file)), hasher(envir), 'html', sep = '.')

  if (!is.null(precompiled_path)) {
    rendered_file <- normalizePath(file.path(precompiled_path, output_file), mustWork = FALSE)
    if (file.exists(rendered_file)) {
      message("Using precompiled file ", rendered_file)
      return(rendered_file)
    }
  }

  # put data into an environment
  envir <- list2env(envir, parent = eval_env)
  # The rmarkdown::render function leaves a folder in the tempdir() folder behind!
  # Manual hack to remove this folder
  dirs_before_render <- list.files(tempdir(), full.names = TRUE, include.dirs = TRUE)
  on.exit({
    unlink(setdiff(list.files(tempdir(), full.names = TRUE, include.dirs = TRUE), dirs_before_render),
           force = TRUE, recursive = TRUE)
  }, add = TRUE, after = FALSE)

  render(file, output_file = I(output_file), runtime = runtime, output_format = output_format,
         knit_root_dir = output_dir, envir = envir, output_dir = output_dir, intermediates_dir = output_dir,
         quiet = quiet)
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
#' @importFrom digest digest
.render_rmd_server_prerendered_chunk <- function (options, ...) {

  if (is.null(options$hasher)) {
    options$hasher <- digest
  }

  eval_env <- parent.frame()
  tmp_dir <- tempfile('render_rmd')
  dir.create(tmp_dir, mode = '0700')
  on.exit(unlink(tmp_dir), add = TRUE)
  rendered <- .compile_rmd(options$file, options$envir, tmp_dir, eval(options$output_format), eval_env, options$runtime,
                           options$quiet, precompiled_path = options$precompiled_path, hasher = options$hasher)
  ui <- trigger_mathjax(HTML(paste0(readLines(rendered, encoding = 'UTF-8'), collapse = '\n')))

  callModule(.render_rmd_server_impl, options$id, options = options, ui = ui,
             username = getDefaultReactiveDomain()$user %||% 'anonymous')
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
