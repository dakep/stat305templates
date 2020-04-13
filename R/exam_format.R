#' Exam Format
#'
#'
#' @export
#' @importFrom rmarkdown html_document
#' @importFrom htmltools htmlDependency
#' @importFrom utils packageVersion
#' @importFrom knitr opts_chunk opts_hooks
exam <- function (...) {
  output_format <- html_document(section_divs = FALSE, ...)
  render_env <- environment(output_format$intermediates_generator)
  render_env$extra_dependencies <- append(render_env$extra_dependencies, list(
    htmlDependency('exam', version = packageVersion('stat305templates'),
                   src = system.file('rmarkdown/templates/exam/resources', package = 'stat305templates'),
                   script = 'exam.js', stylesheet = 'exam.css')))

  # opts_chunk$set(stat305templates.exam = TRUE)
  # opts_hooks$set(stat305templates.exam = function (options) {
  #   initialize_exam()
  #   return(options)
  # })
  initialize_exam()

  return(output_format)
}

#' @importFrom knitr opts_knit knit_meta_add
#' @importFrom rmarkdown shiny_prerendered_chunk html_dependency_jquery html_dependency_bootstrap
initialize_exam <- function (...) {
  if (isTRUE(getOption('knitr.in.progress')) && !isTRUE(opts_knit$get('stat305templates.exam.initialized'))) {
    knit_meta_add(list(html_dependency_jquery()))

    shiny_prerendered_chunk('server', sprintf('stat305templates:::.initialize_exam_server(session, metadata = %s)',
                                              dput_object(rmarkdown::metadata$exam)), singleton = TRUE)

    opts_knit$set(stat305templates.exam.initialized = TRUE)
  }
}

#' @importFrom shinyjs useShinyjs runjs
#' @importFrom utils URLencode
.initialize_exam_server <- function (session, metadata) {
  useShinyjs(html = TRUE)

  exam_id <- URLencode(paste(metadata$id %||% 'exam', metadata$version %||% '1', sep = '-'), reserved = TRUE)
  runjs(sprintf('if (window.exam) { window.exam.setExamId("%s") }', exam_id))
}

