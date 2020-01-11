#' Worksheet Format
#'
#' Displays the content in large font and transforms 3rd-level sections into questions which are by default
#' collapsed.
#'
#' @export
#' @importFrom learnr tutorial
#' @importFrom htmltools htmlDependency
#' @importFrom utils packageVersion
worksheet <- function (...) {
  output_format <- tutorial(...)
  render_env <- environment(output_format$intermediates_generator)
  render_env$extra_dependencies <- append(render_env$extra_dependencies, list(
      htmlDependency('worksheet', version = packageVersion('stat305templates'),
                     src = system.file('rmarkdown/templates/worksheet/resources', package = 'stat305templates'),
                     script = 'worksheet.js', stylesheet = 'worksheet.css')))

  return(output_format)
}
