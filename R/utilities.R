dput_object <- function (x) {
  conn <- textConnection('dput_object_out', 'w', local = TRUE)
  on.exit({ close(conn) })
  dput(x, file = conn, control = 'all')
  paste0(textConnectionValue(conn), collapse = '\n')
}

random_ui_id <- function (prefix) {
  if (missing(prefix) || is.null(prefix)) {
    prefix <- as.hexmode(sample.int(.Machine$integer.max, 1L))
  }
  paste(prefix, as.hexmode(sample.int(.Machine$integer.max, 1L)), sep = '-')
}

#' @importFrom htmltools tagList HTML tags
trigger_mathjax <- function(...) {
  tagList(..., htmltools::tags$script(HTML('if (window.exam) window.exam.triggerMathJax()')))
}

#' @importFrom shinyjs runjs
remote_trigger_mathjax <- function () {
  runjs('if (window.exam) window.exam.triggerMathJax()')
}

#' Render markdown text as HTML
#'
#' Translate markdown into HTML elements.
#'
#' @param text the markdown text to be parsed as HTML
#' @return a HTML container with the parsed markdown.
#'
#' @importFrom markdown markdownToHTML markdownExtensions
#' @export
render_markdown_as_html <- function (text) {
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

`%||%` <- function (x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' Get the default session data
#' @importFrom shiny getDefaultReactiveDomain
get_session_data <- function (what, fallback = NULL, session = getDefaultReactiveDomain(), asis = FALSE) {
  if (!asis && inherits(session, 'session_proxy')) {
    what <- session$ns(what)
  }
  return(session$userData[[paste0('stat305templates_', what)]] %||% fallback)
}

#' Get the default session data
#' @importFrom shiny getDefaultReactiveDomain
set_session_data <- function (what, value, session = getDefaultReactiveDomain(), asis = FALSE) {
  if (!asis && inherits(session, 'session_proxy')) {
    what <- session$ns(what)
  }
  session$userData[[paste0('stat305templates_', what)]] <- value
  return(invisible(value))
}

#' Set/get the "master" seed for the stat305templates package.
#'
#' All subsequent randomizations by the `stat305templates` package are based on this master seed.
#'
#' @param seed seed.
#' @param session the shiny reactive domain.
#' @export
set_session_seed <- function (seed, session) {
  session <- if (missing(session)) {
    getDefaultReactiveDomain()
  }
  set_session_data('master_seed', seed, session)
}

#' @rdname set_session_seed
#' @return [get_session_seed()] returns the current session seed or `NULL` if none was set.
#' @export
get_session_seed <- function(session) {
  get_session_data('master_seed', session)
}

#' @importFrom rlang try_fetch
with_abort <- function (expr) {
  try_fetch(
    expr,
    simpleError = function(cnd) {
      abort(
        conditionMessage(cnd),
        call = conditionCall(cnd),
        parent = NA
      )
    }
  )
}
