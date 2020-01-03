#' Start or Resume a User Session
#'
#' @param appid application id. All applications with the same ID share the same session (e.g., to store the user id).
#' @param userid_param_name query string parameter name for the user id.
#' @param default_userid fallback user id if it can not be determined from the query string or read/written from/to a
#'                       cookie.
#' @param session_length how long (in days) should the user session last?
#' @param domain Shiny reactive domain.
#'
#' @importFrom shinyjs useShinyjs extendShinyjs
#' @importFrom shiny getDefaultReactiveDomain observe
#' @export
start_user_session <- function (appid, userid_param_name = 'userid', default_userid = 'unknown',
                                domain = getDefaultReactiveDomain(), session_length = 0.125) {
  useShinyjs(html = TRUE)
  extendShinyjs(script = system.file('srcjs', 'session.js', package = 'stat305templates'),
                functions = c('getcookie', 'setcookie', 'rmcookie'))

  if (is.null(.get_user_id(domain))) {
    .set_user_id(domain, as.character(default_userid))
  }

  observe(domain = domain, label = 'read_session_cookie', {
    user_id <- read_session_cookie(appid, param_name = userid_param_name, session_length = session_length,
                                   domain = domain)
    .set_user_id(domain, user_id)
  })

  return(invisible(NULL))
}


#' Read or Create the Session Cookie
#'
#' The user session contains the user's id and is stored in the client's browser as cookie.
#' This function should not be called directly by itself, but rather through the setup function [setup_user_session].
#'
#' For the session logic to work, the shiny app needs to have [shinyjs] loaded and the *session.js* file included from
#' `system.file('inst', 'js', 'session.js', package = 'stat305templates')`.
#' See the templates as provided by this package how this can be accomplished.
#'
#' @param appid Application id. All applications with the same ID share the same session.
#' @param domain Shiny reactive domain.
#' @param param_name URL query string parameter name to extract the user id from.
#' @param session_length Duration for how long the session is to be active (in days).
#' @return the user id as given in the query string or as saved in the user session.
#'
#' @importFrom shiny getQueryString
#' @importFrom uuid UUIDgenerate
#' @import shinyjs
read_session_cookie <- function (appid, domain = shiny::getDefaultReactiveDomain(), param_name = 'userid',
                                 session_length = 0.125) {
  input_prefix <- 'stat305templates-jscookie_'

  ## Check if the requests specifies user information
  req_userid <- getQueryString(domain)[[param_name]]
  if (!is.null(req_userid)) {
    shinyjs::js$setcookie(name = appid, data = req_userid, expiration = session_length, input_prefix = input_prefix)
    return(req_userid)
  } else {
    input_name <- paste(input_prefix, appid, sep = '')
    shinyjs::js$getcookie(name = appid, input_prefix = input_prefix)

    if (isTRUE(domain$input[[input_name]] == '')) {
      uuid_userid <- UUIDgenerate()
      shinyjs::js$setcookie(name = appid, data = uuid_userid, expiration = session_length, input_prefix = input_prefix)
      return(uuid_userid)
    }
    return(domain$input[[input_name]])
  }
}

.get_user_id <- function (session) {
  tryCatch(get('stat305templates_user_id', envir = session$userData, mode = 'character'), error = function (e) NULL)
}

.set_user_id <- function (session, user_id) {
  session$userData$stat305templates_user_id <- user_id
}
