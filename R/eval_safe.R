#' Enable Safe Evaluation of User Code for `learnr` Exercises
#'
#' @param max_fsize maximum size of created files in MiB.
#' @param max_address_space maximum size of the address space in MiB.
#' @param max_data maximum size of the data memory in MiB.
#' @param priority priority of the evaluating process.
#' @param rlimits specify custom rlimitis.
#' @param allow_env vector of regular expressions of allowed environment variables (i.e., the matching variables will
#'                  be retained).
#' @param default_allow_env vector of regular expressions of environment variables which should be allowed by default.
#'                          Only change if you know what you are doing, as most of them are fundamental to the R process
#'                          to work.
#' @export
enable_safe_eval <- function (max_fsize = 1L, max_address_space = 2048L, max_data = 256L, priority = 20L, rlimits,
                              allow_env, default_allow_env = c('^SHELL$', '^USER$', '^LANG$', '^LC_CTYPE$', '^HOME$',
                                                               '^DYLD_FALLBACK_LIBRARY_PATH$', '^R_.*')) {
  if (missing(rlimits)) {
    mb <- 1024 * 1024
    rlimits <- c(fsize = max_fsize * mb, as = max_address_space * mb, data = max_data * mb)
  }
  allow_env <- if (missing(allow_env)) {
    default_allow_env
  } else {
    c(allow_env, default_allow_env)
  }

  options(tutorial.exercise.evaluator = get_safe_evaluator(priority, rlimits, allow_env))
}

#' Get a function to safely evaluate user code.
#'
#' @param priority,rlimits,allow_env see [enable_safe_eval] for details.
#' @return a `function(expr, timeout)` which evaluates the given expression `expr` safely in at most `timeout` seconds.
#'
#' @importFrom unix eval_safe
#' @importFrom shiny div
get_safe_evaluator <- function (priority, rlimits, allow_env) {
  # Clear the environment before evaluating the call.
  clear_env <- function () {
    envvars <- names(Sys.getenv())
    to_remove <- matrix(unlist(lapply(allow_env, function (reg) {
      !grepl(reg, envvars, ignore.case = TRUE, perl = TRUE)
    }), use.names = FALSE, recursive = FALSE), ncol = length(allow_env))
    to_remove <- apply(to_remove, 1, all)

    Sys.unsetenv(envvars[to_remove])
  }

  return(function (expr, timeout) {
    result <- NULL
    return(list(
      start = function() {
        tryCatch(
          result <<- eval_safe({
            clear_env()
            force(expr)
          }, timeout = timeout, priority = priority, rlimits = rlimits),
          error = function (e) {
            error_message <- sprintf("Resource limits exceeded (%s).", e)
            result <<- list(feedback = NULL,
                            error_message = error_message,
                            html_output = div(class = 'alert alert-danger', role = 'alert', error_message))
            warning("Resource limits exceeded. Can not evaluate code.", e)
          })
      },
      completed = function() {
        return(TRUE)
      },
      result = function() {
        return(result)
      }
    ))
  })
}
