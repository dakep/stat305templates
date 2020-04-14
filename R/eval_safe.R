#' Enable Safe Evaluation of User Code for `learnr` Exercises
#'
#' @param envir the R environment where the R expression is evaluated.
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
enable_safe_eval <- function (max_fsize = 1L, envir = parent.frame(), max_address_space = 2048L, max_data = 256L,
                              priority = 20L, rlimits, allow_env,
                              default_allow_env = c('^SHELL$', '^USER$', '^LANG$', '^LC_CTYPE$', '^HOME$',
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

  options(tutorial.exercise.evaluator = get_safe_evaluator(priority, envir = envir, rlimits, allow_env))
}

#' Get a function to safely evaluate user code.
#'
#' @param priority,rlimits,allow_env see [enable_safe_eval] for details.
#' @return a `function(expr, timeout)` which evaluates the given expression `expr` safely in at most `timeout` seconds.
#'
#' @importFrom unix eval_safe
#' @importFrom shiny div
get_safe_evaluator <- function (priority, envir, rlimits, allow_env) {
  # Clear the environment before evaluating the call.
  outer_envir <- new.env()
  outer_envir$priority <- priority
  outer_envir$allow_env <- allow_env
  outer_envir$clear_env <- function () {
    envvars <- names(Sys.getenv())
    to_remove <- matrix(unlist(lapply(allow_env, function (reg) {
      !grepl(reg, envvars, ignore.case = TRUE, perl = TRUE)
    }), use.names = FALSE, recursive = FALSE), ncol = length(allow_env))
    to_remove <- apply(to_remove, 1, all)

    Sys.unsetenv(envvars[to_remove])
  }
  outer_envir$envir <- envir

  return(function (expr, timeout) {
    result <- NULL
    outer_envir$expr <- substitute(expr)
    outer_envir$timeout <- timeout

    # Pull learnr stuff into outer environment
    outer_envir$exercise <- parent.frame()$exercise
    outer_envir$evaluate_exercise <- learnr:::evaluate_exercise

    return(list(
      start = function() {
        tryCatch(
          result <<- evalq(eval_safe({
            clear_env()
            tryCatch({
              .setup <- parse(text = trimws(exercise$options$exercise.checker, whitespace = '"'))
              add_to_envir <- eval(.setup, envir)
              for (newvar in names(add_to_envir)) {
                assign(newvar, add_to_envir[[newvar]], envir)
              }
              rm(.setup, add_to_envir)
            }, error = function (e) {})
            eval(expr)
          }, timeout = timeout, priority = priority, rlimits = rlimits), envir = outer_envir),
          error = function (e) {
            result <<- list(feedback = NULL,
                            error_message = as.character(e),
                            html_output = div(class = 'alert alert-danger', role = 'alert', as.character(e)))
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
