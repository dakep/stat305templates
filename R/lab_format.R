#' Online Lab Format
#'
#'
#' @export
#' @importFrom rmarkdown html_document
#' @importFrom htmltools htmlDependency
#' @importFrom utils packageVersion
#' @importFrom knitr opts_chunk opts_hooks
lab <- function (...) {
  output_format <- html_document(section_divs = FALSE, ...)
  render_env <- environment(output_format$intermediates_generator)
  render_env$extra_dependencies <- append(render_env$extra_dependencies, list(
    htmlDependency('lab', version = packageVersion('stat305templates'),
                   src = system.file('rmarkdown/templates/lab/resources', package = 'stat305templates'),
                   script = 'lab.js', stylesheet = 'lab.css'),
    htmlDependency('clientStore', version = packageVersion('stat305templates'),
                   src = system.file('srcjs', package = 'stat305templates'), script = 'clientStore.js')))

  output_format$knitr$opts_chunk$stat305templates.lab <- TRUE
  output_format$knitr$opts_hooks$stat305templates.lab <- function (options) {
    initialize_lab()
    return (options)
  }

  return(output_format)
}

#' @importFrom knitr opts_knit knit_meta_add
#' @importFrom rmarkdown metadata shiny_prerendered_chunk html_dependency_jquery html_dependency_bootstrap
initialize_lab <- function (...) {
  if (isTRUE(getOption('knitr.in.progress')) && !isTRUE(opts_knit$get('stat305templates.lab.initialized'))) {
    knit_meta_add(list(html_dependency_jquery()))

    # Add tutorial id
    rm_ns <- getNamespace('rmarkdown')
    unlockBinding('metadata', rm_ns)
    rm_ns$metadata$tutorial <- rm_ns$metadata$lab
    on.exit(lockBinding('metadata', rm_ns))

    shiny_prerendered_chunk('server', sprintf('stat305templates:::.initialize_lab_server(session, metadata = %s)',
                                              dput_object(rmarkdown::metadata$lab)), singleton = TRUE)

    shiny_prerendered_chunk('server-start', 'stat305templates:::.generate_lab_key()', singleton = TRUE)

    opts_knit$set(stat305templates.lab.initialized = TRUE)
  }
}


#' @importFrom shinyjs useShinyjs runjs
#' @importFrom utils URLencode
.initialize_lab_server <- function (session, metadata) {
  useShinyjs(html = TRUE)

  lab_id <- .get_lab_id(metadata)
  set_session_data('lab_metadata', metadata)
  set_session_data('lab_id', lab_id)

  observe({
    runjs(sprintf('if (window.lab) { window.lab.setLabId("%s") } else { window.labId = "%s" }', lab_id, lab_id))
  })
}


#' @importFrom openssl ed25519_keygen
.generate_lab_key <- function () {
  if (is.null(getOption('stat305templates.lab.keypair'))) {
    options(stat305templates.lab.keypair = ed25519_keygen())
  }
}

#' @importFrom rmarkdown metadata
.get_lab_id <- function(metadata) {
  if (missing(metadata)) {
    metadata <- rmarkdown::metadata$lab
  }
  URLencode(paste(metadata$id %||% 'lab', metadata$version %||% '1', sep = '-'), reserved = TRUE)
}

#' Return the lab ID on the server
#'
#' @return the lab ID or NULL if not available.
#' @export
get_lab_id <- function () {
  get_session_data('lab_id', asis = TRUE)
}


#' Add a text input for the user's name
#'
#' @param title title for the panel containing the text input.
#' @param label label of the text input.
#' @export
lab_name_input <- function (title = "Student information", label = "Your name", label_id = 'Your student nr.') {
  name_input <- list(label = label, label_id = label_id, title = title)
  class(name_input) <- 'lab_name_input'
  return(name_input)
}

#' Add a button to submit the lab.
#'
#' A name input needs to be placed *before* the submit button using [lab_name_input()].
#'
#' @param label button label
#' @param filename name of the file offered to the user for download. Defaults to the title of the Rmd document.
#' @export
#' @importFrom htmltools tags
#' @importFrom knitr opts_current
submit_lab_btn <- function (label = "Download answers", filename = NULL) {
  submit_lab <- list(
    label = label,
    filename = filename
  )

  class(submit_lab) <- 'submit_lab_btn'
  return(submit_lab)
}

#' @inheritParams knitr::knit_print
#' @importFrom htmltools div tags
#' @importFrom knitr knit_print opts_knit opts_chunk
#' @importFrom shiny NS textInput
#' @importFrom rmarkdown shiny_prerendered_chunk
#' @method knit_print lab_name_input
#' @rdname knit_print
#' @export
knit_print.lab_name_input <- function (x, ...) {
  ns <- NS(random_ui_id(opts_current$get('label')))
  opts_chunk$set('stat305templates.lab_name_ns' = ns(NULL))
  ui <- div(id = ns('name-input-panel'), class = 'panel panel-default',
            div(class = 'panel-heading', tags$h4(x$title)),
            div(class = 'panel-body lab-student-name',
                textInput(ns('student-name'), label = x$label, placeholder = x$label),
                textInput(ns('student-nr'), label = x$label_id, placeholder = x$label_id),
                div(class = 'alert alert-danger hidden')))
  knit_print(ui)
}


#' @inheritParams knitr::knit_print
#' @importFrom htmltools div
#' @importFrom knitr knit_print opts_knit opts_current
#' @importFrom shiny NS downloadButton
#' @importFrom rmarkdown shiny_prerendered_chunk
#' @method knit_print submit_lab_btn
#' @rdname knit_print
#' @export
knit_print.submit_lab_btn <- function (x, ...) {
  ns <- opts_current$get('stat305templates.lab_name_ns')
  if (is.null(ns)) {
    stop("lab_name_input() needs to be placed before submit_lab_btn()")
  }
  ns <- NS(ns)
  x$id <- ns(NULL)
  ui <- div(downloadButton(ns('download'), label = x$label, class = 'btn-primary btn-lg submit-lab'),
            class = 'text-center')
  shiny_prerendered_chunk('server', code = sprintf('stat305templates:::.submit_lab_btn_prerendered_chunk(%s)',
                                                   dput_object(x)))
  knit_print(ui)
}

#' @importFrom methods is
#' @importFrom stringr str_replace_all fixed
#' @importFrom openssl ed25519_keygen ed25519_sign base64_encode
#' @importFrom htmltools div
.submit_lab_btn_prerendered_chunk <- function (options) {
  global_session <- getDefaultReactiveDomain()

  moduleServer(options$id, function (input, output, session) {
    keypair <- getOption('stat305templates.lab.keypair') %||% ed25519_keygen()
    url <- isolate(sprintf('%s//%s%s%s', session$clientData$url_protocol, session$clientData$url_hostname,
                           session$clientData$url_pathname, session$clientData$url_search))

    output$download <- downloadHandler(filename = 'lab_answers.md', content = function (fname) {
      fh <- file(fname, open = 'w', encoding = 'UTF-8')
      on.exit(close(fh), add = TRUE)

      student_name <- isolate(input[['student-name']])
      student_id <- isolate(input[['student-nr']])
      input_values <- isolate(reactiveValuesToList(global_session$input))

      rendered_inputs <- lapply(names(input_values), function (input_name) {
        if (input_name %in% c(session$ns('student-name'), session$ns('student-nr'))) {
          return(NULL)
        }
        raw_value <- input_values[[input_name]]
        label <- input_name
        if (is(raw_value, 'list') && !is.null(raw_value$code)) {
          # Exercise chunk
          answer <- tryCatch({
            global_session$getOutput(sprintf('tutorial-exercise-%s-output', raw_value$label))(global_session)$html
          }, error = function (e) {
            '```\n### Unable to evaluate answer\n```'
          })
          value <- paste('```', str_replace_all(raw_value$code, fixed('```'), "'''"), '```\n#### Result',
                         answer, '\n', sep = '\n')
          label <- raw_value$label
        } else {
          # Not an exercise chunk.
          qp_labels <- get_session_data(sprintf('question_pool-labels-%s', input_name), asis = TRUE)
          value <- if (!is.null(qp_labels)) {
            # Multiple choice or single choice question from a question pool
            selected <- sort(raw_value)
            paste(sprintf('- %s [%s]', qp_labels[selected], selected), collapse = '\n')
          } else {
            # Something else...
            sprintf('```\n%s\n```\n', raw_value)
          }
        }
        list(name = label, value = value)
      })

      rendered_inputs <- rendered_inputs[!sapply(rendered_inputs, is.null)]
      rendered_inputs <- rendered_inputs[order(sapply(rendered_inputs, `[[`, 'name'))]

      cat('---\nstudent: ', student_name,
          '\nstudent_id: ', student_id,
          '\nurl: ', url,
          '\npubkey: ', base64_encode(keypair$pubkey$data),
          '\n---\n', file = fh, sep = '')

      cat('# Answers\n', file = fh)
      bytes <- lapply(rendered_inputs, function (input) {
        lines <- paste('## ', input$name, '\n', input$value, '\n\n', sep = '')
        cat(lines, file = fh)
        return(charToRaw(lines))
      })

      bytes <- c(list(charToRaw(paste('student:', student_name)),
                      charToRaw(paste('student_id:', student_id)),
                      charToRaw(paste('url:', url))), bytes)

      bytes <- unlist(bytes, recursive = FALSE, use.names = FALSE)

      cat('---\nsignature: ', base64_encode(ed25519_sign(bytes, key = keypair)), '\n---\n', file = fh, sep = '')
    }, contentType = 'text/plain; charset=UTF-8')
  })

  invisible(TRUE)
}

#' Render and Validate Lab Answers
#'
#' Renders and validates lab answers generated by and downloaded from a lab tutorial.
#'
#' @param filename path to the lab answer file.
#' @param output_dir the output directory for the rendered answer file. If `NULL`, same as the directory of the input
#'  file.
#' @export
#'
#' @importFrom rmarkdown render html_document
#' @importFrom stringr str_sub
render_lab_answers <- function (filename, output_dir = NULL) {
  validation_message <- tryCatch({
    .validate_lab_answers(filename)
  }, error = function (e) {
    sprintf('<div class="alert alert-danger text-center"><p class="lead">Validation failed!</p><p>%s</p></div>', e)
  })

  new_md <- tempfile(pattern = 'lab_answers', fileext = '.md')
  on.exit(unlink(new_md), add = TRUE)
  new_md_fh <- file(new_md, open = 'w')
  on.exit(close(new_md_fh), add = TRUE, after = FALSE)

  in_fh <- file(filename, open = 'r')
  on.exit(close(in_fh), add = TRUE, after = FALSE)

  ## Read metadata
  metadata <- .read_lab_answers_metadata(in_fh)

  cat('---\ntitle: Lab Answers for *', metadata$student_name,
      '*\nauthor: ', basename(filename), ' from ', metadata$url, '\n---\n', file = new_md_fh, sep = '')

  if (!isTRUE(validation_message)) {
    cat(validation_message, file = new_md_fh)
  }

  repeat {
    line <- .read_line(in_fh, n = 1L)
    if (length(line) == 0L) {
      break
    }
    cat(line, '\n', file = new_md_fh)
  }


  if (is.null(output_dir)) {
    output_dir <- dirname(filename)
  }

  render(new_md, output_file = str_sub(basename(filename), end = -4L),
         output_format = html_document(), output_dir = output_dir, quiet = TRUE)
}

#' @importFrom stringr str_sub
#' @importFrom openssl ed25519_verify base64_decode
#' @importFrom rlang abort with_abort
.validate_lab_answers <- function (filename) {
  fh <- file(filename, open = 'r', encoding = 'UTF-8')
  on.exit(close(fh), add = TRUE)

  msg_bytes <- vector('list', 2L)
  signature <- NULL

  ## Read metadata
  metadata <- .read_lab_answers_metadata(fh)

  ## Add metadata info to message
  msg_bytes[[1L]] <- charToRaw(paste('student:', metadata$student_name))
  msg_bytes[[2L]] <- charToRaw(paste('student_id:', metadata$student_id))
  msg_bytes[[3L]] <- charToRaw(paste('url:', metadata$url))

  # line 6: skip
  .read_line(fh, n = 1L, ok = FALSE)

  # all next lines: read as message
  repeat {
    line <- .read_line(fh, n = 1L)

    if (length(line) == 0L) {
      break
    }

    # Add the newline (0x0a) at the end!
    msg_bytes <- c(msg_bytes, list(c(charToRaw(line), as.raw(0x0a))))
  }

  # The last three lines are not part of the message.
  # Extract the signature from the second to last line.
  signature_line <- rawToChar(msg_bytes[[length(msg_bytes) - 1L]])
  if (str_sub(signature_line, end = 11L) != 'signature: ') {
    abort("No signature found in file.")
  }

  # Verify the signature (throws an error if the validation fails)
  with_abort(ed25519_verify(unlist(msg_bytes[seq_len(length(msg_bytes) - 3L)]),
                            sig = base64_decode(str_sub(signature_line, start = 12L)),
                            pubkey = metadata$pubkey))
  return(TRUE)
}

#' @importFrom rlang with_abort
.read_line <- function (...) {
  mc <- match.call(expand.dots = TRUE)
  mc[[1L]] <- quote(readLines)
  with_abort(eval.parent(mc))
}

#' @importFrom openssl read_ed25519_pubkey base64_decode
#' @importFrom stringr str_sub
#' @importFrom rlang abort
.read_lab_answers_metadata <- function (fh) {
  seek(fh, 0L, rw = 'read', origin = 'start')

  info <- list(student_name = NULL, student_id = NULL, url = NULL, pubkey = NULL)
  ## Read metadata
  # line 1: skip
  .read_line(fh, n = 1L, ok = FALSE)
  # line 2: student name -- add to message
  info$student_name <- str_sub(.read_line(fh, n = 1L, ok = FALSE), start = 10L)
  # line 3: student nr -- add to message
  info$student_id <- str_sub(.read_line(fh, n = 1L, ok = FALSE), start = 13L)
  # line 4: url -- add to message
  info$url <- str_sub(.read_line(fh, n = 1L, ok = FALSE), start = 6L)
  # line 5: public key
  info$pubkey <- tryCatch(
    read_ed25519_pubkey(base64_decode(str_sub(.read_line(fh, n = 1L, ok = FALSE), start = 9L))),
    error = function (e) {
      abort("Public key cannot be read.")
    })

  # line 6: skip
  .read_line(fh, n = 1L, ok = FALSE)
  return(info)
}
