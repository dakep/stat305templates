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


#' @importFrom openssl ec_keygen
.generate_lab_key <- function () {
  if (is.null(getOption('stat305templates.lab.keypair'))) {
    options(stat305templates.lab.keypair = ec_keygen())
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


#' Add a text input for the user's name and id
#'
#' @param title title for the panel containing the name input.
#' @param label label of the name input.
#' @param label_id label for the input for the student number (id). If `NULL`, the input
#'   will not be shown.
#' @param error_name_empty,error_id_wrong_length,error_id_wrong_format error messages for
#'   an empty name input, or a wrong id input. If the error message is `NULL`, the check
#'   is disabled.
#' @export
lab_name_input <- function (title = "Student information", label = "Your name",
                            label_id = 'Your student nr.',
                            error_name_empty = "Your name must not be empty.",
                            error_id_wrong_length = "The student number must be 8 digits.",
                            error_id_wrong_format = "The student number must be 8 digits.") {
  name_input <- list(label = label, label_id = label_id, title = title,
                     error_name_empty = error_name_empty,
                     error_id_wrong_length = error_id_wrong_length,
                     error_id_wrong_format = error_id_wrong_format)

  class(name_input) <- 'lab_name_input'
  return(name_input)
}

#' Add a button to submit the lab.
#'
#' A name input needs to be placed *before* the submit button using [lab_name_input()].
#'
#' @param label button label
#' @param filename name of the file offered to the user for download. The file extension is irrelevant.
#' @param require_validation create a markdown file with validation information.
#' @param render_html directly render the lab as HTML instead of outputting a markdown file.
#'   If `TRUE`, validation metadata cannot be included.
#' @export
#' @importFrom htmltools tags
#' @importFrom knitr opts_current
submit_lab_btn <- function (label = "Download answers",
                            filename = 'lab_answers.md',
                            require_validation = TRUE,
                            render_html = FALSE) {
  if (isTRUE(require_validation) && isTRUE(render_html)) {
    stop("HTML output is not supported if validation is required")
  }

  submit_lab <- list(
    label = label,
    filename = filename,
    require_validation = isTRUE(require_validation),
    render_html = isTRUE(render_html)
  )

  class(submit_lab) <- 'submit_lab_btn'
  return(submit_lab)
}

#' @inheritParams knitr::knit_print
#' @importFrom htmltools div tags
#' @importFrom knitr knit_print opts_knit opts_chunk
#' @importFrom shiny NS textInput
#' @importFrom rmarkdown shiny_prerendered_chunk
#' @importFrom jsonlite toJSON
#' @method knit_print lab_name_input
#' @rdname knit_print
#' @export
knit_print.lab_name_input <- function (x, ...) {
  ns <- NS(random_ui_id(opts_current$get('label')))

  error_msgs <- list('studentNameEmpty' = x$error_name_empty)

  nr_input <- textInput(ns('student-nr'), label = x$label_id, placeholder = x$label_id)
  if (is.null(x$label_id)) {
    nr_input <- div(class = "hidden", nr_input)
  } else {
    error_msgs$studentIdWrongLength <- x$error_id_wrong_length
    error_msgs$studentIdWrongFormat <- x$error_id_wrong_format
  }


  opts_chunk$set('stat305templates.lab_name_ns' = ns(NULL))
  ui <- div(id = ns('name-input-panel'), class = 'panel panel-default',
            div(class = 'panel-heading', tags$h4(x$title)),
            div(class = 'panel-body lab-student-name',
                textInput(ns('student-name'), label = x$label, placeholder = x$label),
                nr_input,
                div(class = 'alert alert-danger hidden',
                    `data-messages` = toJSON(error_msgs, auto_unbox = TRUE, null = 'null'))))
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
#' @importFrom stringr str_replace_all fixed str_split
#' @importFrom openssl ec_keygen base64_encode write_der signature_create
#' @importFrom digest digest hmac
#' @importFrom htmltools div
#' @importFrom shiny isolate reactiveValues moduleServer downloadHandler
.submit_lab_btn_prerendered_chunk <- function (options) {
  global_session <- getDefaultReactiveDomain()

  moduleServer(options$id, function (input, output, session) {
    if (options$require_validation) {
      keypair <- getOption('stat305templates.lab.keypair') %||% ec_keygen()
    }

    url <- isolate(sprintf('%s//%s%s%s', session$clientData$url_protocol, session$clientData$url_hostname,
                           session$clientData$url_pathname, session$clientData$url_search))

    output$download <- downloadHandler(filename = options$filename, content = function (fname) {
      # Create the markdown file.
      md_fname <- tempfile(fileext = 'md')
      on.exit(unlink(md_fname), add = TRUE)
      fh <- file(md_fname, open = 'w', encoding = 'UTF-8')
      on.exit(close(fh), add = TRUE)

      student_name <- isolate(input[['student-name']])
      student_id <- isolate(input[['student-nr']])
      input_values <- isolate(reactiveValuesToList(global_session$input))

      if (is.null(student_id)) {
        student_id <- ''
      }

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
            # Something else... Need to ensure only a single character is returned (e.g., for inputs with multiple
            # elements)
            sprintf('```\n%s\n```\n', paste(raw_value, collapse = ', '))
          }
        }
        list(name = label, value = value)
      })

      rendered_inputs <- rendered_inputs[!sapply(rendered_inputs, is.null)]
      rendered_inputs <- rendered_inputs[order(sapply(rendered_inputs, `[[`, 'name'))]

      metadata_hash <- .create_metadata_hash(list(student_name = student_name, student_id = student_id, url = url))

      if (options$require_validation) {
        metadata <- paste('---\nstudent: ', student_name,
                          '\nstudent_id: ', student_id,
                          '\nurl: ', url,
                          '\npubkey: ', base64_encode(write_der(keypair$pubkey)),
                          '\nsignature: ', base64_encode(signature_create(metadata_hash, hash = NULL, key = keypair)),
                          ';', sep = '')
        signature_pos <- nchar(metadata)

        # Leave enough room for the signature.
        cat(metadata, rep(' ', 120), '\n---\n', file = fh, sep = '')
      } else {
        cat('---\ntitle: ',
            sprintf("Lab Answers for *%s* (%s)", student_name, student_id),
            '\nauthor: ', url,
            '\n---\n\n',
            file = fh, sep = '')
      }
      cat('# Answers\n', file = fh)
      running_hash <- NULL
      for (input in rendered_inputs) {
        running_hash <- .digest_cat('## ', input$name, '\n', input$value, '\n\n', prev = running_hash, file = fh)
      }
      running_hash <- .digest_cat(prev = running_hash, file = fh, finalize = TRUE)

      if (options$require_validation) {
        # Re-position at the site of the signature
        seek(fh, where = signature_pos, origin = 'start', rw = 'w')
        cat(base64_encode(signature_create(running_hash$hash, hash = NULL, key = keypair)), file = fh, sep = '')
      }

      if (options$render_html) {
        out_dir <- tempfile('rmarkdown-out')
        dir.create(out_dir, showWarnings = FALSE, mode = '0700')
        on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

        rendered_file <- render(
          md_fname,
          output_file = 'rendered.html',
          output_format = html_document(self_contained = TRUE),
          output_dir = out_dir,
          envir = new.env(),
          quiet = TRUE)

        file.rename(rendered_file, fname)
      } else {
        file.rename(md_fname, fname)
      }
    }, contentType = 'application/octet-stream')
  })

  invisible(TRUE)
}

#' @importFrom stringr str_sub str_remove_all
#' @importFrom digest hmac digest
.digest_cat <- function (..., prev, file, blocksize = 8192L, finalize = FALSE) {
  if (missing(prev) || is.null(prev)) {
    prev <- list(hash = raw(0L), leftover = '')
  }

  new_hash_data <- if (isTRUE(finalize)) {
    # ignore ... and print all remaining bytes
    blocksize <- nchar(prev$leftover)
    prev$leftover
  } else {
    # Pipe everything to the output file.
    cat(..., file = file, sep = '')
    # only hash non-whitespace characters!
    paste(prev$leftover, str_remove_all(paste(..., sep = ''), '\\s'), sep = '')
  }

  chunks <- nchar(new_hash_data) %/% blocksize
  bytes_left <- nchar(new_hash_data) %% blocksize
  for (i in seq_len(chunks)) {
    chunk <- str_sub(new_hash_data, start = (i - 1L) * blocksize + 1L, end = i * blocksize)
    prev$hash <- hmac(prev$hash, chunk, serialize = FALSE, raw = TRUE, algo = 'sha256')
  }

  if (isTRUE(finalize)) {
    prev$leftover <- ''
  } else {
    prev$leftover <- str_sub(new_hash_data, start = -bytes_left)
  }
  return(prev)
}
