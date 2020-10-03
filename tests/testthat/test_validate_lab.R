library(testthat)
library(stat305templates)

expect_valid <- function (object, valid = TRUE) {
  # 1. Capture object and label
  act <- quasi_label(rlang::enquo(object), arg = "object")

  # 2. Call expect()
  rendered_fh <- file(act$val, open = 'rt')
  on.exit(close(rendered_fh), add = TRUE, after = FALSE)
  repeat {
    next_line <- readLines(rendered_fh, n = 1L)
    if(length(next_line) != 1L) {
      fail("Rendered file is invalid.")
    }
    if (next_line == '<body>') {
      break
    }
  }
  rendered_html <- paste(readLines(rendered_fh), collapse = '\n')
  match_failed_valid <- grepl('<p class="lead">\\s+Validation failed!\\s+</p>', rendered_html)
  if (isTRUE(valid)) {
    expect(!match_failed_valid, "Validation has failed.")
  } else {
    expect(match_failed_valid, "Validation has not failed.")
  }

  # 3. Invisibly return the value
  invisible(act$val)
}

test_that("Validate valid files", {
  test_files_path <- system.file('tests', package = 'stat305templates')

  test_files <- list.files(test_files_path, pattern = '^valid_', full.names = TRUE)
  tmp_dir <- tempfile('render')
  on.exit(unlink(tmp_dir), add = TRUE, after = FALSE)
  dir.create(tmp_dir, mode = '0700')

  for (file in test_files) {
    rendered <- render_lab_answers(file, output_dir = tmp_dir)
    expect_true(file.exists(rendered$output))
    expect_match(rendered$output, regexp = '\\.html$')
    expect_valid(rendered$output, valid = TRUE)
  }
})

test_that("Validate an invalid file", {
  test_files_path <- system.file('tests', package = 'stat305templates')

  test_files <- list.files(test_files_path, pattern = '^invalid_', full.names = TRUE)
  tmp_dir <- tempfile('render')
  on.exit(unlink(tmp_dir), add = TRUE, after = FALSE)
  dir.create(tmp_dir, mode = '0700')

  for (file in test_files) {
    rendered <- render_lab_answers(file, output_dir = tmp_dir)
    expect_true(file.exists(rendered$output))
    expect_match(rendered$output, regexp = '\\.html$')
    expect_valid(rendered$output, valid = FALSE)
  }
})
