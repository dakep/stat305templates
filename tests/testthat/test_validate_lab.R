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

test_that("Validate valid file", {
  valid_file <- system.file('tests/valid_signature.md', package = 'stat305templates')
  tmp_dir <- tempfile('render')
  on.exit(unlink(tmp_dir), add = TRUE, after = FALSE)
  dir.create(tmp_dir, mode = '0700')

  render_lab_answers(valid_file, output_dir = tmp_dir)
  rendered_file <- file.path(tmp_dir, 'valid_signature.html')
  expect_true(file.exists(rendered_file))
  expect_valid(rendered_file, valid = TRUE)
})

test_that("Validate an invalid file", {
  invalid_file <- system.file('tests/invalid_signature.md', package = 'stat305templates')
  tmp_dir <- tempfile('render')
  on.exit(unlink(tmp_dir), add = TRUE, after = FALSE)
  dir.create(tmp_dir, mode = '0700')

  render_lab_answers(invalid_file, output_dir = tmp_dir)
  rendered_file <- file.path(tmp_dir, 'invalid_signature.html')
  expect_true(file.exists(rendered_file))
  expect_valid(rendered_file, valid = FALSE)
})
