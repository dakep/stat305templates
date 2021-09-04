if (require(testthat)) {
  library(stat305templates)
  test_check("stat305templates")
} else {
  warning("'stat305templates' requires 'testthat' for tests.")
}
