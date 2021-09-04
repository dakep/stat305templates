if (require(testthat)) {
  library(pyinit)
  test_check("stat305templates")
} else {
  warning("'stat305templates' requires 'testthat' for tests.")
}
