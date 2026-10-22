# Run from the repository root with Rscript --vanilla tests/run-tests.R.
if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Install testthat in a compatible R library before running the checks.")
}
testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)
