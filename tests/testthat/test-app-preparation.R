test_that("both actual base-data bodies coerce, filter and order fabricated rows", {
  for (path in c("R/app.R", "app/app.R")) {
    env <- new.env(parent = globalenv())
    env$dataset <- data.frame(date = c("2025-04-01", "2025-01-01", "2025-04-01",
      "2025-04-01", NA_character_, "2025-01-01"),
      retailer = factor(c("Invented A", "Invented A", "Invented B", "National Total", "Invented C", "Invented D")),
      value = c("12", "0", "suppressed", "12", "99", "5"))
    out <- eval(app_reactive_body(path, "base_data"), env)
    expect_equal(out$retailer, c("Invented A", "Invented D", "Invented A"), info = path)
    expect_equal(out$value, c(0, 5, 12), info = path)
    expect_equal(out$date, as.Date(c("2025-01-01", "2025-01-01", "2025-04-01")), info = path)
    expect_type(out$retailer, "character")
    env$base_data <- function() out
    status <- eval(app_reactive_body(path, "retailer_status"), env)
    expect_equal(status$startup[match(c("Invented A", "Invented D"), status$retailer)], c(TRUE, FALSE), info = path)
    expect_equal(status$inactive[match(c("Invented A", "Invented D"), status$retailer)], c(FALSE, TRUE), info = path)
  }
})

test_that("empty selections produce the actual Shiny validation condition", {
  for (path in c("R/app.R", "app/app.R")) {
    env <- app_test_env(path, list(tier1_mode = "All", inactive_mode = "All",
      startup_mode = "All", pick_retailer = "Invented absent retailer"))
    expect_error(eval(app_reactive_body(path, "filtered_data"), env),
      "No data after filters", class = "validation", info = path)
  }
})
