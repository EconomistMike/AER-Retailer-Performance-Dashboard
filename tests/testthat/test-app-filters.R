
test_that("both app sources classify fabricated retailer histories consistently", {
  for (path in c("R/app.R", "app/app.R")) {
    env <- app_test_env(path)
    status <- env$retailer_status()
    expect_equal(status$inactive[match(c("Invented A", "Invented B", "Invented C"), status$retailer)],
      c(FALSE, TRUE, TRUE), info = path)
    expect_equal(status$startup[match(c("Invented A", "Invented B", "Invented C"), status$retailer)],
      c(TRUE, FALSE, FALSE), info = path)
    expect_equal(status$tier1[match(c("AGL", "Invented A"), status$retailer)], c(TRUE, FALSE), info = path)
    expect_false(anyDuplicated(status$retailer) > 0L)
  }
})

test_that("app modes and retailer selection combine with AND without multiplying rows", {
  for (path in c("R/app.R", "app/app.R")) {
    env <- app_test_env(path, list(tier1_mode = "All", inactive_mode = "Exclude",
      startup_mode = "Include only", pick_retailer = c("Invented A", "Invented D", "AGL")))
    out <- eval(app_reactive_body(path, "filtered_data"), env)
    expect_equal(out$retailer, rep("Invented A", 2), info = path)
    expect_equal(out$value, c(0, 10), info = path)
    expect_false(anyDuplicated(out[c("retailer", "date")]) > 0L)
  }
})


test_that("each Include or Exclude mode changes the set without retailer selection", {
  cases <- list(
    list(column = "tier1_mode", mode = "Include only", expected = "AGL"),
    list(column = "tier1_mode", mode = "Exclude",
      expected = c("Invented A", "Invented B", "Invented C", "Invented D", "Invented E")),
    list(column = "inactive_mode", mode = "Include only",
      expected = c("Invented B", "Invented C", "Invented D")),
    list(column = "inactive_mode", mode = "Exclude", expected = c("Invented A", "AGL", "Invented E")),
    list(column = "startup_mode", mode = "Include only", expected = c("Invented A", "Invented D", "Invented E")),
    list(column = "startup_mode", mode = "Exclude", expected = c("Invented B", "Invented C", "AGL")))
  for (path in c("R/app.R", "app/app.R")) {
    for (case in cases) {
      input <- list(tier1_mode = "All", inactive_mode = "All", startup_mode = "All", pick_retailer = NULL)
      input[[case$column]] <- case$mode
      env <- app_test_env(path, input)
      out <- eval(app_reactive_body(path, "filtered_data"), env)
      expect_setequal(unique(out$retailer), case$expected)
      expect_equal(nrow(out), sum(synthetic_app_data()$retailer %in% case$expected))
    }
  }
})
