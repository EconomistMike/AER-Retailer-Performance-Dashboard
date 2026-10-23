source(testthat::test_path("..", "..", "checks", "snapshot-quality.R"))

invented_snapshots <- function() {
  dates <- as.Date(c("2024-07-01", "2024-10-01", "2025-01-01", "2025-04-01"))
  out <- data.frame(date = rep(dates, each = 3),
    year_quarter = rep(paste0("2024-25 Q", 1:4), each = 3),
    quarter_number = rep(1:4, each = 3), financial_year = "2024-25",
    retailer = rep(c("Invented A", "Invented B", "National Total"), 4),
    metric = "Number", customer_type = "Electricity", customer_group = "Residential",
    jurisdiction = "National", value = c(10, 20, 30, 0, 25, 25, 14, 21, 35, 16, 24, 40))
  out
}

expect_snapshot_issue <- function(data, field) {
  out <- remp_snapshot_issues(data)
  expect_true(any(out$field == field), info = field)
  expect_error(assert_remp_snapshots(data), class = "remp_snapshot_error")
}

test_that("invented quarterly snapshots validate without mutation or summing time", {
  x <- invented_snapshots(); before <- serialize(x, NULL)
  expect_true(assert_remp_snapshots(x))
  expect_equal(nrow(remp_snapshot_issues(x)), 0L)
  expect_identical(serialize(x, NULL), before)
  expect_true(assert_remp_snapshots(x[rev(seq_len(nrow(x))), ]))
})

test_that("schema and empty inputs fail explicitly", {
  x <- invented_snapshots(); x$value <- NULL
  expect_snapshot_issue(x, "value")
  x <- invented_snapshots(); names(x)[2] <- "date"
  expect_snapshot_issue(x, "columns")
  expect_snapshot_issue(list(), "schema")
  expect_snapshot_issue(invented_snapshots()[FALSE, ], "rows")
  x <- invented_snapshots(); x$retailer <- as.factor(x$retailer)
  expect_snapshot_issue(x, "retailer")
})

test_that("customer counts require non-negative finite whole numbers", {
  for (bad in list(-1, 0.5, Inf, NaN, NA_real_, "10 customers")) {
    x <- invented_snapshots(); x$value[1] <- bad
    expect_snapshot_issue(x, "value")
  }
})

test_that("date and fiscal metadata must describe the same quarter", {
  x <- invented_snapshots(); x$date <- as.character(x$date)
  expect_snapshot_issue(x, "date")
  x <- invented_snapshots(); x$date[1] <- as.Date("2024-08-01")
  expect_snapshot_issue(x, "date")
  x <- invented_snapshots(); x$date[1] <- as.Date(NA)
  expect_snapshot_issue(x, "date")
  x <- invented_snapshots(); x$quarter_number[1] <- 4L
  expect_snapshot_issue(x, "quarter_number")
  x <- invented_snapshots(); x$financial_year[1] <- "2024-29"
  expect_snapshot_issue(x, "financial_year")
  x <- invented_snapshots(); x$year_quarter[1] <- "2024-25 Q4"
  expect_snapshot_issue(x, "year_quarter")
})

test_that("duplicate keys and missing or dirty retailer identifiers are rejected", {
  x <- invented_snapshots(); x <- rbind(x, x[1, ])
  expect_snapshot_issue(x, "keys")
  for (bad in c("", " Invented A ", NA_character_)) {
    x <- invented_snapshots(); x$retailer[1] <- bad
    expect_snapshot_issue(x, "retailer")
  }
})

test_that("labels are checked without pretending to recover sheet identity", {
  for (field in c("metric", "customer_type", "customer_group", "jurisdiction")) {
    x <- invented_snapshots(); x[[field]][1] <- "unexpected"
    expect_snapshot_issue(x, field)
  }
})

test_that("national totals must exist once and reconcile within each snapshot", {
  x <- invented_snapshots(); x$value[3] <- 31
  expect_snapshot_issue(x, "total")
  x <- invented_snapshots(); x <- x[x$retailer != "National Total", ]
  expect_snapshot_issue(x, "total")
  x <- invented_snapshots(); x <- x[x$retailer == "National Total", ]
  expect_snapshot_issue(x, "total")
  x <- invented_snapshots(); x <- rbind(x, x[3, ])
  expect_snapshot_issue(x, "total")
})

test_that("independent expected keys detect dropped quarters or retailer rows", {
  x <- invented_snapshots(); expected <- x[c("retailer", "date")]
  expect_true(assert_remp_snapshots(x, expected_keys = expected))
  dropped <- x[-(1:3), ]
  expect_equal(nrow(remp_snapshot_issues(dropped)), 0L)
  issues <- remp_snapshot_issues(dropped, expected_keys = expected)
  expect_true(any(issues$field == "coverage"))
  expect_error(assert_remp_snapshots(dropped, expected_keys = expected), class = "remp_snapshot_error")
  expect_true(any(remp_snapshot_issues(x, expected_keys = expected[-1, ])$field == "coverage"))
  expect_true(any(remp_snapshot_issues(x, expected_keys = list())$field == "expected_keys"))
})

test_that("actual fabricated workbook output satisfies the snapshot contract", {
  with_synthetic_workbooks(list("Schedule 2 - Q1 2025-26.xlsx" = list(Residential = synthetic_panel(
    c(600000, 610000, 590000, 620000, 630000),
    c(500000, 480000, 510000, 520000, 490000)))), function(root) {
      out <- remp$consolidate_REMP_data(root)
      expect_true(assert_remp_snapshots(out))
      broken <- out; broken$value[broken$retailer == "National Total"] <- 1
      expect_snapshot_issue(broken, "total")
    })
})

test_that("complex counts produce structured diagnostics and preserve the issue payload", {
  x <- invented_snapshots(); x$value[1] <- 1+1i
  expect_snapshot_issue(x, "value")
  issues <- remp_snapshot_issues(x)
  condition <- tryCatch(assert_remp_snapshots(x), remp_snapshot_error = identity)
  expect_identical(condition$issues, issues)
})


test_that("opposite national total errors cannot cancel across quarters", {
  x <- invented_snapshots()
  expect_equal(nrow(remp_snapshot_issues(x)), 0L)
  for (bad_rows in list(3L, c(3L, 6L))) {
    changed <- x
    changed$value[3] <- 31
    if (length(bad_rows) == 2L) changed$value[6] <- 24
    issues <- remp_snapshot_issues(changed)
    expect_identical(issues$field, rep("total", length(bad_rows)))
    expect_identical(issues$row, bad_rows)
    # The row payload must still identify the precise quarter/date keys.
    expect_equal(changed$date[issues$row], as.Date(c("2024-07-01", "2024-10-01"))[seq_along(bad_rows)])
    expect_equal(changed$year_quarter[issues$row], c("2024-25 Q1", "2024-25 Q2")[seq_along(bad_rows)])
    expect_error(assert_remp_snapshots(changed), class = "remp_snapshot_error")
  }
})
