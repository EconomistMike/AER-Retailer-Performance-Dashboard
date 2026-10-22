test_that("financial quarters map to start dates across calendar years", {
  out <- remp$parse_quarter_label(c("Q1 2024-25", "Q2 2024-25", "Q3 2024-25", "Q4 2024-25"))
  expect_equal(out$date, as.Date(c("2024-07-01", "2024-10-01", "2025-01-01", "2025-04-01")))
  expect_equal(out$financial_year, rep("2024-25", 4))
  expect_equal(out$quarter_number, 1:4)
})

test_that("quarter header validation rejects blanks and non-quarter panels", {
  valid <- c("Q1 2024-25", "Q2 2024-25", "Q3 2024-25", "Q4 2024-25", "Q1 2025-26")
  expect_true(remp$looks_like_quarter_headers(valid))
  expect_false(remp$looks_like_quarter_headers(valid[-1]))
  valid[2] <- NA_character_
  expect_false(remp$looks_like_quarter_headers(valid))
  expect_false(remp$looks_like_quarter_headers(rep("Percentage", 5)))
})

test_that("counts parse commas and spaces without inventing suppressed values", {
  expect_equal(remp$parse_num(c("1,234", " 5 678 ", "0", "-", "n.p.", NA)),
    c(1234, 5678, 0, NA_real_, NA_real_, NA_real_))
  expect_equal(remp$clean_retailer(c(" Invented A*", "Invented B^^")), c("Invented A", "Invented B"))
})

test_that("real workbook ingestion preserves snapshot counts and totals", {
  with_synthetic_workbooks(list("Schedule 2 - Q1 2025-26.xlsx" = list(Residential = synthetic_panel(
    c(600000, 610000, 590000, 620000, 630000),
    c(500000, 480000, 510000, 520000, 490000)))),
    function(root) {
      out <- remp$consolidate_REMP_data(root)
      expect_equal(nrow(out), 15L)
      a <- filter(out, retailer == "Invented A")
      expect_equal(a$value, c(600000, 610000, 590000, 620000, 630000))
      expect_equal(a$date, as.Date(c("2024-07-01", "2024-10-01", "2025-01-01", "2025-04-01", "2025-07-01")))
      expect_true(all(out$metric == "Number" & out$jurisdiction == "National"))
      expect_false(anyDuplicated(out[c("retailer", "date")]) > 0L)
      for (day in unique(out$date)) {
        rows <- filter(out, date == day)
        expect_equal(rows$value[rows$retailer == "National Total"], sum(rows$value[rows$retailer != "National Total"]))
      }
    })
})

test_that("newer labelled releases replace overlapping observations", {
  with_synthetic_workbooks(list(
    "Schedule 2 - Q1 2025-26.xlsx" = list(Residential = synthetic_panel(600000, 500000)),
    "Schedule 2 - Q2 2025-26.xlsx" = list(Residential = synthetic_panel(700000, 500000))),
    function(root) {
      out <- remp$consolidate_REMP_data(root)
      expect_equal(nrow(out), 15L)
      expect_equal(out$value[out$retailer == "Invented A"], rep(700000, 5))
      expect_false(anyDuplicated(out[c("retailer", "date")]) > 0L)
    })
})

test_that("an empty source directory is explicit", {
  root <- tempfile("empty-remp-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE))
  expect_warning(out <- remp$consolidate_REMP_data(root), "No Excel files")
  expect_equal(nrow(out), 0L)
})
