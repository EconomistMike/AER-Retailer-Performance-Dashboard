library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(tibble)

# Load definitions without running package setup or either Shiny app.
remp <- new.env(parent = globalenv())
for (expr in parse(testthat::test_path("..", "..", "R", "consolidate_REMP_data.R"))) {
  if (is.call(expr) && identical(expr[[1]], as.name("<-")) &&
      is.call(expr[[3]]) && identical(expr[[3]][[1]], as.name("function"))) {
    eval(expr, remp)
  }
}

synthetic_panel <- function(a = 600000, b = 500000) {
  out <- data.frame(retailer = c("Invented A", "Invented B", "National Total"),
    stringsAsFactors = FALSE, check.names = FALSE)
  quarters <- c("Q1 2024-25", "Q2 2024-25", "Q3 2024-25", "Q4 2024-25", "Q1 2025-26")
  a <- rep(a, length.out = length(quarters))
  b <- rep(b, length.out = length(quarters))
  for (i in seq_along(quarters)) {
    out[[quarters[i]]] <- c(a[i], b[i], a[i] + b[i])
  }
  out
}

with_synthetic_workbooks <- function(files, inspect) {
  root <- tempfile("remp-fixture-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  for (name in names(files)) writexl::write_xlsx(files[[name]], file.path(root, name))
  inspect(root)
}

# Extract one reactive body without executing the application or reading its RDS.
app_reactive_body <- function(relative_path, name) {
  exprs <- parse(testthat::test_path("..", "..", relative_path))
  server <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
    identical(x[[2]], as.name("server")), as.list(exprs))
  stopifnot(length(server) == 1L)
  statements <- as.list(server[[1]][[3]][[3]])[-1]
  selected <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
    identical(x[[2]], as.name(name)), statements)
  stopifnot(length(selected) == 1L, identical(selected[[1]][[3]][[1]], as.name("reactive")))
  selected[[1]][[3]][[2]]
}

synthetic_app_data <- function() {
  tibble::tibble(date = as.Date(c("2025-01-01", "2025-04-01", "2025-01-01",
      "2025-01-01", "2025-04-01", "2025-01-01", "2025-04-01",
      "2025-01-01", "2025-02-01", "2025-04-01", "2025-01-01", "2025-04-01")),
    retailer = c("Invented A", "Invented A", "Invented B", "Invented C", "Invented C", "AGL", "AGL",
      "Invented D", "Invented D", "Invented D", "Invented E", "Invented E"),
    value = c(0, 10, 5, 2, 0, 4, 6, 0, 5, 0, 0, 15))
}

app_test_env <- function(relative_path, input = list()) {
  env <- new.env(parent = globalenv())
  env$base_data <- synthetic_app_data
  status <- eval(app_reactive_body(relative_path, "retailer_status"), env)
  env$retailer_status <- function() status
  env$input <- input
  env$validate <- shiny::validate
  env$need <- shiny::need
  env
}
