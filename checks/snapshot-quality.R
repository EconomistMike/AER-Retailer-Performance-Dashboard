# Opt-in checks of consolidated national residential electricity snapshots.
# These checks do not establish source sheet identity or execute any pipeline.
remp_snapshot_issues <- function(data, expected_keys = NULL) {
  issues <- data.frame(row = integer(), field = character(), message = character())
  add <- function(field, message, rows = NA_integer_) {
    if (length(rows)) issues <<- rbind(issues, data.frame(row = as.integer(rows), field = field, message = message))
  }
  if (!is.data.frame(data)) {
    add("schema", "Expected a consolidated snapshot data frame.")
    return(issues)
  }
  if (anyNA(names(data)) || any(names(data) == "") || anyDuplicated(names(data))) {
    add("columns", "Column names must be present and unique.")
    return(issues)
  }
  required <- c("date", "year_quarter", "quarter_number", "financial_year", "retailer", "metric",
    "customer_type", "customer_group", "jurisdiction", "value")
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    for (field in missing) add(field, "Required consolidated column is absent.")
    return(issues)
  }
  if (!nrow(data)) add("rows", "No snapshot observations are present.")
  text <- function(field) {
    x <- data[[field]]
    if (!is.character(x) || !is.null(dim(x))) {
      add(field, "Expected a character vector.")
      return(FALSE)
    }
    add(field, "Expected non-missing, non-blank, trimmed labels.", which(is.na(x) | trimws(x) == "" | trimws(x) != x))
    TRUE
  }
  text_ok <- setNames(vapply(setdiff(required, c("date", "value", "quarter_number")), text, logical(1)),
    setdiff(required, c("date", "value", "quarter_number")))
  labels <- c(metric = "Number", customer_type = "Electricity", customer_group = "Residential", jurisdiction = "National")
  for (field in names(labels)) {
    if (text_ok[[field]]) add(field, "Unexpected population/metric label.", which(is.na(data[[field]]) | data[[field]] != labels[[field]]))
  }
  date_ok <- inherits(data$date, "Date") && is.null(dim(data$date))
  if (!date_ok) {
    add("date", "Expected a Date vector of quarter starts.")
  } else {
    valid_date <- is.finite(as.numeric(data$date)) & format(data$date, "%m-%d") %in% c("07-01", "10-01", "01-01", "04-01")
    add("date", "Expected non-missing quarter-start dates.", which(!valid_date))
    q <- match(format(data$date, "%m-%d"), c("07-01", "10-01", "01-01", "04-01"))
    year <- as.integer(format(data$date, "%Y"))
    start <- year - as.integer(q %in% c(3L, 4L))
    fy <- sprintf("%d-%02d", start, (start + 1L) %% 100L)
    if (!is.numeric(data$quarter_number) || is.complex(data$quarter_number) || !is.null(dim(data$quarter_number))) {
      add("quarter_number", "Expected numeric quarter numbers.")
    } else {
      add("quarter_number", "Quarter number disagrees with the date.",
        which(valid_date & (is.na(data$quarter_number) | data$quarter_number != q)))
    }
    if (text_ok[["financial_year"]]) add("financial_year", "Financial year disagrees with the date.",
      which(valid_date & (is.na(data$financial_year) | data$financial_year != fy)))
    if (text_ok[["year_quarter"]]) add("year_quarter", "Quarter label disagrees with the date.",
      which(valid_date & (is.na(data$year_quarter) | data$year_quarter != paste0(fy, " Q", q))))
  }
  value_ok <- is.numeric(data$value) && !is.complex(data$value) && is.null(dim(data$value))
  if (!value_ok) {
    add("value", "Expected numeric customer counts.")
  } else {
    add("value", "Customer counts must be finite non-negative whole numbers.",
      which(!is.finite(data$value) | data$value < 0 | data$value != floor(data$value)))
  }
  keys_ok <- date_ok && text_ok[["retailer"]]
  if (keys_ok) {
    keys <- data[c("retailer", "date")]
    add("keys", "Duplicate retailer/date keys must be resolved explicitly.", which(duplicated(keys)))
    if (value_ok) {
      for (day in unique(as.character(data$date[!is.na(data$date)]))) {
        rows <- which(as.character(data$date) == day)
        national <- rows[!is.na(data$retailer[rows]) & data$retailer[rows] == "National Total"]
        others <- setdiff(rows, national)
        if (length(national) != 1L || !length(others)) {
          add("total", paste("Require one National Total and constituent retailer rows for", day))
        } else if (all(is.finite(data$value[rows])) && data$value[national] != sum(data$value[others])) {
          add("total", paste("Retailer sum differs from National Total for", day), national)
        }
      }
    }
    if (!is.null(expected_keys)) {
      valid <- is.data.frame(expected_keys) && !anyDuplicated(names(expected_keys)) &&
        all(c("retailer", "date") %in% names(expected_keys))
      if (valid) valid <- is.character(expected_keys$retailer) && is.null(dim(expected_keys$retailer)) &&
        inherits(expected_keys$date, "Date") && is.null(dim(expected_keys$date)) &&
        !anyNA(expected_keys[c("retailer", "date")]) && all(is.finite(as.numeric(expected_keys$date))) &&
        all(trimws(expected_keys$retailer) == expected_keys$retailer & expected_keys$retailer != "")
      if (!valid) {
        add("expected_keys", "Supply independently established non-missing character retailer / Date pairs.")
      } else {
        expected <- unique(expected_keys[c("retailer", "date")]); actual <- unique(keys)
        missing <- if (nrow(expected)) !duplicated(rbind(actual, expected))[nrow(actual) + seq_len(nrow(expected))] else logical()
        extra <- if (nrow(actual)) !duplicated(rbind(expected, actual))[nrow(expected) + seq_len(nrow(actual))] else logical()
        if (any(missing) || any(extra)) add("coverage", "Observed and independently expected retailer/date pairs differ.")
      }
    }
  }
  rownames(issues) <- NULL
  issues
}

assert_remp_snapshots <- function(data, expected_keys = NULL) {
  issues <- remp_snapshot_issues(data, expected_keys)
  if (nrow(issues)) stop(structure(list(message = paste(nrow(issues), "snapshot issue(s); inspect condition$issues."),
    call = NULL, issues = issues), class = c("remp_snapshot_error", "error", "condition")))
  invisible(TRUE)
}
