#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(here)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# -------------------------------
# Helpers
# -------------------------------

# Parse quarter & FY from FILE NAME (used to order overlapping updates)
parse_update_quarter <- function(filename) {
  fname   <- tolower(filename)
  q_match <- str_match(fname, "(?i)(?:quarter\\s*([1-4])|q([1-4]))")
  q_num   <- suppressWarnings(as.numeric(coalesce(q_match[, 2], q_match[, 3])))
  yr_match <- str_match(fname, "(20\\d{2})\\s*[–\\-/]\\s*(\\d{2})")
  yr_start <- suppressWarnings(as.numeric(yr_match[, 2]))
  update_order <- ifelse(is.na(yr_start) | is.na(q_num), 0, yr_start * 4 + q_num)
  list(quarter = q_num, start_year = yr_start, update_order = update_order)
}

# Vectorised parsing of quarter labels (e.g. "Q3 2024-25") to FY + start-of-quarter date
parse_quarter_label <- function(qlabel) {
  lbl <- toupper(qlabel %||% NA_character_)
  lbl <- stringr::str_replace_all(lbl, "QUARTER", "Q")
  q_num <- suppressWarnings(as.integer(stringr::str_match(lbl, "Q\\s*([1-4])")[, 2]))
  yr_match   <- stringr::str_match(lbl, "(20\\d{2})\\D*(\\d{2})?")
  start_year <- suppressWarnings(as.integer(yr_match[, 2]))
  end_2d     <- suppressWarnings(as.integer(yr_match[, 3]))
  end_full <- ifelse(
    !is.na(end_2d),
    {
      s_yy <- start_year %% 100
      base <- (start_year %/% 100) * 100
      base + ifelse(end_2d < s_yy, 100, 0) + end_2d
    },
    start_year + 1L
  )
  financial_year <- ifelse(
    !is.na(start_year),
    sprintf("%d-%02d", start_year, end_full %% 100),
    NA_character_
  )
  date <- dplyr::case_when(
    !is.na(q_num) & q_num == 1 & !is.na(start_year) ~ as.Date(sprintf("%d-07-01", start_year)),
    !is.na(q_num) & q_num == 2 & !is.na(start_year) ~ as.Date(sprintf("%d-10-01", start_year)),
    !is.na(q_num) & q_num == 3 & !is.na(start_year) ~ as.Date(sprintf("%d-01-01", start_year + 1L)),
    !is.na(q_num) & q_num == 4 & !is.na(start_year) ~ as.Date(sprintf("%d-04-01", start_year + 1L)),
    TRUE ~ as.Date(NA_character_)
  )
  tibble(
    quarter_number = q_num,
    financial_year = financial_year,
    year_quarter   = ifelse(!is.na(q_num) & !is.na(financial_year),
                            paste0(financial_year, " Q", q_num), NA_character_),
    date           = date
  )
}

# NA-safe header validator: columns B..F must be five quarter headers with a year
looks_like_quarter_headers <- function(hdrs) {
  if (length(hdrs) != 5) return(FALSE)
  if (any(is.na(hdrs) | trimws(hdrs) == "")) return(FALSE)
  pat <- "^Q\\s*[1-4].*20\\d{2}"
  det <- stringr::str_detect(
    stringr::str_to_upper(stringr::str_squish(hdrs)), pat
  )
  isTRUE(all(det, na.rm = TRUE))
}

# Safe numeric parser (strip commas/space; tolerate NA)
parse_num <- function(x) {
  x <- stringr::str_replace_all(as.character(x), "[,\\s]", "")
  suppressWarnings(as.numeric(x))
}

# Retailer name normaliser
clean_retailer <- function(x) {
  y <- x %>%
    stringr::str_replace_all("[*^]+$", "") %>%    # drop trailing * or ^
    stringr::str_squish()
  # targeted recodes for common variants (extend if needed)
  y <- dplyr::recode(y,
                     "BlueNRG"        = "Blue NRG",
                     "Glowpower"      = "GlowPower",
                     "Amayism Energy" = "amaysim Energy",
                     "Iberdola"       = "Iberdrola Australia",
                     .default = y
  )
  y
}

# -------------------------------
# Core reader for ONE sheet
#  - find all candidate Retailer header rows,
#  - validate B..F as quarter labels w/ year,
#  - for each candidate block (A:F .. National Total), compute score = sum(National Total across 5 quarters),
#  - keep the block with the largest score (true customer numbers table),
#  - pivot to long.
# -------------------------------
read_schedule2_data <- function(file_path, schedule_sheet, update_info) {
  # Scan A:F rows 1..300 to locate candidates
  search_rows <- suppressWarnings(
    readxl::read_excel(
      file_path, sheet = schedule_sheet,
      range = readxl::cell_limits(c(1, 1), c(300, 6)),
      col_names = FALSE, col_types = rep("text", 6)
    )
  )
  if (nrow(search_rows) == 0) return(NULL)
  names(search_rows) <- paste0("X", seq_len(ncol(search_rows)))
  
  cand_rows <- which(stringr::str_detect(tolower(search_rows$X1), "retailer"))
  if (!length(cand_rows)) return(NULL)
  
  best <- NULL
  best_score <- -Inf
  
  for (r in cand_rows) {
    # validate header B..F
    hdr_rng <- readxl::cell_limits(c(r, 1), c(r, 6))
    hdr_row <- suppressWarnings(
      readxl::read_excel(
        file_path, sheet = schedule_sheet, range = hdr_rng,
        col_names = FALSE, col_types = "text"
      )
    )
    if (!nrow(hdr_row) || ncol(hdr_row) < 6) next
    hdr_vec <- as.character(hdr_row[1, 2:6, drop = TRUE])
    if (!isTRUE(looks_like_quarter_headers(hdr_vec))) next
    
    # find National Total below this header
    nat_idx <- which(stringr::str_detect(tolower(search_rows$X1), "national total") &
                       seq_len(nrow(search_rows)) > r)
    end_row <- if (!length(nat_idx)) r + 100 else nat_idx[1]
    end_row <- max(end_row, r)
    
    # read A:F for this block
    data_range <- readxl::cell_limits(c(r, 1), c(end_row, 6))
    df <- suppressWarnings(
      readxl::read_excel(
        file_path, sheet = schedule_sheet, range = data_range,
        col_names = TRUE, .name_repair = "minimal"
      )
    )
    if (ncol(df) < 2 || nrow(df) == 0) next
    
    # names and first column
    nm <- names(df)
    nm[is.na(nm) | nm == ""] <- paste0("col", which(is.na(nm) | nm == ""))
    names(df) <- nm
    names(df)[1] <- "retailer"
    df$retailer <- as.character(df$retailer)
    
    # keep only first five distinct quarter cols next to Retailer
    hdrs <- names(df)[2:min(6, ncol(df))]
    canon <- stringr::str_squish(stringr::str_to_upper(hdrs))
    keep_idx_rel <- match(unique(canon), canon)
    keep_idx_rel <- keep_idx_rel[seq_len(min(5, length(keep_idx_rel)))]
    df <- df[, c(1, 1 + keep_idx_rel), drop = FALSE]
    
    # score by National Total sum
    nat_row <- df %>% dplyr::filter(stringr::str_detect(tolower(retailer), "^national\\s+total$"))
    if (!nrow(nat_row)) next
    score <- sum(parse_num(unlist(nat_row[1, -1, drop = TRUE])), na.rm = TRUE)
    
    # heuristic guard: skip obviously tiny panels (e.g. percents)
    if (!is.finite(score) || score < 1e6) next
    
    # candidate wins
    if (score > best_score) {
      # drop rows with all quarter cells blank
      qmat <- df[, -1, drop = FALSE]
      all_blank <- apply(qmat, 1, function(rw) all(is.na(rw) | trimws(as.character(rw)) == ""))
      df2 <- df[!all_blank, , drop = FALSE]
      if (!nrow(df2)) next
      
      best <- df2 %>%
        tidyr::pivot_longer(-retailer, names_to = "quarter_col", values_to = "customers") %>%
        mutate(
          quarter_label = stringr::str_squish(stringr::str_replace_all(quarter_col, "(?i)quarter", "Q")),
          customers     = as.character(customers),
          update_order  = update_info$update_order,
          file          = basename(file_path)
        ) %>%
        group_by(retailer, quarter_label) %>% slice(1L) %>% ungroup()
      
      best_score <- score
    }
  }
  
  best
}

# -------------------------------
# Public API
# -------------------------------
consolidate_REMP_data <- function(base_dir = here::here("data", "REMP")) {
  
  files <- list.files(
    path = base_dir,
    pattern = "\\.xls", ignore.case = TRUE,
    full.names = TRUE, recursive = FALSE  # no subfolders per your setup
  )
  if (!length(files)) {
    warning("No Excel files found under: ", base_dir)
    return(tibble())
  }
  
  out <- purrr::map_dfr(files, function(path) {
    update_info <- parse_update_quarter(basename(path))
    shts <- tryCatch(readxl::excel_sheets(path), error = function(e) character(0))
    if (!length(shts)) return(NULL)
    
    per_sheets <- lapply(shts, function(sht) {
      tryCatch(read_schedule2_data(path, sht, update_info), error = function(e) NULL)
    })
    per_sheets <- per_sheets[!vapply(per_sheets, is.null, logical(1))]
    if (!length(per_sheets)) return(NULL)
    dplyr::bind_rows(per_sheets)
  })
  
  if (!nrow(out)) return(tibble())
  
  # clean values and names
  out <- out %>%
    mutate(
      retailer  = clean_retailer(retailer),
      customers = parse_num(customers)
    ) %>%
    filter(!is.na(customers))
  
  # enrich with FY + date, then de-dup by (retailer, date) newest update wins
  qinfo <- parse_quarter_label(out$quarter_label)
  out <- bind_cols(out, qinfo)
  
  out <- out %>%
    group_by(retailer, date) %>%
    arrange(desc(update_order), .by_group = TRUE) %>%
    slice(1L) %>%
    ungroup()
  
  # final long form
  out %>%
    transmute(
      date,
      year_quarter,
      quarter_number,
      financial_year,
      retailer,
      metric         = "Number",
      customer_type  = "Electricity",
      customer_group = "Residential",
      jurisdiction   = "National",
      value          = customers
    ) %>%
    arrange(date, retailer)
}

# Backwards-compat wrapper
consolidate_customer_numbers_national <- function(base_dir, folder_pattern = NULL) {
  consolidate_REMP_data(base_dir = base_dir)
}