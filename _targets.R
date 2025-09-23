suppressPackageStartupMessages({
  library(targets)
  library(tarchetypes)   # optional but handy
  library(here)
  library(dplyr)
  library(readr)
})

tar_option_set(
  packages = c("dplyr","readr","ggplot2","scales","plotly","stringr","readxl"),
  format   = "rds"
)

# Source your modules
source(here::here("R", "consolidate_REMP_data.R"), local = TRUE)

# Minimal plotting helpers inline to avoid another file
open_pdf_safely <- function(path, width = 10, height = 6, onefile = TRUE, prefer_cairo = TRUE) {
  before <- grDevices::dev.cur(); used <- NA_character_; opened <- FALSE
  if (prefer_cairo && isTRUE(capabilities("cairo"))) {
    try({ grDevices::cairo_pdf(path, width = width, height = height, onefile = onefile) }, silent = TRUE)
    if (!identical(grDevices::dev.cur(), before)) { used <- "cairo"; opened <- TRUE }
  }
  if (!opened) {
    grDevices::pdf(path, width = width, height = height, onefile = onefile, useDingbats = FALSE)
    if (!identical(grDevices::dev.cur(), before)) { used <- "pdf"; opened <- TRUE }
  }
  list(device = used, opened = opened)
}
export_multi_page_pdf <- function(result_national, out_file) {
  suppressPackageStartupMessages({ library(ggplot2); library(scales); library(dplyr) })
  plot_data <- result_national %>% filter(!is.na(value), !is.na(date)) %>%
    filter(tolower(retailer) != "national total") %>% arrange(date)
  retailer_order <- plot_data %>% group_by(retailer) %>%
    summarise(peak = max(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(peak)) %>% pull(retailer)
  dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
  op <- open_pdf_safely(out_file, width = 10, height = 6, onefile = TRUE)
  if (!isTRUE(op$opened)) stop("Could not open a PDF device at: ", out_file)
  title_dash <- if (identical(op$device, "cairo")) " — " else " - "
  for (r in retailer_order) {
    df_r <- dplyr::filter(plot_data, retailer == r)
    p <- ggplot(df_r, aes(date, value)) +
      geom_line(linewidth = 0.7) + geom_point(size = 1.6) +
      scale_y_continuous(labels = comma) +
      scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
      labs(
        title = paste0("Residential electricity customers", title_dash, r),
        subtitle = "National, quarterly counts",
        x = "Quarter (start date)", y = "Customers",
        caption = "Source: AER Retail Performance (Schedule 2)."
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title.position = "plot",
            axis.text.x = element_text(angle = 45, hjust = 1))
    print(p)
  }
  invisible(dev.off())
  out_file
}

list(
  # Input directory (no hard-coded absolute path)
  tar_target(remp_dir, here::here("data","REMP"), format = "file"),
  
  # Consolidate
  tar_target(result_national, consolidate_REMP_data(base_dir = remp_dir)),
  
  # Sanity checks (optional artefacts you can inspect)
  tar_target(check_q2_2018_19,
             result_national %>%
               filter(retailer %in% c("Alinta Energy","Red Energy","Origin Energy"),
                      year_quarter == "2018-19 Q2") %>%
               arrange(retailer)),
  tar_target(check_dupes,
             result_national %>%
               count(retailer, date) %>%
               filter(n > 1)),
  
  # CSV export
  tar_target(results_csv, {
    out <- here::here("data","processed","retailer_cust_no.csv")
    dir.create(dirname(out), FALSE, TRUE); readr::write_csv(result_national, out); out
  }, format = "file"),
  
  # RDS export (for Shiny)
  tar_target(results_rds, {
    out <- here::here("data","processed","result_national.rds")
    dir.create(dirname(out), FALSE, TRUE); saveRDS(result_national, out); out
  }, format = "file"),
  
  # Multi-page PDF export
  tar_target(retailers_pdf, {
    out <- here::here("retailer_customer_counts_all.pdf")
    export_multi_page_pdf(result_national, out); out
  }, format = "file")
)