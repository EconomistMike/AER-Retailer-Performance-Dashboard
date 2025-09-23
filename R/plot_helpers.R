# R/plot_helpers.R
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
  library(plotly)
})

# Open a PDF device robustly, falling back to base pdf() if Cairo fails
open_pdf_safely <- function(path, width = 10, height = 6, onefile = TRUE, prefer_cairo = TRUE) {
  before <- grDevices::dev.cur()
  used   <- NA_character_
  opened <- FALSE
  
  if (prefer_cairo && isTRUE(capabilities("cairo"))) {
    try({
      grDevices::cairo_pdf(path, width = width, height = height, onefile = onefile)
    }, silent = TRUE)
    if (!identical(grDevices::dev.cur(), before)) {
      used <- "cairo"; opened <- TRUE
    }
  }
  if (!opened) {
    grDevices::pdf(path, width = width, height = height, onefile = onefile, useDingbats = FALSE)
    if (!identical(grDevices::dev.cur(), before)) {
      used <- "pdf"; opened <- TRUE
    }
  }
  list(device = used, opened = opened)
}

# Prepare plotting data (drops National Total, orders by date)
build_plot_data <- function(result_national) {
  result_national %>%
    filter(!is.na(value), !is.na(date)) %>%
    filter(tolower(retailer) != "national total") %>%
    arrange(date)
}

# Export a multi-page PDF: one retailer per page, ordered by peak size
export_multi_page_pdf <- function(result_national, out_file) {
  plot_data <- build_plot_data(result_national)
  
  # Order pages by peak size
  retailer_order <- plot_data %>%
    group_by(retailer) %>%
    summarise(peak = max(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(peak)) %>%
    pull(retailer)
  
  dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
  op <- open_pdf_safely(out_file, width = 10, height = 6, onefile = TRUE)
  if (!isTRUE(op$opened)) stop("Could not open a PDF device at: ", out_file)
  
  title_dash <- if (identical(op$device, "cairo")) " — " else " - "
  
  for (r in retailer_order) {
    df_r <- dplyr::filter(plot_data, retailer == r)
    
    p <- ggplot(df_r, aes(date, value)) +
      geom_line(linewidth = 0.7) +
      geom_point(size = 1.6) +
      scale_y_continuous(labels = comma) +
      scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
      labs(
        title = paste0("Residential electricity customers", title_dash, r),
        subtitle = "National, quarterly counts",
        x = "Quarter (start date)", y = "Customers",
        caption = "Source: AER Retail Performance (Schedule 2)."
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title.position = "plot",
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    print(p)
  }
  invisible(dev.off())
  out_file
}

# OPTIONAL: export one big interactive HTML (legend off)
export_plotly_all <- function(result_national, out_html) {
  pd <- build_plot_data(result_national)
  
  p <- plot_ly(
    data = pd,
    x = ~date, y = ~value,
    type = "scatter", mode = "lines",
    split = ~retailer,
    text = ~sprintf("%s<br>%s<br>Customers: %s",
                    retailer, format(date, "%Y-%m-%d"), comma(value)),
    hovertemplate = "%{text}<extra></extra>",
    showlegend = FALSE
  ) %>%
    layout(
      title = list(text = "Residential electricity customers — interactive"),
      xaxis = list(title = "Quarter (start date)"),
      yaxis = list(title = "Customers", tickformat = ","),
      margin = list(l = 60, r = 10, t = 60, b = 50)
    )
  
  dir.create(dirname(out_html), showWarnings = FALSE, recursive = TRUE)
  htmlwidgets::saveWidget(p, file = out_html, selfcontained = TRUE)
  out_html
}
