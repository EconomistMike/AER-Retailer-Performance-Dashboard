suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(plotly)
  library(scales)
  library(stringr)
  library(here)
})

options(shiny.fullstacktrace = TRUE)

result_national <- readRDS("result_national.rds")

ui <- fluidPage(
  titlePanel("Residential electricity customers — National, quarterly"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("tier1_mode",   "Tier 1 (AGL, EnergyAustralia, Origin, Ergon)",
                  choices = c("All", "Include only", "Exclude"), selected = "All"),
      selectInput("inactive_mode","Inactive (latest quarter = 0 OR absent in latest quarter)",
                  choices = c("All", "Include only", "Exclude"), selected = "All"),
      selectInput("startup_mode", "Startup (has zero then later positive)",
                  choices = c("All", "Include only", "Exclude"), selected = "All"),
      selectizeInput(
        "pick_retailer", "Retailer filter",
        choices = NULL, multiple = TRUE,
        options = list(
          placeholder = "Type to search… (leave empty for All)",
          plugins = list("remove_button"), create = FALSE
        )
      ),
      helpText("Filters are ANDed with the retailer selection. Leave retailer box empty to show All.")
    ),
    mainPanel(
      width = 9,
      plotlyOutput("retailer_plot", height = "680px")
    )
  )
)

server <- function(input, output, session) {
  # Load processed data from Targets; graceful fallback to global for local dev
  data_path <- here::here("data", "processed", "result_national.rds")
  dataset <- if (file.exists(data_path)) readRDS(data_path) else get0("result_national", envir = .GlobalEnv, inherits = FALSE)
  validate(need(!is.null(dataset), "No result_national data found. Run `tar_make()` first or provide data/processed/result_national.rds."))
  
  required_cols <- c("date", "retailer", "value")
  missing_cols <- setdiff(required_cols, names(dataset))
  validate(need(length(missing_cols) == 0,
                paste0("Missing required columns in result_national: ",
                       paste(missing_cols, collapse = ", "))))
  
  base_data <- reactive({
    dataset %>%
      mutate(date = as.Date(date),
             retailer = as.character(retailer),
             value = suppressWarnings(as.numeric(value))) %>%
      filter(!is.na(date), !is.na(value)) %>%
      filter(tolower(retailer) != "national total") %>%
      arrange(date)
  })
  
  retailer_status <- reactive({
    bd <- base_data()
    latest_date <- max(bd$date, na.rm = TRUE)
    tier1_set <- c("AGL", "EnergyAustralia", "Origin Energy", "Ergon Energy")
    
    bd %>%
      group_by(retailer) %>%
      arrange(date, .by_group = TRUE) %>%
      summarise(
        present_latest = any(date == latest_date),
        value_latest   = if (present_latest) value[date == latest_date][1] else NA_real_,
        inactive       = (!present_latest) || (present_latest && isTRUE(value_latest == 0)),
        startup        = {
          v <- value; seen_zero <- FALSE; flag <- FALSE
          for (vv in v) {
            if (isTRUE(vv == 0)) seen_zero <- TRUE
            if (isTRUE(vv > 0) && isTRUE(seen_zero)) { flag <- TRUE; break }
          }
          flag
        },
        .groups = "drop"
      ) %>%
      mutate(tier1 = retailer %in% tier1_set)
  })
  
  observe({
    choices <- base_data() %>% distinct(retailer) %>% arrange(retailer) %>% pull(retailer)
    updateSelectizeInput(session, "pick_retailer", choices = choices, server = TRUE)
  })
  
  filtered_data <- reactive({
    df <- base_data() %>% left_join(retailer_status(), by = "retailer")
    
    apply_mode <- function(data, col, mode) {
      if (identical(mode, "Include only")) {
        data %>% filter(.data[[col]] %in% TRUE)
      } else if (identical(mode, "Exclude")) {
        data %>% filter(!(.data[[col]] %in% TRUE))
      } else data
    }
    
    df <- df %>%
      apply_mode("tier1",    input$tier1_mode) %>%
      apply_mode("inactive", input$inactive_mode) %>%
      apply_mode("startup",  input$startup_mode)
    
    if (!is.null(input$pick_retailer) && length(input$pick_retailer) > 0) {
      df <- df %>% filter(retailer %in% input$pick_retailer)
    }
    
    validate(need(nrow(df) > 0, "No data after filters. Try changing Include/Exclude modes or clear the retailer box."))
    df
  })
  
  output$retailer_plot <- renderPlotly({
    df <- filtered_data()
    
    # base plot: all lines, no legend
    p <- plot_ly(
      data = df,
      x = ~date, y = ~value,
      type = "scatter", mode = "lines",
      split = ~retailer,
      text = ~sprintf("%s<br>%s<br>Customers: %s",
                      retailer, format(date, "%Y-%m-%d"), comma(value)),
      hovertemplate = "%{text}<extra></extra>",
      showlegend = FALSE
    )
    
    # compute last point per retailer
    last_points <- df %>%
      group_by(retailer) %>%
      filter(date == max(date)) %>%
      ungroup()
    
    # add text labels slightly nudged to the right
    for (i in seq_len(nrow(last_points))) {
      p <- add_annotations(
        p,
        x = last_points$date[i],
        y = last_points$value[i],
        text = last_points$retailer[i],
        xanchor = "left", yanchor = "middle",
        ax = 20, ay = 0,       # offset in pixels so text doesn’t overlap point
        showarrow = FALSE,
        font = list(size = 10, color = "black")
      )
    }
    
    p <- layout(
      p,
      title = list(text = "Residential electricity customers — interactive"),
      xaxis = list(title = "Quarter (start date)"),
      yaxis = list(title = "Customers", tickformat = ","),
      margin = list(l = 60, r = 120, t = 60, b = 50) # extra right margin for labels
    )
    
    p
  })
  
}

shinyApp(ui, server)