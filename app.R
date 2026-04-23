# =============================================================================
# MERIDIAN
# Main Application Entry Point
# =============================================================================

# ---- Load packages ----
library(shiny)
library(bslib)
library(shinyWidgets)
library(DT)
library(shinycssloaders)
library(thematic)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(heatmaply)
library(scales)
library(lme4)
library(emmeans)
library(metan)
library(rlang)
library(ggrepel)

# ---- Source R files ----
source("R/utils.R")
source("R/data_processing.R")
source("R/plots.R")
source("R/models.R")
source("R/stability.R")
source("R/ammi.R")
source("R/ammi_plots.R")
source("R/enviromics.R")
source("R/mod_data_upload.R")
source("R/mod_eda.R")
source("R/mod_anova.R")
source("R/mod_stability.R")
source("R/mod_adaptation.R")

# ---- Theme ----
app_theme <- bs_theme(
  version  = 5,
  bootswatch = "flatly",
  base_font    = font_google("Inter"),
  heading_font = font_google("Inter"),
  primary   = "#2c7a51",
  secondary = "#6c757d",
  success   = "#48a868",
  info      = "#5b9279",
  warning   = "#d4a853",
  danger    = "#c44e52",
  "navbar-bg"         = "#1a1a2e",
  "body-bg"           = "#f8f9fa",
  "card-bg"           = "#ffffff",
  "border-radius"     = "0.5rem",
  "card-border-color" = "#e0e0e0"
)

# ---- Placeholder UI for future modules ----
placeholder_ui <- function(module_name) {
  div(
    class = "empty-state",
    style = "padding: 5rem 2rem;",
    tags$div(class = "icon", icon("cogs")),
    h4(module_name),
    p(LABELS$coming_soon),
    tags$p(
      style = "font-size: 0.85rem; color: #999; margin-top: 1rem;",
      "This module will be implemented in a future update."
    )
  )
}

# =============================================================================
# UI
# =============================================================================
ui <- page_navbar(
  title = tags$span(
    tags$img(src = "logo.png", height = "32px", style = "margin-right: 8px;"),
    LABELS$app_title
  ),
  id     = "main_navbar",
  theme  = app_theme,
  header = tags$head(
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$title("MERIDIAN")
  ),
  fillable = TRUE,

  # ---- Module 1: Data Upload ----
  nav_panel(
    title = tags$span(icon("upload"), " Data Upload"),
    value = "tab_upload",
    mod_data_upload_ui("data")
  ),

  # ---- Module 2: EDA ----
  nav_panel(
    title = tags$span(icon("chart-bar"), " Exploratory Analysis"),
    value = "tab_eda",
    mod_eda_ui("eda")
  ),

  # ---- Module 3: ANOVA & Variance ----
  nav_panel(
    title = tags$span(icon("calculator"), " ANOVA & Variance"),
    value = "tab_anova",
    mod_anova_ui("anova")
  ),

  # ---- Module 4: Stability ----
  nav_panel(
    title = tags$span(icon("balance-scale"), " Stability"),
    value = "tab_stability",
    mod_stability_ui("stability")
  ),

  # ---- Module 5: Adaptation ----
  nav_panel(
    title = tags$span(icon("globe"), " Adaptation & Enviromics"),
    value = "tab_adaptation",
    mod_adaptation_ui("adaptation")
  ),

  # ---- Module 6: Reports (placeholder) ----
  nav_panel(
    title = tags$span(icon("file-alt"), " Reports"),
    value = "tab_reports",
    placeholder_ui(LABELS$m6_title)
  )
)


# =============================================================================
# Server
# =============================================================================
server <- function(input, output, session) {

  # ---- Auto-theme ggplot2 to match app ----
  thematic::thematic_shiny()

  # ---- Startup modal ----
  showModal(modalDialog(
    title = tags$div(
      style = "display: flex; align-items: center; gap: 10px;",
      tags$img(src = "logo.png", height = "40px"),
      tags$span("Welcome to MERIDIAN", style = "font-weight: 600;")
    ),
    tags$div(
      style = "text-align: center; padding: 1rem 0;",
      tags$p(
        style = "font-size: 1rem; color: #555; margin-bottom: 1.5rem;",
        "A comprehensive tool for analyzing Multi-Environment Trial data.",
        tags$br(),
        "Get started by uploading your own data or exploring with our example dataset."
      ),
      layout_column_wrap(
        width = 1 / 2,
        fill = FALSE,
        actionButton("btn_load_example", 
          tags$span(icon("seedling"), " Load Example Data"),
          class = "btn btn-success btn-lg w-100",
          style = "padding: 1rem;"
        ),
        actionButton("btn_upload_own",
          tags$span(icon("upload"), " Upload My Data"),
          class = "btn btn-outline-primary btn-lg w-100",
          style = "padding: 1rem;"
        )
      ),
      tags$p(
        style = "font-size: 0.8rem; color: #999; margin-top: 1.5rem;",
        "Example: 20 genotypes \u00D7 5 environments \u00D7 3 reps (RCBD) with 4 traits"
      )
    ),
    footer = NULL,
    size = "m",
    easyClose = TRUE
  ))

  # ---- Initialize Module 1 ----
  data_result <- mod_data_upload_server("data")

  # ---- Initialize Module 2 ----
  mod_eda_server("eda", data_result)

  # ---- Initialize Module 3 ----
  anova_result <- mod_anova_server("anova", data_result)

  # ---- Initialize Module 4 ----
  stab_result <- mod_stability_server("stability", data_result)

  # ---- Initialize Module 5 ----
  adapt_result <- mod_adaptation_server("adaptation", data_result)

  # ---- Handle startup modal buttons ----
  observeEvent(input$btn_load_example, {
    removeModal()
    data_result$load_example()
  })

  observeEvent(input$btn_upload_own, {
    removeModal()
    # User stays on the Data Upload tab to upload their file
  })

  # ---- Guard: Show notification if user navigates to empty module ----
  observeEvent(input$main_navbar, {
    # Only Phase 3 modules are still placeholders
    empty_tabs <- c("tab_reports")
    if (input$main_navbar %in% empty_tabs) {
      showNotification(
        paste("This module will be available in a future update."),
        type = "message", duration = 4
      )
    }

    # Guard: require data for analysis modules
    data_tabs <- c("tab_eda", "tab_anova", "tab_stability", "tab_adaptation")
    if (input$main_navbar %in% data_tabs) {
      db <- data_result$data_bundle()
      if (is.null(db)) {
        showNotification(
          LABELS$no_data_warning,
          type = "warning", duration = 5
        )
      }
    }
  })
}


# =============================================================================
# Run
# =============================================================================
shinyApp(ui = ui, server = server)
