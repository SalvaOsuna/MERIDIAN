#' MERIDIAN application UI
#'
#' @param request Internal Shiny request object.
#' @keywords internal
app_ui <- function(request) {
  shiny::addResourcePath("assets", app_sys("app/www"))

  app_theme <- bslib::bs_theme(
    version  = 5,
    bootswatch = "flatly",
    base_font    = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter"),
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

  bslib::page_navbar(
    title = shiny::tags$span(
      shiny::tags$img(src = "assets/logo.png", height = "32px", style = "margin-right: 8px;"),
      LABELS$app_title
    ),
    id     = "main_navbar",
    theme  = app_theme,
    header = shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", href = "assets/styles.css"),
      shiny::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      shiny::tags$title("MERIDIAN")
    ),
    fillable = TRUE,

    bslib::nav_panel(
      title = shiny::tags$span(shiny::icon("upload"), " Data Upload"),
      value = "tab_upload",
      mod_data_upload_ui("data")
    ),

    bslib::nav_panel(
      title = shiny::tags$span(shiny::icon("chart-bar"), " Exploratory Analysis"),
      value = "tab_eda",
      mod_eda_ui("eda")
    ),

    bslib::nav_panel(
      title = shiny::tags$span(shiny::icon("calculator"), " ANOVA & Variance"),
      value = "tab_anova",
      mod_anova_ui("anova")
    ),

    bslib::nav_panel(
      title = shiny::tags$span(shiny::icon("balance-scale"), " Stability"),
      value = "tab_stability",
      mod_stability_ui("stability")
    ),

    bslib::nav_panel(
      title = shiny::tags$span(shiny::icon("globe"), " Adaptation & Enviromics"),
      value = "tab_adaptation",
      mod_adaptation_ui("adaptation")
    ),

    bslib::nav_panel(
      title = shiny::tags$span(shiny::icon("map"), " Spatial Trends"),
      value = "tab_spatial",
      mod_spatial_ui("spatial")
    ),

    bslib::nav_panel(
      title = shiny::tags$span(shiny::icon("file-alt"), " Reports"),
      value = "tab_reports",
      mod_reports_ui("reports")
    )
  )
}
