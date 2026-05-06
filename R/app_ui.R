#' MERIDIAN application UI
#'
#' @param request Internal Shiny request object.
#' @keywords internal
app_ui <- function(request) {
  shiny::addResourcePath("assets", app_sys("app/www"))

  app_theme <- bslib::bs_theme(
    version  = 5,
    base_font    = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter"),
    primary   = "#1f8f55",
    secondary = "#60736a",
    success   = "#2fb66d",
    info      = "#3aa7a3",
    warning   = "#d8a735",
    danger    = "#cf5b55",
    "body-bg"           = "#f7f9f6",
    "body-color"        = "#102018",
    "card-bg"           = "#ffffff",
    "border-radius"     = "0.5rem",
    "card-border-color" = "#dfe7e1",
    "input-border-color" = "#d9e3dc",
    "font-size-base"    = "0.94rem"
  )

  nav_label <- function(icon, label) {
    shiny::tags$span(
      class = "meridian-nav-label",
      shiny::icon(icon),
      shiny::tags$span(label)
    )
  }

  bslib::page_navbar(
    title = shiny::tags$span(
      class = "meridian-brand",
      shiny::tags$img(src = "assets/logo.png", class = "meridian-brand-mark", alt = ""),
      shiny::tags$span(
        class = "meridian-brand-copy",
        shiny::tags$span(class = "meridian-brand-name", LABELS$app_title),
        shiny::tags$span(class = "meridian-brand-tagline", "Plant phenotype data intelligence")
      )
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
      title = nav_label("home", "Overview"),
      value = "tab_upload",
      mod_data_upload_ui("data")
    ),

    bslib::nav_panel(
      title = nav_label("leaf", "Traits"),
      value = "tab_eda",
      mod_eda_ui("eda")
    ),

    bslib::nav_panel(
      title = nav_label("calculator", "ANOVA"),
      value = "tab_anova",
      mod_anova_ui("anova")
    ),

    bslib::nav_panel(
      title = nav_label("chart-line", "Stability"),
      value = "tab_stability",
      mod_stability_ui("stability")
    ),

    bslib::nav_panel(
      title = nav_label("globe-americas", "Adaptation"),
      value = "tab_adaptation",
      mod_adaptation_ui("adaptation")
    ),

    bslib::nav_panel(
      title = nav_label("map", "Field Map"),
      value = "tab_spatial",
      mod_spatial_ui("spatial")
    ),

    bslib::nav_panel(
      title = nav_label("file-alt", "Reports"),
      value = "tab_reports",
      mod_reports_ui("reports")
    )
  )
}
