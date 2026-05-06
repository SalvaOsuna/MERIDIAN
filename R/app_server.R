#' MERIDIAN application server
#'
#' @param input,output,session Internal Shiny objects.
#' @keywords internal
app_server <- function(input, output, session) {
  report_registry <- shiny::reactiveValues()

  if (requireNamespace("thematic", quietly = TRUE)) {
    thematic::thematic_shiny()
  }

  shiny::showModal(shiny::modalDialog(
    title = shiny::tags$div(
      class = "meridian-modal-title",
      shiny::tags$img(src = "assets/logo.png", height = "40px"),
      shiny::tags$span("Welcome to MERIDIAN")
    ),
    shiny::tags$div(
      class = "meridian-welcome",
      shiny::tags$p(
        "Analyze multi-environment trial data from upload to exploratory summaries, ",
        "variance components, spatial trends, stability models, and reports."
      ),
      bslib::layout_column_wrap(
        width = 1 / 2,
        fill = FALSE,
        shiny::actionButton(
          "btn_load_example",
          shiny::tags$span(shiny::icon("seedling"), " Load Example Data"),
          class = "btn btn-success btn-lg w-100"
        ),
        shiny::actionButton(
          "btn_upload_own",
          shiny::tags$span(shiny::icon("upload"), " Upload My Data"),
          class = "btn btn-outline-primary btn-lg w-100"
        )
      ),
      shiny::tags$p(
        class = "meridian-welcome-note",
        "Example: 20 genotypes x 5 environments x 3 reps (RCBD) with 4 traits"
      )
    ),
    footer = NULL,
    size = "m",
    easyClose = TRUE
  ))

  data_result <- mod_data_upload_server("data")

  mod_eda_server("eda", data_result, report_registry = report_registry)

  anova_result <- mod_anova_server("anova", data_result, report_registry = report_registry)

  stab_result <- mod_stability_server("stability", data_result, report_registry = report_registry)

  adapt_result <- mod_adaptation_server("adaptation", data_result, report_registry = report_registry)

  mod_spatial_server("spatial", data_result, report_registry = report_registry)

  mod_reports_server(
    "reports",
    data_result = data_result,
    anova_result = anova_result,
    stab_result = stab_result,
    adapt_result = adapt_result,
    plot_registry = report_registry
  )

  previous_dataset_signature <- shiny::reactiveVal(NULL)
  shiny::observeEvent(data_result$data_bundle(), {
    sig <- make_dataset_signature(data_result$data_bundle())
    old <- previous_dataset_signature()
    if (!is.null(old) && !identical(old, sig)) {
      clear_report_registry(report_registry, "all")
      shiny::showNotification(
        "Report Registry cleared because a new dataset was loaded.",
        type = "message", duration = 5
      )
    }
    previous_dataset_signature(sig)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$btn_load_example, {
    shiny::removeModal()
    data_result$load_example()
  })

  shiny::observeEvent(input$btn_upload_own, {
    shiny::removeModal()
  })

  shiny::observeEvent(input$main_navbar, {
    empty_tabs <- character(0)
    if (input$main_navbar %in% empty_tabs) {
      shiny::showNotification(
        "This module will be available in a future update.",
        type = "message", duration = 4
      )
    }

    data_tabs <- c("tab_eda", "tab_anova", "tab_stability", "tab_adaptation", "tab_spatial", "tab_reports")
    if (input$main_navbar %in% data_tabs) {
      db <- data_result$data_bundle()
      if (is.null(db)) {
        shiny::showNotification(
          LABELS$no_data_warning,
          type = "warning", duration = 5
        )
      }
    }
  })
}
