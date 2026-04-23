# =============================================================================
# MERIDIAN — Module 1: Data Upload & Validation
# Shiny module for file upload, column mapping, design detection, data preview
# =============================================================================


# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
mod_data_upload_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    fillable = FALSE,
    # ---- Sidebar: Inputs ----
    sidebar = bslib::sidebar(
      width = 320,
      title = shiny::tagList(
        shiny::icon("upload"),
        LABELS$m1_title
      ),

      # File upload
      shiny::fileInput(
        ns("file_upload"),
        label = "Phenotypic Data (Required)",
        accept = c(".csv", ".xlsx", ".xls"),
        placeholder = LABELS$m1_upload_hint
      ),

      shiny::fileInput(
        ns("file_env_upload"),
        label = "Environmental Covariates (Optional)",
        accept = c(".csv", ".xlsx", ".xls"),
        placeholder = "e.g., example_envdata.csv"
      ),

      # CSV options (conditional for phenotypic data)
      shiny::conditionalPanel(
        condition = sprintf(
          "input['%s'] != null && (input['%s'].name.endsWith('.csv') || input['%s'].name.endsWith('.CSV'))",
          ns("file_upload"), ns("file_upload"), ns("file_upload")
        ),
        shiny::selectInput(
          ns("csv_sep"), LABELS$m1_separator,
          choices  = c("Comma (,)" = ",", "Semicolon (;)" = ";",
                       "Tab" = "\t", "Space" = " "),
          selected = ","
        )
      ),

      # CSV options (conditional for env data)
      shiny::conditionalPanel(
        condition = sprintf(
          "input['%s'] != null && (input['%s'].name.endsWith('.csv') || input['%s'].name.endsWith('.CSV'))",
          ns("file_env_upload"), ns("file_env_upload"), ns("file_env_upload")
        ),
        shiny::selectInput(
          ns("csv_sep_env"), "Env. Data Separator",
          choices  = c("Comma (,)" = ",", "Semicolon (;)" = ";",
                       "Tab" = "\t", "Space" = " "),
          selected = ","
        )
      ),

      shiny::checkboxInput(ns("header"), LABELS$m1_header, value = TRUE),

      shiny::tags$hr(),

      # Column mapping
      shiny::tags$h6(
        shiny::icon("columns"), " Column Mapping",
        shiny::tags$span(
          class = "info-tooltip",
          title = "Map your data columns to the expected roles. Auto-detection is attempted based on column names.",
          "?"
        )
      ),

      shiny::selectInput(ns("col_gen"),   LABELS$m1_select_gen,   choices = NULL),
      shiny::selectInput(ns("col_env"),   LABELS$m1_select_env,   choices = NULL),
      shiny::selectInput(ns("col_rep"),   LABELS$m1_select_rep,   choices = NULL),
      shiny::selectInput(ns("col_block"), LABELS$m1_select_block, choices = NULL),
      shiny::selectInput(ns("col_row"),   LABELS$m1_select_row,   choices = NULL),
      shiny::selectInput(ns("col_col"),   LABELS$m1_select_col,   choices = NULL),

      shinyWidgets::pickerInput(
        ns("col_traits"),
        LABELS$m1_select_traits,
        choices  = NULL,
        multiple = TRUE,
        options  = list(
          `actions-box`   = TRUE,
          `live-search`   = TRUE,
          `selected-text-format` = "count > 3",
          `count-selected-text`  = "{0} traits selected"
        )
      ),

      shiny::tags$hr(),

      # Download example
      shiny::downloadButton(
        ns("download_example"),
        LABELS$m1_download_ex,
        class = "btn-outline-primary btn-sm w-100"
      )
    ),

    # ---- Main Panel: Outputs ----
    # Row 1: Value boxes
    bslib::layout_column_wrap(
      width = 1 / 4,
      fill  = FALSE,
      bslib::value_box(
        title = "Observations",
        value = shiny::textOutput(ns("n_obs")),
        showcase = shiny::icon("table"),
        theme = "primary"
      ),
      bslib::value_box(
        title = "Genotypes",
        value = shiny::textOutput(ns("n_gen")),
        showcase = shiny::icon("dna"),
        theme = "success"
      ),
      bslib::value_box(
        title = "Environments",
        value = shiny::textOutput(ns("n_env")),
        showcase = shiny::icon("globe"),
        theme = "info"
      ),
      bslib::value_box(
        title = LABELS$m1_design,
        value = shiny::textOutput(ns("design_type")),
        showcase = shiny::icon("th-large"),
        theme = "warning"
      )
    ),

    # Row 2: Data preview
    bslib::navset_card_tab(
      full_screen = TRUE,
      bslib::nav_panel(
        "Phenotypic Data",
        shiny::tagList(shiny::icon("table-list"), " ", LABELS$m1_preview),
        shiny::div(style = "min-height: 420px;",
          shinycssloaders::withSpinner(
            DT::dataTableOutput(ns("data_table")),
            type  = 6,
            color = "#2c7a51"
          )
        )
      ),
      bslib::nav_panel(
        "Environmental Data",
        shiny::tagList(shiny::icon("leaf"), " Environmental Covariates"),
        shiny::div(style = "min-height: 420px;",
          shinycssloaders::withSpinner(
            DT::dataTableOutput(ns("env_data_table")),
            type  = 6,
            color = "#2c7a51"
          )
        )
      )
    ),

    # Row 3: Missing values + Design details
    bslib::layout_column_wrap(
      width = 1 / 2,
      fill  = FALSE,
      # Missing value summary
      bslib::card(
        bslib::card_header(
          shiny::tagList(shiny::icon("exclamation-triangle"), " ", LABELS$m1_missing)
        ),
        full_screen = TRUE,
        style = "min-height: 280px;",
        DT::dataTableOutput(ns("missing_table"))
      ),
      # Design description
      bslib::card(
        bslib::card_header(
          shiny::tagList(shiny::icon("info-circle"), " Design Details")
        ),
        full_screen = TRUE,
        style = "min-height: 280px;",
        shiny::uiOutput(ns("design_details")),
        shiny::uiOutput(ns("validation_messages"))
      )
    ),

    # Row 4: Descriptive summaries in tabs
    bslib::navset_card_tab(
      title = "Descriptive Statistics",
      full_screen = TRUE,
      bslib::nav_panel(
        LABELS$m1_summary_env,
        shiny::div(style = "min-height: 350px;",
          shinycssloaders::withSpinner(
            DT::dataTableOutput(ns("summary_env_table")),
            type = 6, color = "#2c7a51"
          )
        )
      ),
      bslib::nav_panel(
        LABELS$m1_summary_gen,
        shiny::div(style = "min-height: 350px;",
          shinycssloaders::withSpinner(
            DT::dataTableOutput(ns("summary_gen_table")),
            type = 6, color = "#2c7a51"
          )
        )
      ),
      bslib::nav_panel(
        "Quick Visualization",
        shiny::div(style = "min-height: 350px;",
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("quick_boxplot"), height = "400px"),
            type = 6, color = "#2c7a51"
          )
        )
      )
    )
  )
}


# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
mod_data_upload_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Reactive: Raw uploaded data ----
    raw_data <- shiny::reactive({
      # Check for uploaded file or example data flag
      if (!is.null(input$file_upload)) {
        read_met_file(
          path   = input$file_upload$datapath,
          sep    = input$csv_sep,
          header = input$header
        )
      } else if (isTRUE(example_loaded())) {
        read_met_file(
          path   = file.path("data", "example_phenotypic.csv"),
          sep    = ",",
          header = TRUE
        )
      } else {
        NULL
      }
    })

    # ---- Reactive: Raw environmental data ----
    raw_env_data <- shiny::reactive({
      if (!is.null(input$file_env_upload)) {
        read_met_file(
          path   = input$file_env_upload$datapath,
          sep    = input$csv_sep_env,
          header = input$header
        )
      } else if (isTRUE(example_loaded())) {
        read_met_file(
          path   = file.path("data", "example_envdata.csv"),
          sep    = ",",
          header = TRUE
        )
      } else {
        NULL
      }
    })

    # ---- Reactive: Example data flag ----
    example_loaded <- shiny::reactiveVal(FALSE)

    # ---- Observe: Update column dropdowns when data loads ----
    shiny::observeEvent(raw_data(), {
      req(raw_data())
      df <- raw_data()
      col_names <- names(df)

      # Auto-detect columns
      detected <- auto_detect_columns(col_names)

      # All columns for mapping
      choices_with_none <- c("(none)" = "", col_names)
      choices_cols      <- col_names

      # Update genotype dropdown
      shiny::updateSelectInput(session, "col_gen",
        choices  = choices_cols,
        selected = detected$genotype %||% choices_cols[1]
      )

      # Update environment dropdown
      shiny::updateSelectInput(session, "col_env",
        choices  = choices_cols,
        selected = detected$environment %||% choices_cols[min(2, length(choices_cols))]
      )

      # Update rep dropdown
      shiny::updateSelectInput(session, "col_rep",
        choices  = choices_cols,
        selected = detected$rep %||% choices_cols[min(3, length(choices_cols))]
      )

      # Update block dropdown (with "none" option)
      shiny::updateSelectInput(session, "col_block",
        choices  = choices_with_none,
        selected = detected$block %||% ""
      )

      # Update row and col dropdowns
      shiny::updateSelectInput(session, "col_row",
        choices  = choices_with_none,
        selected = detected$row %||% ""
      )
      
      shiny::updateSelectInput(session, "col_col",
        choices  = choices_with_none,
        selected = detected$col %||% ""
      )

      # Detect numeric columns for traits
      numeric_cols <- col_names[sapply(df, is.numeric)]
      # Remove already-mapped columns from trait candidates
      mapped <- c(detected$genotype, detected$environment,
                  detected$rep, detected$block, detected$row, detected$col)
      trait_candidates <- setdiff(numeric_cols, mapped)

      shinyWidgets::updatePickerInput(session, "col_traits",
        choices  = numeric_cols,
        selected = trait_candidates
      )

      shiny::showNotification(
        paste0("\u2705 Data loaded: ", nrow(df), " rows \u00D7 ", ncol(df), " columns"),
        type = "message", duration = 5
      )
    })

    # ---- Reactive: Validated data bundle ----
    data_bundle <- shiny::reactive({
      req(raw_data())
      df <- raw_data()

      gen_col   <- input$col_gen
      env_col   <- input$col_env
      rep_col   <- input$col_rep
      block_col <- if (is.null(input$col_block) || input$col_block == "") NULL else input$col_block
      row_col   <- if (is.null(input$col_row) || input$col_row == "") NULL else input$col_row
      col_col   <- if (is.null(input$col_col) || input$col_col == "") NULL else input$col_col
      traits    <- input$col_traits

      # Validate
      validation <- validate_met_data(df, gen_col, env_col, rep_col, traits)

      # Detect design
      design <- detect_design(df, gen_col, env_col, rep_col, block_col)

      list(
        data       = df,
        gen_col    = gen_col,
        env_col    = env_col,
        rep_col    = rep_col,
        block_col  = block_col,
        row_col    = row_col,
        col_col    = col_col,
        traits     = traits,
        design     = design,
        validation = validation,
        env_data   = raw_env_data()
      )
    })

    # ---- Outputs: Value boxes ----
    output$n_obs <- shiny::renderText({
      req(raw_data())
      format(nrow(raw_data()), big.mark = ",")
    })

    output$n_gen <- shiny::renderText({
      req(raw_data(), input$col_gen)
      format(dplyr::n_distinct(raw_data()[[input$col_gen]]), big.mark = ",")
    })

    output$n_env <- shiny::renderText({
      req(raw_data(), input$col_env)
      format(dplyr::n_distinct(raw_data()[[input$col_env]]), big.mark = ",")
    })

    output$design_type <- shiny::renderText({
      req(data_bundle())
      data_bundle()$design$design
    })

    # ---- Output: Data preview table ----
    output$data_table <- DT::renderDataTable({
      req(raw_data())
      DT::datatable(
        raw_data(),
        options = list(
          pageLength = 15,
          scrollX    = TRUE,
          dom        = "lfrtip",
          language   = list(
            search      = "Search:",
            lengthMenu  = "Show _MENU_ entries"
          )
        ),
        class    = "compact stripe hover",
        rownames = FALSE
      )
    })

    # ---- Output: Environmental Data preview table ----
    output$env_data_table <- DT::renderDataTable({
      req(raw_env_data())
      DT::datatable(
        raw_env_data(),
        options = list(
          pageLength = 15,
          scrollX    = TRUE,
          dom        = "lfrtip",
          language   = list(
            search      = "Search:",
            lengthMenu  = "Show _MENU_ entries"
          )
        ),
        class    = "compact stripe hover",
        rownames = FALSE
      )
    })

    # ---- Output: Missing value summary ----
    output$missing_table <- DT::renderDataTable({
      req(raw_data())
      missing_df <- compute_missing_summary(raw_data())
      # Only show columns with missing values, or all if none
      if (any(missing_df$N_Missing > 0)) {
        missing_df <- missing_df |> dplyr::filter(N_Missing > 0)
      }
      DT::datatable(
        missing_df,
        options  = list(pageLength = 10, dom = "t", scrollX = TRUE),
        class    = "compact stripe",
        rownames = FALSE
      ) |>
        DT::formatStyle(
          "N_Missing",
          backgroundColor = DT::styleInterval(c(0, 5), c("#e8f5e9", "#fff9c4", "#ffebee"))
        )
    })

    # ---- Output: Design details ----
    output$design_details <- shiny::renderUI({
      req(data_bundle())
      design <- data_bundle()$design

      badge_class <- switch(tolower(design$design),
        "rcbd"        = "badge-rcbd",
        "alpha-lattice" = "badge-alpha",
        "augmented"   = "badge-augmented",
        "badge-unknown"
      )

      shiny::tagList(
        shiny::tags$p(
          shiny::tags$span(class = paste("badge badge-design", badge_class),
                           design$design),
          style = "margin-bottom: 0.5rem;"
        ),
        shiny::tags$p(design$description, style = "font-size: 0.9rem; color: #555;")
      )
    })

    # ---- Output: Validation messages ----
    output$validation_messages <- shiny::renderUI({
      req(data_bundle())
      v <- data_bundle()$validation

      msgs <- shiny::tagList()

      if (length(v$errors) > 0) {
        msgs <- shiny::tagList(msgs, lapply(v$errors, function(e) {
          shiny::tags$div(
            class = "alert alert-danger py-1 px-2 mb-1",
            style = "font-size: 0.85rem;",
            shiny::icon("times-circle"), " ", e
          )
        }))
      }

      if (length(v$warnings) > 0) {
        msgs <- shiny::tagList(msgs, lapply(v$warnings, function(w) {
          shiny::tags$div(
            class = "alert alert-warning py-1 px-2 mb-1",
            style = "font-size: 0.85rem;",
            shiny::icon("exclamation-triangle"), " ", w
          )
        }))
      }

      if (v$valid && length(v$warnings) == 0) {
        msgs <- shiny::tags$div(
          class = "alert alert-success py-1 px-2",
          style = "font-size: 0.85rem;",
          shiny::icon("check-circle"), " All validations passed."
        )
      }

      msgs
    })

    # ---- Output: Summary by Environment ----
    output$summary_env_table <- DT::renderDataTable({
      req(data_bundle())
      db <- data_bundle()
      req(db$env_col, db$traits, length(db$traits) > 0)

      summary_df <- descriptive_summary(db$data, db$env_col, db$traits)

      DT::datatable(
        summary_df,
        options  = list(pageLength = 20, scrollX = TRUE, dom = "lfrtip"),
        class    = "compact stripe hover",
        rownames = FALSE
      ) |>
        DT::formatRound(c("Mean", "SD", "Min", "Max", "CV_pct"), digits = 2)
    })

    # ---- Output: Summary by Genotype ----
    output$summary_gen_table <- DT::renderDataTable({
      req(data_bundle())
      db <- data_bundle()
      req(db$gen_col, db$traits, length(db$traits) > 0)

      summary_df <- descriptive_summary(db$data, db$gen_col, db$traits)

      DT::datatable(
        summary_df,
        options  = list(pageLength = 20, scrollX = TRUE, dom = "lfrtip"),
        class    = "compact stripe hover",
        rownames = FALSE
      ) |>
        DT::formatRound(c("Mean", "SD", "Min", "Max", "CV_pct"), digits = 2)
    })

    # ---- Output: Quick boxplot ----
    output$quick_boxplot <- plotly::renderPlotly({
      req(data_bundle())
      db <- data_bundle()
      req(db$env_col, db$traits, length(db$traits) > 0)

      trait <- db$traits[1]
      p <- plot_boxplots(db$data, trait, db$env_col, db$env_col, show_points = TRUE)
      plotly::ggplotly(p, tooltip = c("x", "y")) |>
        plotly::layout(
          margin = list(b = 80),
          legend = list(orientation = "h", y = -0.2)
        )
    })

    # ---- Download: Example dataset ----
    output$download_example <- shiny::downloadHandler(
      filename = function() "example_phenotypic.csv",
      content  = function(file) {
        file.copy(file.path("data", "example_phenotypic.csv"), file)
      }
    )

    # ---- Public: Load example data (called from app.R) ----
    load_example <- function() {
      example_loaded(TRUE)
    }

    # Return reactive data bundle + load_example function
    list(
      data_bundle  = data_bundle,
      load_example = load_example
    )
  })
}
