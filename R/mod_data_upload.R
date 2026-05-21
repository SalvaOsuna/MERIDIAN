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
      shiny::tags$h6(shiny::icon("sliders-h"), " Experimental Design"),
      shiny::selectInput(
        ns("design_manual"),
        "Design Selection",
        choices = c(
          "Auto-detect" = "Auto",
          "RCBD" = "RCBD",
          "Alpha-Lattice" = "Alpha-Lattice",
          "Augmented" = "Augmented",
          "Unbalanced" = "Unbalanced"
        ),
        selected = "Auto"
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'Augmented'", ns("design_manual")),
        shinyWidgets::pickerInput(
          ns("aug_checks"),
          "Control Checks (Genotypes)",
          choices = NULL,
          multiple = TRUE,
          options = list(
            `actions-box` = TRUE,
            `live-search` = TRUE,
            `selected-text-format` = "count > 3",
            `count-selected-text` = "{0} checks selected"
          )
        )
      ),

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
      shiny::selectInput(ns("col_pheno_flowering"), "Days to Flowering (Optional)", choices = NULL),
      shiny::selectInput(ns("col_pheno_maturity"),  "Days to Maturity (Optional)",  choices = NULL),

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
    shiny::tags$section(
      class = "meridian-dashboard-intro",
      shiny::tags$div(
        class = "meridian-intro-copy",
        shiny::tags$span(class = "meridian-eyebrow", "Trial Overview"),
        shiny::tags$h1("Plant phenotype workspace"),
        shiny::tags$p(
          "Load a MET dataset, map the experimental design, and move from raw trial records ",
          "to trait summaries, field maps, stability models, and report-ready outputs."
        ),
        shiny::tags$div(
          class = "meridian-chip-row",
          shiny::tags$span(shiny::icon("seedling"), " Phenotype traits"),
          shiny::tags$span(shiny::icon("map"), " Field layout"),
          shiny::tags$span(shiny::icon("chart-line"), " Model outputs")
        )
      )
    ),

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
        tryCatch({
          read_met_file(
            path   = input$file_upload$datapath,
            sep    = input$csv_sep,
            header = input$header
          )
        }, error = function(e) {
          shiny::showNotification(
            paste("Error reading phenotypic data file:", e$message),
            type = "error",
            duration = 10
          )
          NULL
        })
      } else if (isTRUE(example_loaded())) {
        tryCatch({
          read_met_file(
            path   = app_sys("extdata", "example_phenotypic.csv"),
            sep    = ",",
            header = TRUE
          )
        }, error = function(e) {
          shiny::showNotification(
            paste("Error reading example phenotypic data:", e$message),
            type = "error",
            duration = 10
          )
          NULL
        })
      } else {
        NULL
      }
    })

    # ---- Reactive: Raw environmental data ----
    raw_env_data <- shiny::reactive({
      if (!is.null(input$file_env_upload)) {
        tryCatch({
          read_met_file(
            path   = input$file_env_upload$datapath,
            sep    = input$csv_sep_env,
            header = input$header
          )
        }, error = function(e) {
          shiny::showNotification(
            paste("Error reading environmental data file:", e$message),
            type = "error",
            duration = 10
          )
          NULL
        })
      } else if (isTRUE(example_loaded())) {
        tryCatch({
          read_met_file(
            path   = app_sys("extdata", "example_envdata.csv"),
            sep    = ",",
            header = TRUE
          )
        }, error = function(e) {
          shiny::showNotification(
            paste("Error reading example environmental data:", e$message),
            type = "error",
            duration = 10
          )
          NULL
        })
      } else {
        NULL
      }
    })

    # ---- Reactive: Example data flag ----
    example_loaded <- shiny::reactiveVal(FALSE)

    # ---- Reactive: Processed environmental data with weather indices ----
    processed_env_data <- shiny::reactive({
      raw_env <- raw_env_data()
      if (is.null(raw_env)) return(NULL)
      
      v_env <- validate_environmental_data(raw_env)
      if (!v_env$valid) {
        shiny::showNotification(
          paste("Environmental Data validation failed:", paste(v_env$errors, collapse = "; ")),
          type = "error",
          duration = 10
        )
        return(NULL)
      }
      
      if (length(v_env$warnings) > 0) {
        lapply(v_env$warnings, function(w) {
          shiny::showNotification(w, type = "warning", duration = 8)
        })
      }
      
      col_names <- names(raw_env)
      lat_col  <- col_names[grep("^lat", col_names, ignore.case = TRUE)][1]
      lon_col  <- col_names[grep("^lon", col_names, ignore.case = TRUE)][1]
      pdate_col <- col_names[grep("plant", col_names, ignore.case = TRUE)][1]
      hdate_col <- col_names[grep("harvest", col_names, ignore.case = TRUE)][1]
      
      if (is.na(lat_col) || is.na(lon_col) || is.na(pdate_col) || is.na(hdate_col)) {
        return(raw_env)
      }
      
      res <- tryCatch({
        shiny::withProgress(
          message = "Fetching NASA POWER daily weather...",
          value = 0,
          {
            dtf_means <- NULL
            dtm_means <- NULL
            pheno_df <- raw_data()
            pheno_env_col <- input$col_env
            
            if (!is.null(pheno_df) && !is.null(pheno_env_col) && pheno_env_col %in% names(pheno_df)) {
              flowering_col <- input$col_pheno_flowering
              maturity_col <- input$col_pheno_maturity
              
              if (!is.null(flowering_col) && flowering_col != "" && flowering_col %in% names(pheno_df)) {
                dtf_vals <- suppressWarnings(as.numeric(pheno_df[[flowering_col]]))
                if (any(!is.na(dtf_vals))) {
                  dtf_agg <- aggregate(
                    dtf_vals, 
                    by = list(Env = pheno_df[[pheno_env_col]]), 
                    FUN = mean, 
                    na.rm = TRUE
                  )
                  dtf_means <- stats::setNames(dtf_agg$x, as.character(dtf_agg$Env))
                }
              }
              
              if (!is.null(maturity_col) && maturity_col != "" && maturity_col %in% names(pheno_df)) {
                dtm_vals <- suppressWarnings(as.numeric(pheno_df[[maturity_col]]))
                if (any(!is.na(dtm_vals))) {
                  dtm_agg <- aggregate(
                    dtm_vals, 
                    by = list(Env = pheno_df[[pheno_env_col]]), 
                    FUN = mean, 
                    na.rm = TRUE
                  )
                  dtm_means <- stats::setNames(dtm_agg$x, as.character(dtm_agg$Env))
                }
              }
            }
            
            process_environmental_covariates(
              raw_env, 
              dtf_means = dtf_means, 
              dtm_means = dtm_means, 
              session = session
            )
          }
        )
      }, error = function(e) {
        shiny::showNotification(
          paste("NASA POWER weather retrieval is currently unavailable: ", e$message),
          type = "error",
          duration = 10
        )
        NULL
      })
      
      res
    })

    shiny::observeEvent(raw_data(), {
      req(raw_data())
      df <- raw_data()
      col_names <- names(df)
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

      # Update pheno mapping dropdowns
      shiny::updateSelectInput(session, "col_pheno_flowering",
        choices  = choices_with_none,
        selected = detected$flowering %||% ""
      )
      
      shiny::updateSelectInput(session, "col_pheno_maturity",
        choices  = choices_with_none,
        selected = detected$maturity %||% ""
      )

      # Detect numeric columns for traits
      numeric_cols <- col_names[sapply(df, is.numeric)]
      # Remove already-mapped columns from trait candidates
      mapped <- c(detected$genotype, detected$environment,
                  detected$rep, detected$block, detected$row, detected$col,
                  detected$flowering, detected$maturity)
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

    # ---- Observe: Update augmented check choices from selected genotype column ----
    shiny::observeEvent(
      list(raw_data(), input$col_gen, input$design_manual),
      {
        req(raw_data(), input$col_gen)
        df <- raw_data()
        if (!input$col_gen %in% names(df)) return(NULL)

        gen_vals <- sort(unique(as.character(df[[input$col_gen]])))
        gen_vals <- gen_vals[!is.na(gen_vals) & nzchar(gen_vals)]
        if (length(gen_vals) == 0) return(NULL)

        detected_aug_checks <- character(0)
        design_sel <- input$design_manual %||% "Auto"
        if (design_sel %in% c("Auto", "Augmented")) {
          rep_guess <- input$col_rep
          env_guess <- input$col_env
          if (!is.null(rep_guess) && rep_guess %in% names(df) &&
              !is.null(env_guess) && env_guess %in% names(df)) {
            rep_counts <- df |>
              dplyr::group_by(.data[[env_guess]], .data[[input$col_gen]]) |>
              dplyr::summarise(n_rep = dplyr::n_distinct(.data[[rep_guess]]), .groups = "drop") |>
              dplyr::group_by(.data[[input$col_gen]]) |>
              dplyr::summarise(avg_rep = mean(n_rep, na.rm = TRUE), .groups = "drop")
            if (nrow(rep_counts) > 0) {
              max_rep <- max(rep_counts$avg_rep, na.rm = TRUE)
              detected_aug_checks <- rep_counts |>
                dplyr::filter(avg_rep >= max_rep - 1e-8, avg_rep > 1) |>
                dplyr::pull(.data[[input$col_gen]]) |>
                as.character()
            }
          }
        }

        selected_now <- intersect(input$aug_checks %||% character(0), gen_vals)
        selected_final <- unique(c(selected_now, detected_aug_checks))
        shinyWidgets::updatePickerInput(
          session, "aug_checks",
          choices = gen_vals,
          selected = selected_final
        )
      },
      ignoreInit = FALSE
    )

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
      detected_design <- detect_design(df, gen_col, env_col, rep_col, block_col)
      selected_design <- input$design_manual %||% "Auto"

      design <- if (!identical(selected_design, "Auto")) {
        list(
          design = selected_design,
          description = paste0(
            selected_design, " selected manually by user."
          ),
          icon = "sliders-h",
          source = "Manual"
        )
      } else {
        detected_design$source <- "Auto"
        detected_design
      }

      aug_checks <- character(0)
      if (identical(design$design, "Augmented")) {
        aug_checks <- intersect(input$aug_checks %||% character(0), unique(as.character(df[[gen_col]])))
      }

      list(
        data       = df,
        gen_col    = gen_col,
        env_col    = env_col,
        rep_col    = rep_col,
        block_col  = block_col,
        row_col    = row_col,
        col_col    = col_col,
        traits     = traits,
        col_pheno_flowering = if (is.null(input$col_pheno_flowering) || input$col_pheno_flowering == "") NULL else input$col_pheno_flowering,
        col_pheno_maturity  = if (is.null(input$col_pheno_maturity) || input$col_pheno_maturity == "") NULL else input$col_pheno_maturity,
        design     = design,
        augmented_checks = aug_checks,
        validation = validation,
        env_data   = processed_env_data()
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
      req(processed_env_data())
      DT::datatable(
        processed_env_data(),
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
      checks <- data_bundle()$augmented_checks %||% character(0)

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
          shiny::tags$span(
            style = "margin-left: 8px; color: #666; font-size: 0.8rem;",
            paste0("(", design$source %||% "Auto", ")")
          ),
          style = "margin-bottom: 0.5rem;"
        ),
        shiny::tags$p(design$description, style = "font-size: 0.9rem; color: #555;"),
        if (identical(design$design, "Augmented")) {
          shiny::tags$p(
            paste0("Selected check genotypes: ", length(checks),
                   if (length(checks) > 0) paste0(" (", paste(utils::head(checks, 8), collapse = ", "),
                                                if (length(checks) > 8) ", ..." else "", ")") else ""),
            style = "font-size: 0.85rem; color: #444;"
          )
        }
      )
    })

    # ---- Output: Validation messages ----
    output$validation_messages <- shiny::renderUI({
      req(data_bundle())
      db <- data_bundle()
      v <- db$validation

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

      # Augmented-design specific validation
      if (db$design$design == "Augmented" &&
          length(db$augmented_checks %||% character(0)) == 0) {
        msgs <- shiny::tagList(
          msgs,
          shiny::tags$div(
            class = "alert alert-warning py-1 px-2 mb-1",
            style = "font-size: 0.85rem;",
            shiny::icon("exclamation-triangle"),
            " Augmented design selected but no control checks were selected."
          )
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
        meridian_plotly_layout(
          margin = list(b = 80),
          legend = list(orientation = "h", y = -0.2)
        )
    })

    # ---- Download: Example dataset ----
    output$download_example <- shiny::downloadHandler(
      filename = function() "example_phenotypic.csv",
      content  = function(file) {
        file.copy(app_sys("extdata", "example_phenotypic.csv"), file)
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

#' Validate environmental covariates dataset
#' @param env_df Data frame
#' @return List of errors and warnings
validate_environmental_data <- function(env_df) {
  errors <- character(0)
  warnings <- character(0)
  
  if (is.null(env_df) || nrow(env_df) == 0) {
    return(list(valid = FALSE, errors = "Environmental data is empty.", warnings = warnings))
  }
  
  col_names <- names(env_df)
  env_col  <- col_names[grep("^(env|site|loc)", col_names, ignore.case = TRUE)][1]
  lat_col  <- col_names[grep("^lat", col_names, ignore.case = TRUE)][1]
  lon_col  <- col_names[grep("^lon", col_names, ignore.case = TRUE)][1]
  pdate_col <- col_names[grep("plant", col_names, ignore.case = TRUE)][1]
  hdate_col <- col_names[grep("harvest", col_names, ignore.case = TRUE)][1]
  
  if (is.na(env_col)) {
    errors <- c(errors, "An 'Environment' column is required.")
  }
  
  has_gps_dates <- !is.na(lat_col) && !is.na(lon_col) && !is.na(pdate_col) && !is.na(hdate_col)
  
  if (!has_gps_dates) {
    num_cols <- col_names[sapply(env_df, is.numeric)]
    num_cols <- setdiff(num_cols, c(env_col, lat_col, lon_col))
    if (length(num_cols) == 0) {
      errors <- c(errors, "Environmental data must either contain all five geolocational columns (Environment, Latitude, Longitude, PlantingDate, HarvestDate) or pre-computed numeric covariates.")
    } else {
      warnings <- c(warnings, "Missing Latitude, Longitude, PlantingDate, or HarvestDate. Online weather retrieval via NASA POWER is disabled; relying on pre-computed covariates.")
    }
  } else {
    lats <- suppressWarnings(as.numeric(env_df[[lat_col]]))
    lons <- suppressWarnings(as.numeric(env_df[[lon_col]]))
    if (any(is.na(lats)) || any(lats < -90) || any(lats > 90)) {
      errors <- c(errors, "Latitude values must be numeric and between -90 and 90.")
    }
    if (any(is.na(lons)) || any(lons < -180) || any(lons > 180)) {
      errors <- c(errors, "Longitude values must be numeric and between -180 and 180.")
    }
    
    p_dates <- tryCatch(parse_date_robustly(env_df[[pdate_col]]), error = function(e) NULL)
    h_dates <- tryCatch(parse_date_robustly(env_df[[hdate_col]]), error = function(e) NULL)
    
    if (is.null(p_dates) || any(is.na(p_dates))) {
      errors <- c(errors, "PlantingDate column has invalid or unparseable dates.")
    }
    if (is.null(h_dates)) {
      errors <- c(errors, "HarvestDate column has invalid or unparseable dates.")
    } else if (any(is.na(h_dates))) {
      # Some are NA! Calculate mean duration from valid ones
      valid_idx <- !is.na(h_dates) & !is.na(p_dates)
      mean_dur <- 120
      if (sum(valid_idx) > 0) {
        durations <- as.numeric(h_dates[valid_idx] - p_dates[valid_idx])
        valid_durs <- durations[durations >= 30 & durations <= 200]
        if (length(valid_durs) > 0) {
          mean_dur <- round(mean(valid_durs))
        } else {
          mean_dur <- round(mean(durations))
          if (mean_dur < 30 || mean_dur > 200) mean_dur <- 120
        }
      }
      
      missing_envs <- env_df[[env_col]][is.na(h_dates)]
      warnings <- c(warnings, sprintf(
        "Some environments (%s) have missing HarvestDate values. They will be dynamically estimated using a crop cycle of %d days from planting.", 
        paste(missing_envs, collapse = ", "), 
        mean_dur
      ))
    }
    
    if (!is.null(p_dates) && !is.null(h_dates) && any(!is.na(p_dates)) && any(!is.na(h_dates))) {
      if (any(h_dates <= p_dates, na.rm = TRUE)) {
        errors <- c(errors, "HarvestDate must be strictly after PlantingDate.")
      }
    }
  }
  
  list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings
  )
}
