# =============================================================================
# MERIDIAN — Module 6: Spatial Trends
# SpATS-based spatial visualization and adjusted means
# =============================================================================

mod_spatial_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      width = 350,
      title = shiny::tagList(shiny::icon("map"), LABELS$m7_title),

      shiny::selectInput(ns("trait"), LABELS$m7_select_trait, choices = NULL),
      
      shiny::tags$hr(),
      shiny::tags$h6("Single Environment Analysis"),
      shiny::selectInput(ns("env"), LABELS$m7_select_env, choices = NULL),

      shiny::selectInput(ns("fixed_terms"), LABELS$m7_fixed_terms, choices = NULL, multiple = TRUE),
      shiny::selectInput(ns("random_terms"), LABELS$m7_random_terms, choices = NULL, multiple = TRUE),

      shiny::checkboxInput(ns("gen_random"), "Genotype as Random (BLUPs)", value = TRUE),

      bslib::layout_columns(
        col_widths = c(6, 6),
        shiny::numericInput(ns("nseg_row"), LABELS$m7_nseg_row, value = 10, min = 2),
        shiny::numericInput(ns("nseg_col"), LABELS$m7_nseg_col, value = 10, min = 2)
      ),

      shiny::actionButton(ns("run_spats"), "Fit SpATS (Single Env)", class = "btn-primary w-100 mb-2", icon = shiny::icon("play")),
      
      shiny::tags$hr(),
      shiny::tags$h6("Across Environments Analysis"),
      shiny::p(style = "font-size: 0.85rem; color: #666;", 
               "Calculates adjusted means for all environments to obtain an across-environment BLUE/BLUP summary."),
      shiny::actionButton(ns("run_spats_all"), "Calculate Across Environments", class = "btn-success w-100", icon = shiny::icon("globe"))
    ),

    # Main Panel
    bslib::navset_card_tab(
      full_screen = TRUE,
      bslib::nav_panel(
        "Spatial Trend (Single Env)",
        shiny::div(
          style = "min-height: 500px;",
          shiny::uiOutput(ns("spats_message")),
          shinycssloaders::withSpinner(
            shiny::plotOutput(ns("spats_plot"), height = "700px"),
            type = 6, color = "#2c7a51"
          )
        )
      ),
      bslib::nav_panel(
        "Within Environment Means",
        shiny::div(
          shiny::downloadButton(ns("download_means"), "Download Adjusted Means", class = "btn-outline-primary mb-3"),
          shinycssloaders::withSpinner(
            DT::dataTableOutput(ns("means_table")),
            type = 6, color = "#2c7a51"
          )
        )
      ),
      bslib::nav_panel(
        "Across Environment Means",
        shiny::div(
          shiny::downloadButton(ns("download_means_all"), "Download Across-Env Means", class = "btn-outline-primary mb-3"),
          shinycssloaders::withSpinner(
            DT::dataTableOutput(ns("means_all_table")),
            type = 6, color = "#2c7a51"
          )
        )
      )
    )
  )
}

mod_spatial_server <- function(id, data_result) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Update UI choices based on data ----
    shiny::observe({
      db <- data_result$data_bundle()
      req(db)

      shiny::updateSelectInput(session, "trait", choices = db$traits)
      
      envs <- unique(db$data[[db$env_col]])
      shiny::updateSelectInput(session, "env", choices = envs)

      candidates <- c(db$rep_col, db$block_col)
      candidates <- candidates[!is.null(candidates) & candidates != ""]
      
      shiny::updateSelectInput(session, "fixed_terms", choices = candidates)
      shiny::updateSelectInput(session, "random_terms", choices = candidates)
    })

    # ---- Update default nseg based on selected env ----
    shiny::observeEvent(c(input$env, data_result$data_bundle()), {
      db <- data_result$data_bundle()
      req(db, input$env)
      
      if (!is.null(db$row_col) && db$row_col != "" && !is.null(db$col_col) && db$col_col != "") {
        env_data <- db$data[db$data[[db$env_col]] == input$env, ]
        n_row <- length(unique(env_data[[db$row_col]]))
        n_col <- length(unique(env_data[[db$col_col]]))
        
        # SpATS default recommendation: nseg = number of coordinates (or max 10/20)
        shiny::updateNumericInput(session, "nseg_row", value = max(10, n_row))
        shiny::updateNumericInput(session, "nseg_col", value = max(10, n_col))
      }
    })

    # ---- Helper: Fit SpATS model for a single dataset ----
    fit_spats <- function(env_data, trait, row_col, col_col, gen_col, fixed_terms, random_terms, nseg_row, nseg_col, gen_random) {
      env_data[[row_col]] <- as.numeric(as.character(env_data[[row_col]]))
      env_data[[col_col]] <- as.numeric(as.character(env_data[[col_col]]))
      env_data[[gen_col]] <- as.factor(env_data[[gen_col]])

      # Fixed terms
      fixed_form <- as.formula(paste(trait, "~ 1"))
      if (length(fixed_terms) > 0) {
        for (term in fixed_terms) {
          env_data[[term]] <- as.factor(env_data[[term]])
        }
        fixed_form <- as.formula(paste(trait, "~", paste(fixed_terms, collapse = " + ")))
      }

      # Random terms
      random_form <- NULL
      if (length(random_terms) > 0) {
        for (term in random_terms) {
          env_data[[term]] <- as.factor(env_data[[term]])
        }
        random_form <- as.formula(paste("~", paste(random_terms, collapse = " + ")))
      }

      # Spatial formula
      spatial_form <- as.formula(paste0("~ PSANOVA(", col_col, ", ", row_col, ", nseg = c(", nseg_col, ", ", nseg_row, "))"))

      SpATS::SpATS(
        response = trait,
        spatial = spatial_form,
        genotype = gen_col,
        fixed = fixed_form,
        random = random_form,
        data = env_data,
        genotype.as.random = gen_random
      )
    }

    # ---- Fit Single Env SpATS Model ----
    spats_model <- shiny::eventReactive(input$run_spats, {
      db <- data_result$data_bundle()
      req(db)

      if (is.null(db$row_col) || is.null(db$col_col) || db$row_col == "" || db$col_col == "") {
        shiny::showNotification("Row and Column variables must be mapped in the Data Upload tab for Spatial Analysis.", type = "error")
        return(NULL)
      }

      env_data <- db$data[db$data[[db$env_col]] == input$env, ]

      safe_analysis({
        fit_spats(
          env_data = env_data,
          trait = input$trait,
          row_col = db$row_col,
          col_col = db$col_col,
          gen_col = db$gen_col,
          fixed_terms = input$fixed_terms,
          random_terms = input$random_terms,
          nseg_row = input$nseg_row,
          nseg_col = input$nseg_col,
          gen_random = input$gen_random
        )
      }, session)
    })

    # ---- Output: Spatial Trend Plot ----
    output$spats_message <- shiny::renderUI({
      db <- data_result$data_bundle()
      if (is.null(db$row_col) || is.null(db$col_col) || db$row_col == "" || db$col_col == "") {
        shiny::tags$div(
          class = "alert alert-warning",
          "Please map the Row and Column variables in the Data Upload tab to perform spatial analysis."
        )
      } else {
        NULL
      }
    })

    output$spats_plot <- shiny::renderPlot({
      mod <- spats_model()
      req(mod)
      plot(mod, main = paste("Spatial Trend for", input$trait, "in", input$env))
    })

    # ---- Output: Single Env Adjusted Means ----
    adjusted_means <- shiny::reactive({
      mod <- spats_model()
      req(mod)
      
      db <- data_result$data_bundle()
      preds <- predict(mod, which = db$gen_col)
      
      df <- data.frame(
        Genotype = preds[[db$gen_col]],
        Environment = input$env,
        Adjusted_Mean = round(preds$predicted.values, 4),
        SE = round(preds$standard.errors, 4)
      )
      df
    })

    output$means_table <- DT::renderDataTable({
      req(adjusted_means())
      DT::datatable(
        adjusted_means(),
        options = list(pageLength = 15, dom = "Bfrtip", scrollX = TRUE),
        class = "compact stripe hover",
        rownames = FALSE
      )
    })

    output$download_means <- shiny::downloadHandler(
      filename = function() {
        paste0("Adjusted_Means_", input$env, "_", input$trait, ".csv")
      },
      content = function(file) {
        write.csv(adjusted_means(), file, row.names = FALSE)
      }
    )

    # ---- Across Environments Analysis ----
    across_env_results <- shiny::eventReactive(input$run_spats_all, {
      db <- data_result$data_bundle()
      req(db)

      if (is.null(db$row_col) || is.null(db$col_col) || db$row_col == "" || db$col_col == "") {
        shiny::showNotification("Row and Column variables must be mapped for Spatial Analysis.", type = "error")
        return(NULL)
      }

      envs <- unique(db$data[[db$env_col]])
      all_preds <- list()

      shiny::withProgress(message = 'Fitting SpATS models...', value = 0, {
        for (i in seq_along(envs)) {
          env_name <- envs[i]
          shiny::incProgress(1/length(envs), detail = paste("Environment:", env_name))
          
          env_data <- db$data[db$data[[db$env_col]] == env_name, ]
          
          # Dynamic nseg for this env if default was requested, or use the UI value
          n_row <- length(unique(env_data[[db$row_col]]))
          n_col <- length(unique(env_data[[db$col_col]]))
          
          # Fit model safely
          mod <- tryCatch({
            fit_spats(
              env_data = env_data,
              trait = input$trait,
              row_col = db$row_col,
              col_col = db$col_col,
              gen_col = db$gen_col,
              fixed_terms = input$fixed_terms,
              random_terms = input$random_terms,
              nseg_row = max(10, n_row), # Use env specific nseg for batch process
              nseg_col = max(10, n_col),
              gen_random = input$gen_random
            )
          }, error = function(e) {
            shiny::showNotification(paste("Error in", env_name, ":", e$message), type = "warning")
            NULL
          })
          
          if (!is.null(mod)) {
            preds <- predict(mod, which = db$gen_col)
            all_preds[[env_name]] <- data.frame(
              Genotype = preds[[db$gen_col]],
              Environment = env_name,
              Adjusted_Mean = preds$predicted.values,
              SE = preds$standard.errors
            )
          }
        }
      })
      
      req(length(all_preds) > 0)
      
      # Combine all single environment predictions
      combined_df <- do.call(rbind, all_preds)
      
      # Calculate across environment means (simple average of adjusted means)
      # For a strict two-stage analysis, one would run a meta-analysis or mixed model on the adjusted means.
      # Here we provide the grand mean across environments for each genotype based on SpATS adjusted means.
      across_df <- combined_df |>
        dplyr::group_by(Genotype) |>
        dplyr::summarise(
          Across_Env_Mean = round(mean(Adjusted_Mean, na.rm = TRUE), 4),
          Mean_SE = round(mean(SE, na.rm = TRUE), 4), # Simple average of standard errors (or could pool variances)
          N_Envs = dplyr::n(),
          .groups = "drop"
        ) |>
        dplyr::arrange(dplyr::desc(Across_Env_Mean))
        
      list(combined = combined_df, across = across_df)
    })

    output$means_all_table <- DT::renderDataTable({
      res <- across_env_results()
      req(res)
      DT::datatable(
        res$across,
        options = list(pageLength = 20, dom = "Bfrtip", scrollX = TRUE),
        class = "compact stripe hover",
        rownames = FALSE
      )
    })

    output$download_means_all <- shiny::downloadHandler(
      filename = function() {
        paste0("Across_Env_Adjusted_Means_", input$trait, ".csv")
      },
      content = function(file) {
        req(across_env_results())
        write.csv(across_env_results()$across, file, row.names = FALSE)
      }
    )

  })
}
