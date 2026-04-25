# =============================================================================
# MERIDIAN — Module 4: Stability Analysis
# Native AMMI (ammi.R + ammi_plots.R), GGE (metan), native fast Eberhart-Russell,
# Wricke, Shukla, Combined rankings
# =============================================================================


# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
mod_stability_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      width = 300,
      title = shiny::tagList(shiny::icon("balance-scale"), " ", LABELS$m4_title),

      shiny::selectInput(ns("trait"), LABELS$m2_select_trait, choices = NULL),

      shiny::tags$hr(),

      # AMMI-specific: number of axes
      shiny::numericInput(ns("ammi_n_axes"), "AMMI Axes to Retain",
        value = 2, min = 1, max = 10, step = 1
      ),

      # AMMI1 axis selector
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'AMMI1'", ns("stability_tabs")),
        shiny::selectInput(ns("ammi1_axis"), "IPCA Axis (Y)",
          choices = paste0("IPCA", 1:4), selected = "IPCA1")
      ),

      # AMMI2 axes selector
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'AMMI2'", ns("stability_tabs")),
        shiny::selectInput(ns("ammi2_x"), "X-axis",
          choices = 1:4, selected = 1),
        shiny::selectInput(ns("ammi2_y"), "Y-axis",
          choices = 1:4, selected = 2)
      ),

      # GGE biplot type selector
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'GGE Biplot'", ns("stability_tabs")),
        shiny::selectInput(ns("gge_type"), "Biplot Type",
          choices  = c(
            "Basic"                = 1,
            "Mean vs. Stability"   = 2,
            "Which-Won-Where"      = 3,
            "Discriminativeness"   = 4,
            "Ranking Genotypes"    = 6,
            "Ranking Environments" = 8
          ),
          selected = 2
        )
      ),

      # Stability metric selector
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'Stability'", ns("stability_tabs")),
        shiny::selectInput(ns("stability_metric"), "Stability Metric",
          choices  = c("WAAS", "ASV", "SIPC"),
          selected = "WAAS"
        )
      ),

      shiny::tags$hr(),

      shiny::actionButton(ns("run_stability"), LABELS$run_analysis,
        icon  = shiny::icon("play"),
        class = "btn-success btn-lg w-100",
        style = "padding: 0.8rem; font-weight: 600;"
      ),

      shiny::tags$hr(),

      # Info tooltips
      shiny::tags$div(
        class = "info-tooltip-text",
        style = "font-size: 0.8rem; color: #777;",
        shiny::tags$p(
          shiny::icon("info-circle"),
          " AMMI: Native SVD decomposition of G x E interaction matrix."
        ),
        shiny::tags$p(
          shiny::icon("info-circle"),
          " AMMI1: Mean vs IPCA axis (stability vs performance)."
        ),
        shiny::tags$p(
          shiny::icon("info-circle"),
          " AMMI2: IPCA1 vs IPCA2 biplot (interaction patterns)."
        ),
        shiny::tags$p(
          shiny::icon("info-circle"),
          " GGE: G + GxE biplot via metan (which-won-where, etc.)."
        ),
        shiny::tags$p(
          shiny::icon("info-circle"),
          " Eberhart-Russell: bi=1 (avg stability), S\u00B2di=0 (predictable)."
        )
      )
    ),

    # ---- Main Panel: Sub-tabs ----
    bslib::navset_card_tab(
      id         = ns("stability_tabs"),
      title      = shiny::tagList(shiny::icon("balance-scale"), " Stability Analysis"),
      full_screen = TRUE,

      # Tab 1: AMMI1 Biplot (Mean vs IPCA)
      bslib::nav_panel(
        title = "AMMI1",
        style = "min-height: 500px;",
        bslib::card(
          bslib::card_header("AMMI1 Biplot: Mean vs IPCA"),
          full_screen = TRUE,
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("plot_ammi1"), height = "520px"),
            type = 6, color = "#2c7a51"
          )
        ),
        bslib::card(
          bslib::card_header("AMMI ANOVA"),
          full_screen = TRUE,
          shinycssloaders::withSpinner(
            DT::dataTableOutput(ns("ammi_anova")),
            type = 6, color = "#2c7a51"
          )
        )
      ),

      # Tab 2: AMMI2 Biplot (IPCA1 vs IPCA2)
      bslib::nav_panel(
        title = "AMMI2",
        style = "min-height: 500px;",
        bslib::card(
          bslib::card_header("AMMI2 Biplot: IPCA1 vs IPCA2"),
          full_screen = TRUE,
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("plot_ammi2"), height = "520px"),
            type = 6, color = "#2c7a51"
          )
        ),
        bslib::card(
          bslib::card_header("GxE Variance Explained by IPCA Axis"),
          full_screen = TRUE,
          shinycssloaders::withSpinner(
            shiny::plotOutput(ns("plot_variance"), height = "320px"),
            type = 6, color = "#2c7a51"
          )
        )
      ),

      # Tab 3: GxE Interaction Heatmap
      bslib::nav_panel(
        title = "GxE Heatmap",
        style = "min-height: 500px;",
        bslib::card(
          bslib::card_header("GxE Interaction Residuals (from additive model)"),
          full_screen = TRUE,
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("plot_heatmap"), height = "500px"),
            type = 6, color = "#2c7a51"
          )
        )
      ),

      # Tab 4: Stability Ranking (WAAS/ASV/SIPC)
      bslib::nav_panel(
        title = "Stability",
        style = "min-height: 400px;",
        bslib::layout_column_wrap(
          width = 1 / 2,
          fill  = FALSE,
          bslib::card(
            bslib::card_header("Mean vs Stability Plot"),
            full_screen = TRUE,
            shinycssloaders::withSpinner(
              plotly::plotlyOutput(ns("plot_stability"), height = "420px"),
              type = 6, color = "#2c7a51"
            )
          ),
          bslib::card(
            bslib::card_header(
              shiny::tagList(
                "Stability Table (ASV, WAAS, SIPC)",
                shiny::downloadButton(ns("dl_stability_csv"), "CSV",
                  class = "btn-outline-primary btn-sm float-end ms-1"),
                shiny::downloadButton(ns("dl_stability_xlsx"), "Excel",
                  class = "btn-outline-primary btn-sm float-end")
              )
            ),
            full_screen = TRUE,
            shinycssloaders::withSpinner(
              DT::dataTableOutput(ns("table_stability")),
              type = 6, color = "#2c7a51"
            )
          )
        )
      ),

      # Tab 5: GGE Biplot
      bslib::nav_panel(
        title = "GGE Biplot",
        style = "min-height: 500px;",
        bslib::card(
          bslib::card_header("GGE Biplot"),
          full_screen = TRUE,
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("gge_biplot"), height = "550px"),
            type = 6, color = "#2c7a51"
          )
        )
      ),

      # Tab 6: Eberhart-Russell
      bslib::nav_panel(
        title = "Eberhart-Russell",
        style = "min-height: 400px;",
        bslib::layout_column_wrap(
          width = 1 / 2,
          fill  = FALSE,
          bslib::card(
            bslib::card_header("Regression Parameters"),
            full_screen = TRUE,
            shinycssloaders::withSpinner(
              DT::dataTableOutput(ns("er_table")),
              type = 6, color = "#2c7a51"
            )
          ),
          bslib::card(
            bslib::card_header("bi vs Mean"),
            full_screen = TRUE,
            shinycssloaders::withSpinner(
              plotly::plotlyOutput(ns("er_plot"), height = "400px"),
              type = 6, color = "#2c7a51"
            )
          )
        )
      ),

      # Tab 7: Wricke & Shukla
      bslib::nav_panel(
        title = "Wricke & Shukla",
        style = "min-height: 400px;",
        bslib::layout_column_wrap(
          width = 1 / 2,
          fill  = FALSE,
          bslib::card(
            bslib::card_header("Wricke Ecovalence"),
            full_screen = TRUE,
            shinycssloaders::withSpinner(
              DT::dataTableOutput(ns("wricke_table")),
              type = 6, color = "#2c7a51"
            )
          ),
          bslib::card(
            bslib::card_header("Shukla Stability Variance"),
            full_screen = TRUE,
            shinycssloaders::withSpinner(
              DT::dataTableOutput(ns("shukla_table")),
              type = 6, color = "#2c7a51"
            )
          )
        )
      ),

      # Tab 8: Combined Rankings
      bslib::nav_panel(
        title = shiny::tagList(shiny::icon("trophy"), " Rankings"),
        style = "min-height: 400px;",
        bslib::card(
          bslib::card_header(
            shiny::tagList(
              "Combined Stability Indices (native fast kernel)",
              shiny::downloadButton(ns("dl_rankings_csv"), "CSV",
                class = "btn-outline-primary btn-sm float-end ms-1"),
              shiny::downloadButton(ns("dl_rankings_xlsx"), "Excel",
                class = "btn-outline-primary btn-sm float-end")
            )
          ),
          full_screen = TRUE,
          shinycssloaders::withSpinner(
            DT::dataTableOutput(ns("rankings_table")),
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
mod_stability_server <- function(id, data_result, results_store) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    db <- shiny::reactive({
      req(data_result$data_bundle())
      data_result$data_bundle()
    })

    shiny::observeEvent(db(), {
      traits <- db()$traits
      req(traits, length(traits) > 0)
      shiny::updateSelectInput(session, "trait",
        choices = traits, selected = traits[1])
    })

    # ---- Run all stability analyses on button click ----
    stab_results <- shiny::eventReactive(input$run_stability, {
      req(db(), input$trait)
      shiny::withProgress(message = "Running Stability Analyses...", value = 0, {

        # ---- Prepare metan data ONCE (expensive: pruning, balancing) ----
        shiny::incProgress(0.05, detail = "Preparing balanced data...")
        df_prepared <- safe_analysis(
          prepare_metan_data(db()$data, db()$gen_col, db()$env_col,
                             db()$rep_col, input$trait),
          session
        )

        # Native AMMI (ammi.R)
        shiny::incProgress(0.20, detail = "AMMI analysis (native SVD)")
        ammi_res <- safe_analysis(
          run_ammi(
            data   = db()$data,
            gen    = db()$gen_col,
            env    = db()$env_col,
            rep    = db()$rep_col,
            trait  = input$trait,
            n_axis = input$ammi_n_axes
          ),
          session
        )

        # GGE (metan) — use pre-prepared data
        shiny::incProgress(0.15, detail = "GGE biplot")
        gge_res <- safe_analysis(
          run_gge_fast(db()$data, db()$gen_col, db()$env_col, input$trait),
          session
        )
        if (!is.null(gge_res$n_imputed) && gge_res$n_imputed > 0) {
          shiny::showNotification(
            paste0(
              "GGE: preserved all environments by imputing ",
              gge_res$n_imputed, " missing GxE mean cell(s)."
            ),
            type = "message",
            duration = 8
          )
        }

        # Native stability kernels share one precomputed GxE matrix
        shiny::incProgress(0.05, detail = "Precomputing GxE matrices")
        ge_cache <- safe_analysis(
          build_ge_matrices(df_prepared, db()$gen_col, db()$env_col, input$trait),
          session
        )

        # Eberhart-Russell (native fast)
        shiny::incProgress(0.10, detail = "Eberhart-Russell")
        er_res <- safe_analysis(
          list(model = NULL, params = compute_er_table_fast(ge_cache)),
          session
        )

        # Wricke (native fast)
        shiny::incProgress(0.10, detail = "Wricke ecovalence")
        wricke_res <- safe_analysis(
          list(model = NULL, params = compute_wricke_table_fast(ge_cache)),
          session
        )

        # Shukla (native fast)
        shiny::incProgress(0.10, detail = "Shukla variance")
        shukla_res <- safe_analysis(
          list(model = NULL, params = compute_shukla_table_fast(ge_cache)),
          session
        )

        # Combined stability table (native fast)
        shiny::incProgress(0.15, detail = "Combined stability stats")
        all_res <- safe_analysis(
          run_all_stability_fast(df_prepared, db()$gen_col, db()$env_col,
                            db()$rep_col, input$trait),
          session
        )

        shiny::incProgress(0.10, detail = "Done!")

        res_list <- list(
          ammi   = ammi_res,
          gge    = gge_res,
          er     = er_res,
          wricke = wricke_res,
          shukla = shukla_res,
          all    = all_res,
          trait  = input$trait
        )
        
        results_store[[make_results_key("AMMI", input$trait)]] <- ammi_res
        results_store[[make_results_key("GGE", input$trait)]] <- gge_res
        results_store[[make_results_key("Eberhart_Russell", input$trait)]] <- er_res
        results_store[[make_results_key("Wricke", input$trait)]] <- wricke_res
        results_store[[make_results_key("Shukla", input$trait)]] <- shukla_res
        results_store[[make_results_key("Combined_Stability", input$trait)]] <- all_res
        
        res_list
      })
    })

    # ===========================================================================
    # AMMI Tabs (native, using ammi_plots.R)
    # ===========================================================================

    # ---- AMMI1 Biplot: Mean vs IPCA ----
    output$plot_ammi1 <- plotly::renderPlotly({
      req(stab_results(), stab_results()$ammi)
      tryCatch({
        axis_num <- as.integer(gsub("IPCA", "", input$ammi1_axis))
        plot_ammi1(stab_results()$ammi,
                   axis        = axis_num,
                   interactive = TRUE)
      }, error = function(e) {
        plotly::plotly_empty() |>
          plotly::layout(title = paste("AMMI1 error:", e$message))
      })
    })

    # ---- AMMI2 Biplot: IPCA x vs IPCA y ----
    output$plot_ammi2 <- plotly::renderPlotly({
      req(stab_results(), stab_results()$ammi)
      tryCatch({
        plot_ammi2(stab_results()$ammi,
                   axis_x      = as.integer(input$ammi2_x),
                   axis_y      = as.integer(input$ammi2_y),
                   interactive = TRUE)
      }, error = function(e) {
        plotly::plotly_empty() |>
          plotly::layout(title = paste("AMMI2 error:", e$message))
      })
    })

    # ---- Variance explained bar chart ----
    output$plot_variance <- shiny::renderPlot({
      req(stab_results(), stab_results()$ammi)
      tryCatch(
        plot_ammi_variance(stab_results()$ammi),
        error = function(e) {
          plot.new()
          text(0.5, 0.5, paste("Variance plot error:", e$message),
               cex = 1.2, col = "#c44e52")
        }
      )
    }, res = 96)

    # ---- GxE Interaction Heatmap ----
    output$plot_heatmap <- plotly::renderPlotly({
      req(stab_results(), stab_results()$ammi)
      tryCatch(
        plot_ge_heatmap_ammi(stab_results()$ammi, interactive = TRUE),
        error = function(e) {
          plotly::plotly_empty() |>
            plotly::layout(title = paste("Heatmap error:", e$message))
        }
      )
    })

    # ---- Stability Ranking Plot (WAAS/ASV/SIPC) ----
    output$plot_stability <- plotly::renderPlotly({
      req(stab_results(), stab_results()$ammi)
      tryCatch({
        metric <- input$stability_metric %||% "WAAS"
        plot_stability_ranking(stab_results()$ammi,
                               metric      = metric,
                               interactive = TRUE)
      }, error = function(e) {
        plotly::plotly_empty() |>
          plotly::layout(title = paste("Stability plot error:", e$message))
      })
    })

    # ---- Stability Table ----
    output$table_stability <- DT::renderDataTable({
      req(stab_results(), stab_results()$ammi)
      stab_df <- stab_results()$ammi$stability
      if (is.null(stab_df)) return(NULL)

      # Round numeric columns
      num_cols <- sapply(stab_df, is.numeric)
      stab_df[num_cols] <- lapply(stab_df[num_cols], round, 3)

      DT::datatable(
        stab_df,
        options  = list(pageLength = 25, scrollX = TRUE, dom = "lfrtip"),
        class    = "compact stripe hover",
        rownames = FALSE
      )
    })

    # ---- AMMI ANOVA table ----
    output$ammi_anova <- DT::renderDataTable({
      req(stab_results(), stab_results()$ammi)
      anova_df <- stab_results()$ammi$anova_table
      if (is.null(anova_df)) return(NULL)

      # Round numeric columns
      num_cols <- sapply(anova_df, is.numeric)
      anova_df[num_cols] <- lapply(anova_df[num_cols], round, 4)

      DT::datatable(
        anova_df,
        options  = list(dom = "t", scrollX = TRUE, pageLength = 20),
        class    = "compact stripe hover",
        rownames = FALSE
      )
    })

    # ===========================================================================
    # GGE / Eberhart-Russell / Wricke / Shukla
    # ===========================================================================

    # ---- GGE Biplot (custom plotly) ----
    output$gge_biplot <- plotly::renderPlotly({
      req(stab_results(), stab_results()$gge)
      tryCatch({
        gge_type <- as.integer(input$gge_type)
        plot_gge_custom(stab_results()$gge$model,
                        trait = stab_results()$trait,
                        biplot_type = gge_type)
      }, error = function(e) {
        plotly::plotly_empty() |>
          plotly::layout(title = paste("GGE Error:", e$message))
      })
    })

    # ---- Eberhart-Russell Table ----
    output$er_table <- DT::renderDataTable({
      req(stab_results(), stab_results()$er)
      params <- stab_results()$er$params
      if (is.null(params)) return(NULL)

      params_df <- as.data.frame(params)
      num_cols <- sapply(params_df, is.numeric)
      params_df[num_cols] <- lapply(params_df[num_cols], round, 3)

      DT::datatable(
        params_df,
        options  = list(dom = "lfrtip", scrollX = TRUE, pageLength = 25),
        class    = "compact stripe hover",
        rownames = FALSE
      )
    })

    # ---- Eberhart-Russell Plot ----
    output$er_plot <- plotly::renderPlotly({
      req(stab_results(), stab_results()$er)
      params <- as.data.frame(stab_results()$er$params)

      gen_col_name <- intersect(c("GEN", "gen", "Genotype"), names(params))
      mean_col <- intersect(c("Y", "Mean", "mean", "Overall"), names(params))
      bi_col   <- intersect(c("b1", "bi", "b", "slope"), names(params))

      if (length(gen_col_name) == 0 || length(mean_col) == 0 || length(bi_col) == 0) {
        return(
          plotly::plotly_empty() |>
            plotly::layout(title = paste("Column mismatch. Available:",
                                         paste(names(params), collapse = ", ")))
        )
      }

      plotly::plot_ly(
        data = params,
        x    = ~get(mean_col[1]),
        y    = ~get(bi_col[1]),
        text = ~get(gen_col_name[1]),
        type = "scatter", mode = "markers+text",
        marker   = list(color = "#2c7a51", size = 10),
        textposition = "top center",
        textfont = list(size = 9),
        hovertemplate = paste0(
          "%{text}<br>Mean = %{x:.2f}<br>bi = %{y:.3f}<extra></extra>"
        )
      ) |>
        plotly::layout(
          title  = paste("Eberhart-Russell:", stab_results()$trait),
          xaxis  = list(title = "Mean"),
          yaxis  = list(title = "Regression Coefficient (bi)"),
          shapes = list(
            list(type = "line", y0 = 1, y1 = 1, x0 = 0, x1 = 1,
                 xref = "paper", line = list(dash = "dash", color = "#c44e52"))
          )
        )
    })

    # ---- Wricke & Shukla Tables ----
    render_stability_dt <- function(res) {
      if (is.null(res)) return(NULL)
      params_df <- as.data.frame(res$params)
      num_cols <- sapply(params_df, is.numeric)
      params_df[num_cols] <- lapply(params_df[num_cols], round, 3)

      DT::datatable(
        params_df,
        options  = list(dom = "lfrtip", scrollX = TRUE, pageLength = 25),
        class    = "compact stripe hover",
        rownames = FALSE
      )
    }

    output$wricke_table <- DT::renderDataTable({
      req(stab_results(), stab_results()$wricke)
      render_stability_dt(stab_results()$wricke)
    })

    output$shukla_table <- DT::renderDataTable({
      req(stab_results(), stab_results()$shukla)
      render_stability_dt(stab_results()$shukla)
    })

    # ---- Combined Rankings ----
    rankings_data <- shiny::reactive({
      req(stab_results(), stab_results()$all)
      stability_ranking(stab_results()$all$stats_table)
    })

    output$rankings_table <- DT::renderDataTable({
      req(rankings_data())
      DT::datatable(
        rankings_data(),
        options  = list(dom = "lfrtip", scrollX = TRUE, pageLength = 25),
        class    = "compact stripe hover",
        rownames = FALSE
      )
    })

    # ===========================================================================
    # Downloads
    # ===========================================================================
    output$dl_stability_csv <- shiny::downloadHandler(
      filename = function() paste0("ammi_stability_", input$trait, ".csv"),
      content  = function(file) readr::write_csv(stab_results()$ammi$stability, file)
    )
    output$dl_stability_xlsx <- shiny::downloadHandler(
      filename = function() paste0("ammi_stability_", input$trait, ".xlsx"),
      content  = function(file) openxlsx::write.xlsx(stab_results()$ammi$stability, file)
    )
    output$dl_rankings_csv <- shiny::downloadHandler(
      filename = function() paste0("stability_rankings_", input$trait, ".csv"),
      content  = function(file) readr::write_csv(rankings_data(), file)
    )
    output$dl_rankings_xlsx <- shiny::downloadHandler(
      filename = function() paste0("stability_rankings_", input$trait, ".xlsx"),
      content  = function(file) openxlsx::write.xlsx(rankings_data(), file)
    )

    # ---- Return ----
    return(list(
      stab_results = stab_results
    ))
  })
}
