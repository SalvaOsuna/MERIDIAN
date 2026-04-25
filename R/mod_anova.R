# =============================================================================
# MERIDIAN — Module 3: ANOVA & Variance Components
# Two-way ANOVA, variance components, heritability, BLUEs, BLUPs
# =============================================================================


# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
mod_anova_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      width = 300,
      title = shiny::tagList(shiny::icon("calculator"), " ", LABELS$m3_title),

      shiny::selectInput(ns("trait"), LABELS$m2_select_trait, choices = NULL),

      shiny::tags$hr(),

      shiny::actionButton(ns("run_anova"), LABELS$run_analysis,
        icon  = shiny::icon("play"),
        class = "btn-success btn-lg w-100",
        style = "padding: 0.8rem; font-weight: 600;"
      ),

      shiny::tags$hr(),

      # Method tooltips
      shiny::tags$div(
        class = "info-tooltip-text",
        style = "font-size: 0.8rem; color: #777;",
        shiny::tags$p(
          shiny::icon("info-circle"),
          " Fixed ANOVA: Tests significance of Genotype (G), Environment (E), and G x E interaction."
        ),
        shiny::tags$p(
          shiny::icon("info-circle"),
          " Mixed Model (REML): Treats genotype as random to estimate variance components and heritability."
        ),
        shiny::tags$p(
          shiny::icon("info-circle"),
          " BLUEs: Best Linear Unbiased Estimates (genotype as fixed effect)."
        ),
        shiny::tags$p(
          shiny::icon("info-circle"),
          " BLUPs: Best Linear Unbiased Predictions (genotype as random effect, shrinkage towards mean)."
        )
      )
    ),

    # ---- Main Panel ----
    bslib::navset_card_tab(
      id         = ns("anova_tabs"),
      title      = shiny::tagList(shiny::icon("calculator"), " ANOVA & Variance Components"),
      full_screen = TRUE,

      # Tab 1: ANOVA Table
      bslib::nav_panel(
        title = shiny::tagList(shiny::icon("table"), " ANOVA Table"),
        style = "min-height: 400px;",
        shiny::uiOutput(ns("anova_summary_boxes")),
        shinycssloaders::withSpinner(
          DT::dataTableOutput(ns("anova_table")),
          type = 6, color = "#2c7a51"
        ),
        shiny::tags$br(),
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("ss_barplot"), height = "350px"),
          type = 6, color = "#2c7a51"
        )
      ),

      # Tab 2: Variance Components
      bslib::nav_panel(
        title = shiny::tagList(shiny::icon("chart-pie"), " Variance Components"),
        style = "min-height: 400px;",
        shiny::uiOutput(ns("heritability_boxes")),
        bslib::layout_column_wrap(
          width = 1 / 2,
          fill  = FALSE,
          bslib::card(
            bslib::card_header("Variance Components (REML)"),
            full_screen = TRUE,
            shinycssloaders::withSpinner(
              DT::dataTableOutput(ns("var_table")),
              type = 6, color = "#2c7a51"
            )
          ),
          bslib::card(
            bslib::card_header("Variance Partition"),
            full_screen = TRUE,
            shinycssloaders::withSpinner(
              plotly::plotlyOutput(ns("var_pie"), height = "320px"),
              type = 6, color = "#2c7a51"
            )
          )
        )
      ),

      # Tab 3: BLUEs
      bslib::nav_panel(
        title = shiny::tagList(shiny::icon("sort-amount-down"), " BLUEs"),
        style = "min-height: 400px;",
        bslib::layout_column_wrap(
          width = 1 / 2,
          fill  = FALSE,
          bslib::card(
            bslib::card_header(
              shiny::tagList(
                "Genotype BLUEs (Ranked)",
                shiny::downloadButton(ns("dl_blues_csv"), "CSV",
                  class = "btn-outline-primary btn-sm float-end ms-1"),
                shiny::downloadButton(ns("dl_blues_xlsx"), "Excel",
                  class = "btn-outline-primary btn-sm float-end")
              )
            ),
            full_screen = TRUE,
            shinycssloaders::withSpinner(
              DT::dataTableOutput(ns("blues_table")),
              type = 6, color = "#2c7a51"
            )
          ),
          bslib::card(
            bslib::card_header("BLUEs with 95% CI"),
            full_screen = TRUE,
            shinycssloaders::withSpinner(
              plotly::plotlyOutput(ns("blues_plot"), height = "450px"),
              type = 6, color = "#2c7a51"
            )
          )
        )
      ),

      # Tab 4: BLUPs
      bslib::nav_panel(
        title = shiny::tagList(shiny::icon("sort-amount-down-alt"), " BLUPs"),
        style = "min-height: 400px;",
        bslib::layout_column_wrap(
          width = 1 / 2,
          fill  = FALSE,
          bslib::card(
            bslib::card_header(
              shiny::tagList(
                "Genotype BLUPs (Ranked)",
                shiny::downloadButton(ns("dl_blups_csv"), "CSV",
                  class = "btn-outline-primary btn-sm float-end ms-1"),
                shiny::downloadButton(ns("dl_blups_xlsx"), "Excel",
                  class = "btn-outline-primary btn-sm float-end")
              )
            ),
            full_screen = TRUE,
            shinycssloaders::withSpinner(
              DT::dataTableOutput(ns("blups_table")),
              type = 6, color = "#2c7a51"
            )
          ),
          bslib::card(
            bslib::card_header("BLUPs with 95% CI"),
            full_screen = TRUE,
            shinycssloaders::withSpinner(
              plotly::plotlyOutput(ns("blups_plot"), height = "450px"),
              type = 6, color = "#2c7a51"
            )
          )
        )
      )
    )
  )
}


# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
mod_anova_server <- function(id, data_result, results_store) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Data bundle helper ----
    db <- shiny::reactive({
      req(data_result$data_bundle())
      data_result$data_bundle()
    })

    # ---- Update trait selector ----
    shiny::observeEvent(db(), {
      traits <- db()$traits
      req(traits, length(traits) > 0)
      shiny::updateSelectInput(session, "trait",
        choices = traits, selected = traits[1])
    })

    # ---- Run analysis on button click ----
    anova_results <- shiny::eventReactive(input$run_anova, {
      req(db(), input$trait)
      shiny::withProgress(message = "Running ANOVA & Mixed Models...", value = 0, {

        shiny::incProgress(0.1, detail = "Fixed-effects ANOVA")
        anova_res <- safe_analysis(
          run_anova_met(db()$data, db()$gen_col, db()$env_col,
                        db()$rep_col, db()$block_col, input$trait,
                        design_type = db()$design$design %||% "RCBD",
                        augmented_checks = db()$augmented_checks %||% character(0)),
          session
        )

        shiny::incProgress(0.3, detail = "Mixed model (REML)")
        mixed_res <- safe_analysis(
          run_mixed_model(db()$data, db()$gen_col, db()$env_col,
                          db()$rep_col, input$trait),
          session
        )

        shiny::incProgress(0.2, detail = "Computing BLUEs")
        blues_res <- safe_analysis(
          compute_blues(db()$data, db()$gen_col, db()$env_col,
                        db()$rep_col, input$trait),
          session
        )

        shiny::incProgress(0.2, detail = "Computing BLUPs")
        blups_res <- safe_analysis(
          compute_blups(db()$data, db()$gen_col, db()$env_col,
                        db()$rep_col, input$trait),
          session
        )

        shiny::incProgress(0.2, detail = "Done!")

        res_list <- list(
          anova = anova_res,
          mixed = mixed_res,
          blues = blues_res,
          blups = blups_res,
          trait = input$trait
        )
        results_store[[make_results_key("ANOVA", input$trait)]] <- res_list
        res_list
      })
    })

    # ---- Tab 1: ANOVA Table ----
    output$anova_summary_boxes <- shiny::renderUI({
      req(anova_results(), anova_results()$anova)
      res <- anova_results()$anova

      bslib::layout_column_wrap(
        width = 1 / 4,
        fill  = FALSE,
        bslib::value_box(
          title = "Observations", value = res$n_obs,
          showcase = shiny::icon("table"), theme = "primary"
        ),
        bslib::value_box(
          title = "Grand Mean", value = round(res$grand_mean, 2),
          showcase = shiny::icon("bullseye"), theme = "success"
        ),
        bslib::value_box(
          title = "Trait", value = anova_results()$trait,
          showcase = shiny::icon("chart-line"), theme = "info"
        ),
        bslib::value_box(
          title = "G x E",
          value = {
            ge_row <- res$anova_table[grep("G.*x.*E|Genotype.*x.*Env", res$anova_table$Source), ]
            if (nrow(ge_row) > 0) ge_row$Signif[1] else "—"
          },
          showcase = shiny::icon("asterisk"), theme = "warning"
        )
      )
    })

    output$anova_table <- DT::renderDataTable({
      req(anova_results(), anova_results()$anova)
      tbl <- anova_results()$anova$anova_table
      tbl$P_value <- formatC(tbl$P_value, format = "e", digits = 3)

      DT::datatable(
        tbl,
        options  = list(dom = "t", scrollX = TRUE, pageLength = 20),
        class    = "compact stripe hover",
        rownames = FALSE
      ) |>
        DT::formatStyle("Signif",
          backgroundColor = DT::styleEqual(
            c("***", "**", "*", ".", "ns", ""),
            c("#c8e6c9", "#dcedc8", "#f0f4c3", "#fff9c4", "#ffffff", "#ffffff")
          ),
          fontWeight = "bold"
        )
    })

    output$ss_barplot <- plotly::renderPlotly({
      req(anova_results(), anova_results()$anova)
      tbl <- anova_results()$anova$anova_table
      tbl <- tbl[tbl$Source != "Residuals", ]

      plotly::plot_ly(
        data = tbl, x = ~Source, y = ~Pct_SS,
        type = "bar",
        marker = list(color = c("#2c7a51", "#5b9279", "#d4a853", "#8fbc8f")),
        text = ~paste0(Pct_SS, "%"),
        textposition = "auto",
        hovertemplate = "%{x}<br>%{y:.1f}% of total SS<extra></extra>"
      ) |>
        plotly::layout(
          title  = "Partitioning of Sum of Squares (%)",
          xaxis  = list(title = ""),
          yaxis  = list(title = "% of Total SS"),
          margin = list(b = 80)
        )
    })

    # ---- Tab 2: Variance Components ----
    output$heritability_boxes <- shiny::renderUI({
      req(anova_results(), anova_results()$mixed)
      res <- anova_results()$mixed

      bslib::layout_column_wrap(
        width = 1 / 3,
        fill  = FALSE,
        bslib::value_box(
          title = "Broad-sense H\u00B2",
          value = res$H2,
          showcase = shiny::icon("seedling"),
          theme = if (res$H2 >= 0.5) "success" else if (res$H2 >= 0.3) "warning" else "danger"
        ),
        bslib::value_box(
          title = "Selection Accuracy",
          value = res$accuracy,
          showcase = shiny::icon("bullseye"),
          theme = "info"
        ),
        bslib::value_box(
          title = "Environments x Reps",
          value = paste0(res$n_env, " x ", res$n_rep),
          showcase = shiny::icon("globe"),
          theme = "primary"
        )
      )
    })

    output$var_table <- DT::renderDataTable({
      req(anova_results(), anova_results()$mixed)
      DT::datatable(
        anova_results()$mixed$var_table,
        options  = list(dom = "t", scrollX = TRUE),
        class    = "compact stripe hover",
        rownames = FALSE
      )
    })

    output$var_pie <- plotly::renderPlotly({
      req(anova_results(), anova_results()$mixed)
      vt <- anova_results()$mixed$var_table
      vt <- vt[vt$Component != "Total", ]

      plotly::plot_ly(
        data   = vt,
        labels = ~Component,
        values = ~Variance,
        type   = "pie",
        marker = list(colors = c("#2c7a51", "#d4a853", "#8fbc8f", "#c44e52")),
        textinfo = "label+percent",
        hovertemplate = "%{label}<br>Var = %{value:.4f}<br>%{percent}<extra></extra>"
      ) |>
        plotly::layout(
          title       = "Variance Component Partition",
          showlegend  = TRUE,
          margin      = list(t = 40)
        )
    })

    # ---- Tab 3: BLUEs ----
    output$blues_table <- DT::renderDataTable({
      req(anova_results(), anova_results()$blues)
      DT::datatable(
        anova_results()$blues,
        options  = list(pageLength = 25, scrollX = TRUE, dom = "lfrtip"),
        class    = "compact stripe hover",
        rownames = FALSE
      ) |>
        DT::formatRound(c("BLUE", "SE", "CI_lower", "CI_upper"), digits = 3)
    })

    output$blues_plot <- plotly::renderPlotly({
      req(anova_results(), anova_results()$blues)
      df <- anova_results()$blues
      df <- df[order(df$BLUE), ]
      df$Genotype <- factor(df$Genotype, levels = df$Genotype)

      plotly::plot_ly(data = df, x = ~BLUE, y = ~Genotype, type = "scatter",
                      mode = "markers",
                      marker = list(color = "#2c7a51", size = 8),
                      error_x = list(
                        type    = "data",
                        symmetric = FALSE,
                        array    = df$CI_upper - df$BLUE,
                        arrayminus = df$BLUE - df$CI_lower,
                        color = "#999"
                      ),
                      hovertemplate = "%{y}<br>BLUE = %{x:.3f}<extra></extra>") |>
        plotly::layout(
          title  = paste("BLUEs:", anova_results()$trait),
          xaxis  = list(title = anova_results()$trait),
          yaxis  = list(title = "", categoryorder = "trace"),
          margin = list(l = 100)
        )
    })

    # ---- Tab 4: BLUPs ----
    output$blups_table <- DT::renderDataTable({
      req(anova_results(), anova_results()$blups)
      DT::datatable(
        anova_results()$blups,
        options  = list(pageLength = 25, scrollX = TRUE, dom = "lfrtip"),
        class    = "compact stripe hover",
        rownames = FALSE
      ) |>
        DT::formatRound(c("BLUP", "Effect", "SE", "CI_lower", "CI_upper"), digits = 3)
    })

    output$blups_plot <- plotly::renderPlotly({
      req(anova_results(), anova_results()$blups)
      df <- anova_results()$blups
      df <- df[order(df$BLUP), ]
      df$Genotype <- factor(df$Genotype, levels = df$Genotype)

      plotly::plot_ly(data = df, x = ~BLUP, y = ~Genotype, type = "scatter",
                      mode = "markers",
                      marker = list(color = "#d4a853", size = 8),
                      error_x = list(
                        type    = "data",
                        symmetric = FALSE,
                        array    = df$CI_upper - df$BLUP,
                        arrayminus = df$BLUP - df$CI_lower,
                        color = "#999"
                      ),
                      hovertemplate = "%{y}<br>BLUP = %{x:.3f}<extra></extra>") |>
        plotly::layout(
          title  = paste("BLUPs:", anova_results()$trait),
          xaxis  = list(title = anova_results()$trait),
          yaxis  = list(title = "", categoryorder = "trace"),
          margin = list(l = 100)
        )
    })

    # ---- Downloads ----
    output$dl_blues_csv <- shiny::downloadHandler(
      filename = function() paste0("blues_", input$trait, ".csv"),
      content  = function(file) readr::write_csv(anova_results()$blues, file)
    )
    output$dl_blues_xlsx <- shiny::downloadHandler(
      filename = function() paste0("blues_", input$trait, ".xlsx"),
      content  = function(file) openxlsx::write.xlsx(anova_results()$blues, file)
    )
    output$dl_blups_csv <- shiny::downloadHandler(
      filename = function() paste0("blups_", input$trait, ".csv"),
      content  = function(file) readr::write_csv(anova_results()$blups, file)
    )
    output$dl_blups_xlsx <- shiny::downloadHandler(
      filename = function() paste0("blups_", input$trait, ".xlsx"),
      content  = function(file) openxlsx::write.xlsx(anova_results()$blups, file)
    )

    # ---- Return results for downstream modules ----
    return(list(
      anova_results = anova_results
    ))
  })
}
