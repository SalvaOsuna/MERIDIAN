# =============================================================================
# MERIDIAN — Module 2: Exploratory Data Analysis
# Boxplots, heatmaps, correlations, outlier detection, summary tables
# =============================================================================


# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
mod_eda_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::navset_card_tab(
    id    = ns("eda_tabs"),
    title = shiny::tagList(shiny::icon("chart-bar"), " ", LABELS$m2_title),
    full_screen = TRUE,

    # ---- Tab 1: Boxplots ----
    bslib::nav_panel(
      title = shiny::tagList(shiny::icon("box"), " ", LABELS$m2_boxplots),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 280,
          shiny::selectInput(ns("box_trait"), LABELS$m2_select_trait, choices = NULL),
          shiny::selectInput(ns("box_group"), LABELS$m2_group_by,
            choices = c("Environment", "Genotype"), selected = "Environment"
          ),
          shiny::checkboxInput(ns("box_points"), LABELS$m2_show_points, value = TRUE),
          shiny::tags$hr(),
          shiny::downloadButton(ns("download_boxplot"), LABELS$download_plot,
                                class = "btn-outline-primary btn-sm w-100")
        ),
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("boxplot"), height = "500px"),
          type = 6, color = "#2c7a51"
        )
      )
    ),

    # ---- Tab 2: G×E Heatmap ----
    bslib::nav_panel(
      title = shiny::tagList(shiny::icon("th"), " ", LABELS$m2_heatmap),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 280,
          shiny::selectInput(ns("heat_trait"), LABELS$m2_select_trait, choices = NULL),
          shiny::checkboxInput(ns("heat_cluster_rows"), LABELS$m2_cluster_rows, value = TRUE),
          shiny::checkboxInput(ns("heat_cluster_cols"), LABELS$m2_cluster_cols, value = TRUE),
          shiny::selectInput(ns("heat_palette"), LABELS$m2_color_palette,
            choices = c("RdYlGn", "Viridis" = "viridis", "Inferno" = "inferno",
                        "Plasma" = "plasma", "Blues" = "Blues", "YlOrRd"),
            selected = "RdYlGn"
          ),
          shiny::tags$hr(),
          shiny::tags$p(
            class = "info-tooltip-text",
            style = "font-size: 0.8rem; color: #777;",
            shiny::icon("info-circle"),
            " Heatmap of genotype \u00D7 environment means. Dendrograms show ",
            "hierarchical clustering of similar genotypes/environments."
          )
        ),
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("heatmap"), height = "600px"),
          type = 6, color = "#2c7a51"
        )
      )
    ),

    # ---- Tab 3: Environment Correlations ----
    bslib::nav_panel(
      title = shiny::tagList(shiny::icon("project-diagram"), " ", LABELS$m2_correlation),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 280,
          shiny::selectInput(ns("cor_trait"), LABELS$m2_select_trait, choices = NULL),
          shiny::selectInput(ns("cor_method"), LABELS$m2_corr_method,
            choices  = c("Pearson" = "pearson", "Spearman" = "spearman",
                         "Kendall" = "kendall"),
            selected = "pearson"
          ),
          shiny::tags$hr(),
          shiny::tags$p(
            class = "info-tooltip-text",
            style = "font-size: 0.8rem; color: #777;",
            shiny::icon("info-circle"),
            " Correlation between environments based on genotype means. ",
            "High correlations indicate similar genotype ranking across environments."
          )
        ),
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("cor_plot"), height = "550px"),
          type = 6, color = "#2c7a51"
        )
      )
    ),

    # ---- Tab 4: Outlier Detection ----
    bslib::nav_panel(
      title = shiny::tagList(shiny::icon("search"), " ", LABELS$m2_outliers),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 280,
          shiny::selectInput(ns("out_trait"), LABELS$m2_select_trait, choices = NULL),
          shiny::selectInput(ns("out_method"), LABELS$m2_outlier_method,
            choices  = c("IQR (1.5\u00D7)" = "iqr", "Z-score (>3)" = "zscore"),
            selected = "iqr"
          ),
          shiny::tags$hr(),
          shiny::actionButton(ns("remove_outliers"), LABELS$m2_remove_outliers,
            icon  = shiny::icon("filter"),
            class = "btn-warning btn-sm w-100"
          ),
          shiny::tags$hr(),
          shiny::tags$p(
            class = "info-tooltip-text",
            style = "font-size: 0.8rem; color: #777;",
            shiny::icon("info-circle"),
            " Outliers are detected within each environment. ",
            "IQR method: values beyond Q1-1.5\u00D7IQR or Q3+1.5\u00D7IQR. ",
            "Z-score method: |z| > 3."
          )
        ),
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("outlier_plot"), height = "450px"),
          type = 6, color = "#2c7a51"
        ),
        bslib::card(
          bslib::card_header("Detected Outliers"),
          DT::dataTableOutput(ns("outlier_table"))
        )
      )
    ),

    # ---- Tab 5: Summary Tables ----
    bslib::nav_panel(
      title = shiny::tagList(shiny::icon("table"), " ", LABELS$m2_summary),
      bslib::layout_column_wrap(
        width = 1,
        bslib::card(
          bslib::card_header(
            shiny::tagList(
              "Grand Summary",
              shiny::downloadButton(ns("dl_grand_csv"), "CSV",
                class = "btn-outline-primary btn-sm float-end ms-1"),
              shiny::downloadButton(ns("dl_grand_xlsx"), "Excel",
                class = "btn-outline-primary btn-sm float-end")
            )
          ),
          DT::dataTableOutput(ns("grand_summary_table"))
        ),
        bslib::card(
          bslib::card_header(
            shiny::tagList(
              "Summary by Environment",
              shiny::downloadButton(ns("dl_env_csv"), "CSV",
                class = "btn-outline-primary btn-sm float-end ms-1"),
              shiny::downloadButton(ns("dl_env_xlsx"), "Excel",
                class = "btn-outline-primary btn-sm float-end")
            )
          ),
          DT::dataTableOutput(ns("env_summary_table"))
        )
      )
    )
  )
}


# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
mod_eda_server <- function(id, data_result, results_store) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Helper: Get data bundle ----
    db <- shiny::reactive({
      req(data_result$data_bundle())
      data_result$data_bundle()
    })

    # ---- Update trait selectors when data loads ----
    shiny::observeEvent(db(), {
      traits <- db()$traits
      req(traits, length(traits) > 0)

      for (sel_id in c("box_trait", "heat_trait", "cor_trait", "out_trait")) {
        shiny::updateSelectInput(session, sel_id,
          choices = traits, selected = traits[1])
      }
    })

    shiny::observeEvent(list(db(), input$box_trait), {
      shiny::req(db(), input$box_trait)
      key <- make_results_key("EDA_Boxplot", input$box_trait)
      results_store[[key]] <- list(data = db()$data, trait = input$box_trait, env_col = db()$env_col, gen_col = db()$gen_col)
    })

    shiny::observeEvent(list(db(), input$heat_trait), {
      shiny::req(db(), input$heat_trait)
      key <- make_results_key("EDA_Heatmap", input$heat_trait)
      results_store[[key]] <- list(data = db()$data, trait = input$heat_trait, env_col = db()$env_col, gen_col = db()$gen_col)
    })

    shiny::observeEvent(list(db(), input$cor_trait), {
      shiny::req(db(), input$cor_trait)
      key <- make_results_key("EDA_Correlation", input$cor_trait)
      results_store[[key]] <- list(data = db()$data, trait = input$cor_trait, env_col = db()$env_col, gen_col = db()$gen_col)
    })

    # ---- Reactive: Resolve grouping column name ----
    box_group_col <- shiny::reactive({
      req(db())
      if (input$box_group == "Environment") db()$env_col else db()$gen_col
    })

    # ---- Tab 1: Boxplots ----
    boxplot_obj <- shiny::reactive({
      req(db(), input$box_trait)
      plot_boxplots(
        df          = db()$data,
        trait       = input$box_trait,
        group_col   = box_group_col(),
        color_col   = box_group_col(),
        show_points = input$box_points
      )
    })

    output$boxplot <- plotly::renderPlotly({
      req(boxplot_obj())
      plotly::ggplotly(boxplot_obj(), tooltip = c("x", "y")) |>
        plotly::layout(margin = list(b = 80))
    })

    output$download_boxplot <- shiny::downloadHandler(
      filename = function() paste0("boxplot_", input$box_trait, ".png"),
      content  = function(file) {
        ggplot2::ggsave(file, plot = boxplot_obj(), width = 10, height = 6, dpi = 150)
      }
    )

    # ---- Tab 2: G×E Heatmap ----
    output$heatmap <- plotly::renderPlotly({
      req(db(), input$heat_trait)
      tryCatch({
        suppressWarnings(
          plot_ge_heatmap(
            df           = db()$data,
            gen_col      = db()$gen_col,
            env_col      = db()$env_col,
            trait        = input$heat_trait,
            cluster_rows = input$heat_cluster_rows,
            cluster_cols = input$heat_cluster_cols,
            palette      = input$heat_palette
          )
        )
      }, error = function(e) {
        shiny::showNotification(
          paste("Heatmap error:", e$message),
          type = "error", duration = 8
        )
        plotly::plotly_empty() |>
          plotly::layout(title = "Error generating heatmap")
      })
    })

    # ---- Tab 3: Correlations ----
    output$cor_plot <- plotly::renderPlotly({
      req(db(), input$cor_trait)
      safe_analysis({
        plot_env_correlation(
          df      = db()$data,
          gen_col = db()$gen_col,
          env_col = db()$env_col,
          trait   = input$cor_trait,
          method  = input$cor_method
        )
      }, session)
    })

    # ---- Tab 4: Outlier Detection ----
    outlier_data <- shiny::reactive({
      req(db(), input$out_trait)
      detect_outliers(
        df        = db()$data,
        trait     = input$out_trait,
        group_col = db()$env_col,
        method    = input$out_method
      )
    })

    output$outlier_plot <- plotly::renderPlotly({
      req(outlier_data())
      p <- plot_outlier_scatter(
        df        = outlier_data(),
        trait     = input$out_trait,
        group_col = db()$env_col,
        gen_col   = db()$gen_col
      )
      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(margin = list(b = 80))
    })

    output$outlier_table <- DT::renderDataTable({
      req(outlier_data())
      outliers <- outlier_data() |> dplyr::filter(is_outlier == TRUE)
      if (nrow(outliers) == 0) {
        return(DT::datatable(
          data.frame(Message = "No outliers detected with current settings."),
          options = list(dom = "t"), rownames = FALSE
        ))
      }
      # Select relevant columns
      cols_to_show <- c(db()$gen_col, db()$env_col, db()$rep_col, input$out_trait)
      DT::datatable(
        outliers[, cols_to_show, drop = FALSE],
        options  = list(pageLength = 10, dom = "tip", scrollX = TRUE),
        class    = "compact stripe",
        rownames = FALSE
      )
    })

    # ---- Tab 5: Summary Tables ----
    grand_summary <- shiny::reactive({
      req(db(), db()$traits)
      compute_grand_summary(db()$data, db()$traits)
    })

    env_summary <- shiny::reactive({
      req(db(), db()$env_col, db()$traits)
      descriptive_summary(db()$data, db()$env_col, db()$traits)
    })

    output$grand_summary_table <- DT::renderDataTable({
      req(grand_summary())
      DT::datatable(
        grand_summary(),
        options  = list(dom = "t", scrollX = TRUE),
        class    = "compact stripe hover",
        rownames = FALSE
      ) |>
        DT::formatRound(c("Mean", "SD", "Min", "Max", "CV_pct"), digits = 2)
    })

    output$env_summary_table <- DT::renderDataTable({
      req(env_summary())
      DT::datatable(
        env_summary(),
        options  = list(pageLength = 25, scrollX = TRUE, dom = "lfrtip"),
        class    = "compact stripe hover",
        rownames = FALSE
      ) |>
        DT::formatRound(c("Mean", "SD", "Min", "Max", "CV_pct"), digits = 2)
    })

    # ---- Downloads: Summary tables ----
    output$dl_grand_csv <- shiny::downloadHandler(
      filename = function() "grand_summary.csv",
      content  = function(file) readr::write_csv(grand_summary(), file)
    )

    output$dl_grand_xlsx <- shiny::downloadHandler(
      filename = function() "grand_summary.xlsx",
      content  = function(file) openxlsx::write.xlsx(grand_summary(), file)
    )

    output$dl_env_csv <- shiny::downloadHandler(
      filename = function() "summary_by_environment.csv",
      content  = function(file) readr::write_csv(env_summary(), file)
    )

    output$dl_env_xlsx <- shiny::downloadHandler(
      filename = function() "summary_by_environment.xlsx",
      content  = function(file) openxlsx::write.xlsx(env_summary(), file)
    )
  })
}
