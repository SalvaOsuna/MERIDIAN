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
          shiny::actionButton(ns("send_boxplot_report"), "Send this plot to Reports",
            icon = shiny::icon("paper-plane"),
            class = "btn-success btn-sm w-100 mb-2"
          ),
          shiny::downloadButton(ns("download_boxplot"), LABELS$download_plot,
                                class = "btn-outline-primary btn-sm w-100")
        ),
        shiny::div(
          class = "meridian-plotly-frame",
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("boxplot"), width = "100%", height = "560px"),
            type = 6, color = "#2c7a51"
          )
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
          shiny::actionButton(ns("send_heatmap_report"), "Send this plot to Reports",
            icon = shiny::icon("paper-plane"),
            class = "btn-success btn-sm w-100 mb-2"
          ),
          shiny::tags$p(
            class = "info-tooltip-text",
            style = "font-size: 0.8rem; color: #777;",
            shiny::icon("info-circle"),
            " Heatmap of genotype \u00D7 environment means. Dendrograms show ",
            "hierarchical clustering of similar genotypes/environments."
          )
        ),
        shiny::div(
          class = "meridian-plotly-frame meridian-plotly-frame-tall",
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("heatmap"), width = "100%", height = "680px"),
            type = 6, color = "#2c7a51"
          )
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
          shiny::actionButton(ns("send_cor_report"), "Send this plot to Reports",
            icon = shiny::icon("paper-plane"),
            class = "btn-success btn-sm w-100 mb-2"
          ),
          shiny::tags$p(
            class = "info-tooltip-text",
            style = "font-size: 0.8rem; color: #777;",
            shiny::icon("info-circle"),
            " Correlation between environments based on genotype means. ",
            "High correlations indicate similar genotype ranking across environments."
          )
        ),
        shiny::div(
          class = "meridian-plotly-frame",
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("cor_plot"), width = "100%", height = "600px"),
            type = 6, color = "#2c7a51"
          )
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
          shiny::actionButton(ns("send_outlier_report"), "Send this plot to Reports",
            icon = shiny::icon("paper-plane"),
            class = "btn-success btn-sm w-100 mt-2"
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
        shiny::div(
          class = "meridian-plotly-frame",
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("outlier_plot"), width = "100%", height = "540px"),
            type = 6, color = "#2c7a51"
          )
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
              shiny::actionButton(ns("send_grand_report"), "Send to Reports",
                icon = shiny::icon("paper-plane"),
                class = "btn-success btn-sm float-end ms-1"),
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
              shiny::actionButton(ns("send_env_report"), "Send to Reports",
                icon = shiny::icon("paper-plane"),
                class = "btn-success btn-sm float-end ms-1"),
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
mod_eda_server <- function(id, data_result, report_registry = NULL) {
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
        plotly::layout(autosize = TRUE, margin = list(l = 70, r = 20, b = 100, t = 60)) |>
        plotly::config(responsive = TRUE)
    })

    output$download_boxplot <- shiny::downloadHandler(
      filename = function() paste0("boxplot_", input$box_trait, ".png"),
      content  = function(file) {
        ggplot2::ggsave(file, plot = boxplot_obj(), width = 10, height = 6, dpi = 150, bg = "white")
      }
    )

    shiny::observeEvent(input$send_boxplot_report, {
      req(report_registry, db(), input$box_trait)
      trait <- shiny::isolate(input$box_trait)
      group_by <- shiny::isolate(input$box_group)
      group_col <- shiny::isolate(box_group_col())
      show_points <- shiny::isolate(input$box_points)
      sig <- make_dataset_signature(db())
      id <- make_report_item_id("EDA", "plot", trait, paste0("boxplot_", group_by))

      register_report_plot(
        registry = report_registry,
        id = id,
        label = paste("EDA boxplot -", trait, "by", group_by),
        module = "Exploratory Analysis",
        trait = trait,
        plot_builder = function() {
          current_db <- data_result$data_bundle()
          if (is.null(current_db)) stop("No dataset is currently loaded.")
          plot_boxplots(
            df = current_db$data,
            trait = trait,
            group_col = group_col,
            color_col = group_col,
            show_points = show_points
          )
        },
        metadata = list(
          plot_family = "boxplot",
          group_by = group_by,
          show_points = show_points,
          dataset_signature = sig,
          input_snapshot = list(trait = trait, group_by = group_by, show_points = show_points)
        )
      )
      shiny::showNotification("EDA boxplot sent to Reports.", type = "message")
    })

    # ---- Tab 2: G×E Heatmap ----
    build_ge_heatmap_gg <- function(current_db, trait, cluster_rows = TRUE, cluster_cols = TRUE) {
      mat <- pivot_ge_means(current_db$data, current_db$gen_col, current_db$env_col, trait)
      if (isTRUE(cluster_rows) && nrow(mat) > 1) {
        mat <- mat[stats::hclust(stats::dist(mat))$order, , drop = FALSE]
      }
      if (isTRUE(cluster_cols) && ncol(mat) > 1) {
        mat <- mat[, stats::hclust(stats::dist(t(mat)))$order, drop = FALSE]
      }
      plot_df <- as.data.frame(as.table(mat), stringsAsFactors = FALSE)
      names(plot_df) <- c("Genotype", "Environment", "Value")
      ggplot2::ggplot(plot_df, ggplot2::aes(x = Environment, y = Genotype, fill = Value)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.2) +
        ggplot2::scale_fill_viridis_c(option = "D", na.value = "grey90") +
        ggplot2::labs(title = paste0("GxE Means: ", trait), x = "Environment", y = "Genotype", fill = trait) +
        ggplot2::theme_bw(base_size = 12) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    }

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
        ) |>
          plotly::layout(autosize = TRUE, margin = list(l = 120, r = 30, b = 100, t = 80)) |>
          plotly::config(responsive = TRUE)
      }, error = function(e) {
        shiny::showNotification(
          paste("Heatmap error:", e$message),
          type = "error", duration = 8
        )
        plotly::plotly_empty() |>
          plotly::layout(title = "Error generating heatmap")
      })
    })

    shiny::observeEvent(input$send_heatmap_report, {
      req(report_registry, db(), input$heat_trait)
      trait <- shiny::isolate(input$heat_trait)
      cluster_rows <- shiny::isolate(input$heat_cluster_rows)
      cluster_cols <- shiny::isolate(input$heat_cluster_cols)
      sig <- make_dataset_signature(db())
      register_report_plot(
        registry = report_registry,
        id = make_report_item_id("EDA", "plot", trait, "gxe_heatmap"),
        label = paste("EDA GxE heatmap -", trait),
        module = "Exploratory Analysis",
        trait = trait,
        plot_builder = function() {
          current_db <- data_result$data_bundle()
          if (is.null(current_db)) stop("No dataset is currently loaded.")
          build_ge_heatmap_gg(current_db, trait, cluster_rows, cluster_cols)
        },
        metadata = list(plot_family = "gxe_heatmap", dataset_signature = sig)
      )
      shiny::showNotification("EDA GxE heatmap sent to Reports.", type = "message")
    })

    # ---- Tab 3: Correlations ----
    build_env_correlation_gg <- function(current_db, trait, method = "pearson") {
      mat <- pivot_ge_means(current_db$data, current_db$gen_col, current_db$env_col, trait)
      cor_matrix <- round(stats::cor(mat, use = "pairwise.complete.obs", method = method), 2)
      plot_df <- as.data.frame(as.table(cor_matrix), stringsAsFactors = FALSE)
      names(plot_df) <- c("Env1", "Env2", "Correlation")
      ggplot2::ggplot(plot_df, ggplot2::aes(x = Env2, y = Env1, fill = Correlation)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.2) +
        ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", Correlation)), size = 3) +
        ggplot2::scale_fill_gradient2(low = "#c44e52", mid = "#f8f9fa", high = "#2c7a51",
          midpoint = 0, limits = c(-1, 1), name = "r") +
        ggplot2::labs(title = paste("Environment Correlations:", trait, "(", method, ")"), x = NULL, y = NULL) +
        ggplot2::theme_bw(base_size = 12) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    }

    output$cor_plot <- plotly::renderPlotly({
      req(db(), input$cor_trait)
      safe_analysis({
        plot_env_correlation(
          df      = db()$data,
          gen_col = db()$gen_col,
          env_col = db()$env_col,
          trait   = input$cor_trait,
          method  = input$cor_method
        ) |>
          plotly::config(responsive = TRUE)
      }, session)
    })

    shiny::observeEvent(input$send_cor_report, {
      req(report_registry, db(), input$cor_trait)
      trait <- shiny::isolate(input$cor_trait)
      method <- shiny::isolate(input$cor_method)
      sig <- make_dataset_signature(db())
      register_report_plot(
        registry = report_registry,
        id = make_report_item_id("EDA", "plot", trait, paste0("env_correlation_", method)),
        label = paste("EDA environment correlation -", trait),
        module = "Exploratory Analysis",
        trait = trait,
        plot_builder = function() {
          current_db <- data_result$data_bundle()
          if (is.null(current_db)) stop("No dataset is currently loaded.")
          build_env_correlation_gg(current_db, trait, method)
        },
        metadata = list(plot_family = "environment_correlation", method = method, dataset_signature = sig)
      )
      shiny::showNotification("EDA environment correlation sent to Reports.", type = "message")
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
        plotly::layout(autosize = TRUE, margin = list(l = 70, r = 20, b = 100, t = 60)) |>
        plotly::config(responsive = TRUE)
    })

    shiny::observeEvent(input$send_outlier_report, {
      req(report_registry, outlier_data(), input$out_trait)
      trait <- shiny::isolate(input$out_trait)
      method <- shiny::isolate(input$out_method)
      sig <- make_dataset_signature(db())
      register_report_plot(
        registry = report_registry,
        id = make_report_item_id("EDA", "plot", trait, paste0("outliers_", method)),
        label = paste("EDA outlier scatter -", trait),
        module = "Exploratory Analysis",
        trait = trait,
        plot_builder = function() {
          current_db <- data_result$data_bundle()
          if (is.null(current_db)) stop("No dataset is currently loaded.")
          od <- detect_outliers(current_db$data, trait, current_db$env_col, method)
          plot_outlier_scatter(od, trait, current_db$env_col, current_db$gen_col)
        },
        metadata = list(plot_family = "outlier_scatter", method = method, dataset_signature = sig)
      )
      shiny::showNotification("EDA outlier scatter sent to Reports.", type = "message")
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

    shiny::observeEvent(input$send_grand_report, {
      req(report_registry, grand_summary())
      sig <- make_dataset_signature(db())
      register_report_table(
        registry = report_registry,
        id = make_report_item_id("EDA", "table", "all_traits", "grand_summary"),
        label = "EDA grand summary table",
        module = "Exploratory Analysis",
        trait = "All traits",
        table_builder = function() {
          current_db <- data_result$data_bundle()
          if (is.null(current_db)) stop("No dataset is currently loaded.")
          compute_grand_summary(current_db$data, current_db$traits)
        },
        metadata = list(table_family = "grand_summary", dataset_signature = sig)
      )
      shiny::showNotification("Grand summary table sent to Reports.", type = "message")
    })

    shiny::observeEvent(input$send_env_report, {
      req(report_registry, env_summary())
      sig <- make_dataset_signature(db())
      register_report_table(
        registry = report_registry,
        id = make_report_item_id("EDA", "table", "all_traits", "environment_summary"),
        label = "EDA summary by environment table",
        module = "Exploratory Analysis",
        trait = "All traits",
        table_builder = function() {
          current_db <- data_result$data_bundle()
          if (is.null(current_db)) stop("No dataset is currently loaded.")
          descriptive_summary(current_db$data, current_db$env_col, current_db$traits)
        },
        metadata = list(table_family = "environment_summary", dataset_signature = sig)
      )
      shiny::showNotification("Environment summary table sent to Reports.", type = "message")
    })
  })
}
