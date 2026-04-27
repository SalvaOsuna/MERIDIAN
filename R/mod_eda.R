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

    # ---- Tab 2: Distributions ----
    bslib::nav_panel(
      title = shiny::tagList(shiny::icon("chart-area"), " ", "Distributions"),
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 300,
          shiny::selectizeInput(ns("dist_trait"), "Traits",
            choices = NULL,
            multiple = TRUE,
            options = list(plugins = c("remove_button"))
          ),
          shiny::actionButton(ns("dist_toggle_envs"), "Select All / Deselect All",
            icon = shiny::icon("check-square"),
            class = "btn-outline-secondary btn-sm w-100 mb-2"
          ),
          shiny::checkboxGroupInput(ns("dist_envs"), "Environments", choices = NULL),
          shiny::sliderInput(ns("dist_bins"), "Number of bins",
            min = 5, max = 100, value = 30, step = 1
          ),
          colourpicker::colourInput(ns("dist_fill"), "Fill color", value = "#4CAF50"),
          shiny::sliderInput(ns("dist_alpha"), "Transparency (alpha)",
            min = 0.1, max = 1.0, value = 0.8, step = 0.05
          ),
          shiny::checkboxInput(ns("dist_normal"), "Show normal curve overlay", value = FALSE),
          shiny::conditionalPanel(
            condition = "input.dist_normal",
            ns = ns,
            shiny::sliderInput(ns("dist_normal_linewidth"), "Normal curve linewidth",
              min = 0.3, max = 2.0, value = 1.0, step = 0.1
            )
          ),
          shiny::checkboxInput(ns("dist_mean"), "Show mean line", value = TRUE),
          shiny::radioButtons(ns("dist_layout"), "Facet layout",
            choices = c("Wrap", "Grid"), selected = "Wrap", inline = TRUE
          ),
          shiny::conditionalPanel(
            condition = "input.dist_layout == 'Wrap'",
            ns = ns,
            shiny::numericInput(ns("dist_ncol"), "Number of columns in facet wrap",
              value = 2, min = 1, max = 6, step = 1
            )
          ),
          shiny::radioButtons(ns("dist_scales"), "Free or fixed scales",
            choices = c("Fixed", "Free X", "Free Y", "Free XY"),
            selected = "Fixed"
          ),
          shiny::selectInput(ns("dist_strip"), "Strip label style",
            choices = c(
              "Environment name only",
              "Environment name + N obs",
              "Environment name + Mean +/- SD"
            ),
            selected = "Environment name only"
          ),
          shiny::tags$hr(),
          shiny::actionButton(ns("run_distributions"), "Generate",
            icon = shiny::icon("play"),
            class = "btn-primary btn-sm w-100 mb-2"
          ),
          shiny::actionButton(ns("send_distribution_report"), "Save to Figure Composer",
            icon = shiny::icon("paper-plane"),
            class = "btn-success btn-sm w-100"
          )
        ),
        shiny::div(
          class = "meridian-plotly-frame",
          shinycssloaders::withSpinner(
            shiny::plotOutput(ns("distribution_plot"), width = "100%", height = "680px"),
            type = 6, color = "#2c7a51"
          )
        )
      )
    ),

    # ---- Tab 3: G×E Heatmap ----
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

    # ---- Tab 4: Environment Correlations ----
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

    # ---- Tab 5: Outlier Detection ----
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

    # ---- Tab 6: Summary Tables ----
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

      numeric_cols <- names(db()$data)[vapply(db()$data, is.numeric, logical(1))]
      numeric_traits <- intersect(traits, numeric_cols)
      if (length(numeric_traits) == 0) numeric_traits <- numeric_cols
      shiny::updateSelectizeInput(session, "dist_trait",
        choices = numeric_traits,
        selected = if (length(numeric_traits) > 0) numeric_traits[1] else character(0)
      )

      envs <- sort(unique(as.character(db()$data[[db()$env_col]])))
      envs <- envs[!is.na(envs) & nzchar(envs)]
      shiny::updateCheckboxGroupInput(session, "dist_envs",
        choices = envs,
        selected = envs
      )
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

    # ---- Tab 2: Distributions ----
    shiny::observeEvent(input$dist_toggle_envs, {
      req(db())
      envs <- sort(unique(as.character(db()$data[[db()$env_col]])))
      envs <- envs[!is.na(envs) & nzchar(envs)]
      selected <- input$dist_envs %||% character(0)
      next_selection <- if (length(envs) > 0 && setequal(selected, envs)) character(0) else envs
      shiny::updateCheckboxGroupInput(session, "dist_envs", selected = next_selection)
    })

    scale_choice <- function(label) {
      switch(label,
        "Fixed" = "fixed",
        "Free X" = "free_x",
        "Free Y" = "free_y",
        "Free XY" = "free",
        "fixed"
      )
    }

    build_distribution_plot <- function(current_db, traits, envs, bins, fill, alpha,
                                        show_normal, normal_linewidth, show_mean,
                                        layout, ncol, scales_label, strip_label,
                                        notify = FALSE) {
      env_col <- current_db$env_col
      gen_col <- current_db$gen_col
      traits <- unique(traits %||% character(0))
      if (length(traits) == 0) {
        stop("Select at least one trait before generating distributions.", call. = FALSE)
      }
      needed <- c(traits, env_col, gen_col)
      missing_cols <- setdiff(needed, names(current_db$data))
      if (length(missing_cols) > 0) {
        stop("Missing required column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
      }

      non_numeric_traits <- traits[!vapply(current_db$data[traits], is.numeric, logical(1))]
      if (length(non_numeric_traits) > 0) {
        stop(
          "Selected trait(s) must be numeric to draw distribution histograms: ",
          paste(non_numeric_traits, collapse = ", "),
          call. = FALSE
        )
      }

      if (is.null(envs) || length(envs) == 0) {
        stop("Select at least one environment before generating distributions.", call. = FALSE)
      }

      plot_df <- current_db$data[, needed, drop = FALSE]
      plot_df <- tidyr::pivot_longer(
        plot_df,
        cols = dplyr::all_of(traits),
        names_to = "trait_name",
        values_to = "trait_value"
      )
      names(plot_df)[names(plot_df) == env_col] <- "environment"
      names(plot_df)[names(plot_df) == gen_col] <- "genotype"
      plot_df$environment <- as.character(plot_df$environment)
      plot_df <- plot_df[plot_df$environment %in% envs & !is.na(plot_df$trait_value), , drop = FALSE]
      if (nrow(plot_df) == 0) {
        stop("No non-NA observations are available for the selected traits and environments.", call. = FALSE)
      }

      env_counts <- stats::aggregate(
        trait_value ~ trait_name + environment,
        data = plot_df,
        FUN = function(x) sum(!is.na(x))
      )
      names(env_counts)[3] <- "n"
      excluded_counts <- env_counts[env_counts$n < 3, c("trait_name", "environment"), drop = FALSE]

      selected_pairs <- expand.grid(
        trait_name = traits,
        environment = envs,
        stringsAsFactors = FALSE
      )
      observed_pairs <- unique(env_counts[, c("trait_name", "environment"), drop = FALSE])
      missing_pairs <- selected_pairs[!paste(selected_pairs$trait_name, selected_pairs$environment, sep = "\r") %in%
        paste(observed_pairs$trait_name, observed_pairs$environment, sep = "\r"), , drop = FALSE]
      excluded_pairs <- unique(rbind(excluded_counts, missing_pairs))

      if (nrow(excluded_pairs) > 0 && isTRUE(notify)) {
        excluded_labels <- paste(excluded_pairs$trait_name, excluded_pairs$environment, sep = " @ ")
        shiny::showNotification(
          paste(
            "Excluded trait-environment panels with fewer than 3 non-NA observations:",
            paste(excluded_labels, collapse = ", ")
          ),
          type = "warning",
          duration = 8
        )
      }

      if (nrow(excluded_pairs) > 0) {
        excluded_keys <- paste(excluded_pairs$trait_name, excluded_pairs$environment, sep = "\r")
        plot_keys <- paste(plot_df$trait_name, plot_df$environment, sep = "\r")
        plot_df <- plot_df[!plot_keys %in% excluded_keys, , drop = FALSE]
      }
      if (nrow(plot_df) == 0) {
        stop("No selected trait-environment panels have at least 3 non-NA observations.", call. = FALSE)
      }

      env_stats <- stats::aggregate(
        trait_value ~ trait_name + environment,
        data = plot_df,
        FUN = function(x) c(n = length(x), mean = mean(x), sd = stats::sd(x))
      )
      env_stats <- data.frame(
        trait_name = env_stats$trait_name,
        environment = env_stats$environment,
        n = env_stats$trait_value[, "n"],
        env_mean = env_stats$trait_value[, "mean"],
        env_sd = env_stats$trait_value[, "sd"],
        stringsAsFactors = FALSE
      )

      env_stats$env_facet_label <- switch(strip_label,
        "Environment name + N obs" = paste0(env_stats$environment, " (n=", env_stats$n, ")"),
        "Environment name + Mean +/- SD" = paste0(
          env_stats$environment,
          " (mu=", sprintf("%.2f", env_stats$env_mean),
          ", sigma=", sprintf("%.2f", env_stats$env_sd), ")"
        ),
        env_stats$environment
      )
      env_levels <- unique(env_stats$env_facet_label[order(match(env_stats$environment, envs))])
      trait_levels <- traits[traits %in% env_stats$trait_name]
      env_stats$env_facet_label <- factor(env_stats$env_facet_label, levels = env_levels)
      env_stats$trait_facet_label <- factor(env_stats$trait_name, levels = trait_levels)
      env_stats$panel_facet_label <- factor(
        paste(env_stats$trait_name, env_stats$env_facet_label, sep = " | "),
        levels = unique(paste(env_stats$trait_name, env_stats$env_facet_label, sep = " | "))
      )

      plot_df <- merge(plot_df, env_stats[, c(
        "trait_name", "environment", "env_facet_label",
        "trait_facet_label", "panel_facet_label"
      )],
        by = c("trait_name", "environment"), all.x = TRUE, sort = FALSE
      )
      plot_df$env_facet_label <- factor(plot_df$env_facet_label, levels = levels(env_stats$env_facet_label))
      plot_df$trait_facet_label <- factor(plot_df$trait_facet_label, levels = levels(env_stats$trait_facet_label))
      plot_df$panel_facet_label <- factor(plot_df$panel_facet_label, levels = levels(env_stats$panel_facet_label))

      p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = trait_value)) +
        ggplot2::geom_histogram(
          bins = bins,
          fill = fill,
          alpha = alpha,
          color = "white",
          linewidth = 0.2
        ) +
        ggplot2::labs(
          title = if (length(traits) == 1) {
            paste0("Distribution of ", traits, " by Environment")
          } else {
            "Trait Distributions by Environment"
          },
          x = if (length(traits) == 1) traits else "Trait value",
          y = "Count"
        ) +
        ggplot2::theme_bw(base_size = 12)

      if (isTRUE(show_mean)) {
        p <- p +
          ggplot2::geom_vline(
            data = env_stats,
            ggplot2::aes(xintercept = env_mean),
            stat = "identity",
            linetype = "dashed",
            color = "#1565C0",
            linewidth = 0.8
          )
      }

      if (isTRUE(show_normal)) {
        density_data <- lapply(seq_len(nrow(env_stats)), function(i) {
          trait_i <- env_stats$trait_name[i]
          env <- env_stats$environment[i]
          env_values <- plot_df$trait_value[plot_df$trait_name == trait_i & plot_df$environment == env]
          rng <- range(env_values, na.rm = TRUE)
          env_sd <- env_stats$env_sd[i]
          if (!is.finite(env_sd) || env_sd <= 0 || diff(rng) <= 0) return(NULL)
          x_vals <- seq(rng[1], rng[2], length.out = 200)
          bin_width <- diff(rng) / bins
          data.frame(
            trait_name = trait_i,
            environment = env,
            env_facet_label = env_stats$env_facet_label[i],
            trait_facet_label = env_stats$trait_facet_label[i],
            panel_facet_label = env_stats$panel_facet_label[i],
            trait_value = x_vals,
            count = stats::dnorm(x_vals, mean = env_stats$env_mean[i], sd = env_sd) *
              env_stats$n[i] * bin_width,
            stringsAsFactors = FALSE
          )
        })
        density_data <- dplyr::bind_rows(density_data)
        if (nrow(density_data) > 0) {
          density_data$env_facet_label <- factor(density_data$env_facet_label, levels = levels(env_stats$env_facet_label))
          density_data$trait_facet_label <- factor(density_data$trait_facet_label, levels = levels(env_stats$trait_facet_label))
          density_data$panel_facet_label <- factor(density_data$panel_facet_label, levels = levels(env_stats$panel_facet_label))
          p <- p +
            ggplot2::geom_line(
              data = density_data,
              ggplot2::aes(x = trait_value, y = count),
              inherit.aes = FALSE,
              color = "#B71C1C",
              linewidth = normal_linewidth
            )
        }
      }

      facet_scales <- scale_choice(scales_label)
      if (identical(layout, "Grid")) {
        p <- p + ggplot2::facet_grid(
          stats::as.formula(if (length(trait_levels) > 1) "env_facet_label ~ trait_facet_label" else "env_facet_label ~ ."),
          scales = facet_scales
        )
      } else {
        p <- p + ggplot2::facet_wrap(
          stats::as.formula(if (length(trait_levels) > 1) "~ panel_facet_label" else "~ env_facet_label"),
          ncol = max(1, min(6, as.integer(ncol %||% 2))),
          scales = facet_scales
        )
      }

      p
    }

    distribution_plot_obj <- shiny::eventReactive(input$run_distributions, {
      req(db(), input$dist_trait)
      safe_analysis({
        build_distribution_plot(
          current_db = db(),
          traits = input$dist_trait,
          envs = input$dist_envs,
          bins = input$dist_bins,
          fill = input$dist_fill,
          alpha = input$dist_alpha,
          show_normal = input$dist_normal,
          normal_linewidth = input$dist_normal_linewidth,
          show_mean = input$dist_mean,
          layout = input$dist_layout,
          ncol = input$dist_ncol,
          scales_label = input$dist_scales,
          strip_label = input$dist_strip,
          notify = TRUE
        )
      }, session)
    }, ignoreInit = TRUE)

    output$distribution_plot <- shiny::renderPlot({
      req(distribution_plot_obj())
      distribution_plot_obj()
    }, res = 96)

    shiny::observeEvent(input$send_distribution_report, {
      req(report_registry, distribution_plot_obj(), input$dist_trait)
      traits <- shiny::isolate(input$dist_trait)
      settings <- shiny::isolate(list(
        traits = traits,
        envs = input$dist_envs,
        bins = input$dist_bins,
        fill = input$dist_fill,
        alpha = input$dist_alpha,
        show_normal = input$dist_normal,
        normal_linewidth = input$dist_normal_linewidth,
        show_mean = input$dist_mean,
        layout = input$dist_layout,
        ncol = input$dist_ncol,
        scales_label = input$dist_scales,
        strip_label = input$dist_strip
      ))
      sig <- make_dataset_signature(db())
      trait_label <- paste(traits, collapse = ", ")
      register_report_plot(
        registry = report_registry,
        id = make_report_item_id("EDA", "plot", paste(traits, collapse = "_"), "distributions"),
        label = paste("EDA distributions -", trait_label),
        module = "Exploratory Analysis",
        trait = trait_label,
        plot_builder = function() {
          current_db <- data_result$data_bundle()
          if (is.null(current_db)) stop("No dataset is currently loaded.")
          build_distribution_plot(
            current_db = current_db,
            traits = settings$traits,
            envs = settings$envs,
            bins = settings$bins,
            fill = settings$fill,
            alpha = settings$alpha,
            show_normal = settings$show_normal,
            normal_linewidth = settings$normal_linewidth,
            show_mean = settings$show_mean,
            layout = settings$layout,
            ncol = settings$ncol,
            scales_label = settings$scales_label,
            strip_label = settings$strip_label,
            notify = FALSE
          )
        },
        metadata = list(
          plot_family = "distribution_histograms",
          dataset_signature = sig,
          input_snapshot = settings
        )
      )
      shiny::showNotification("EDA distribution plot sent to Figure Composer.", type = "message")
    })

    # ---- Tab 3: G×E Heatmap ----
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
