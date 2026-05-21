# =============================================================================
# MERIDIAN — Module 5: Adaptation & Enviromics
# =============================================================================

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
mod_adaptation_ui <- function(id) {
  ns <- shiny::NS(id)
  
  bslib::layout_sidebar(
    fillable = FALSE,
    sidebar = bslib::sidebar(
      width = 300,
      title = shiny::tagList(shiny::icon("globe"), " Adaptation & Enviromics"),
      
      shiny::selectInput(ns("trait"), LABELS$m3_select_trait, choices = NULL),
      
      shiny::actionButton(
        ns("run_adaptation"),
        "Run Analysis",
        icon  = shiny::icon("play"),
        class = "btn-success w-100",
        style = "margin-top: 10px;"
      ),
      
      shiny::tags$hr(),
      shiny::tags$h6("About this module:"),
      shiny::tags$p(
        style = "font-size: 0.85rem; color: #666;",
        "Explore how genotypes adapt to different environments and uncover the physical covariates driving adaptation."
      )
    ),
    
    # ---- Main Panel ----
    bslib::navset_card_pill(
      id = ns("adaptation_tabs"),
      
      # 1. Phenotypic Adaptation
      bslib::nav_panel(
        title = shiny::tagList(shiny::icon("leaf"), " Phenotypic Adaptation"),
        bslib::layout_column_wrap(
          width = 1,
          fill = FALSE,
          bslib::card(
            bslib::card_header("Mega-Environments (Which-won-where clustering)"),
            shiny::div(class = "px-3 pt-2",
              shiny::actionButton(ns("send_mega_env_report"), "Send this plot to Reports",
                icon = shiny::icon("paper-plane"), class = "btn-success btn-sm w-100")
            ),
            full_screen = TRUE,
            style = "min-height: 400px;",
            shinycssloaders::withSpinner(
              plotly::plotlyOutput(ns("plot_mega_env")),
              type = 6, color = "#2c7a51"
            )
          ),
          bslib::card(
            bslib::card_header("Reaction Norms (Finlay-Wilkinson)"),
            shiny::div(class = "px-3 pt-2",
              shiny::actionButton(ns("send_fw_report"), "Send this plot to Reports",
                icon = shiny::icon("paper-plane"), class = "btn-success btn-sm w-100")
            ),
            full_screen = TRUE,
            style = "min-height: 400px;",
            shinycssloaders::withSpinner(
              plotly::plotlyOutput(ns("plot_fw")),
              type = 6, color = "#2c7a51"
            )
          )
        )
      ),
      
      # 2. Enviromics (Requires Env Data)
      bslib::nav_panel(
        title = shiny::tagList(shiny::icon("cloud-sun"), " Enviromics"),
        shiny::uiOutput(ns("enviromics_ui"))
      )
    )
  )
}

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
mod_adaptation_server <- function(id, data_result, report_registry = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    db <- shiny::reactive({
      req(data_result$data_bundle())
      data_result$data_bundle()
    })
    
    # Update trait dropdown
    shiny::observeEvent(db(), {
      traits <- db()$traits
      req(traits)
      shiny::updateSelectInput(session, "trait", choices = traits, selected = traits[1])
    })
    
    # Run analysis
    adapt_results <- shiny::eventReactive(input$run_adaptation, {
      req(db(), input$trait)
      
      shiny::withProgress(message = "Running Adaptation Analysis...", value = 0, {
        
        # 1. Mega-environments
        shiny::incProgress(0.3, detail = "Mega-Environments")
        mega_res <- safe_analysis(
          run_mega_envs(db()$data, db()$gen_col, db()$env_col, input$trait),
          session
        )
        
        # 2. Finlay-Wilkinson
        shiny::incProgress(0.3, detail = "Finlay-Wilkinson")
        fw_res <- safe_analysis(
          run_finlay_wilkinson(db()$data, db()$gen_col, db()$env_col, input$trait),
          session
        )
        
        # 3. Enviromics (if available)
        shiny::incProgress(0.3, detail = "Enviromics")
        env_res <- NULL
        env_cor <- NULL
        
        if (!is.null(db()$env_data)) {
          # Dynamically detect environment column in env_data
          env_names <- names(db()$env_data)
          env_data_env_col <- env_names[grep("^(env|site|loc)", env_names, ignore.case = TRUE)][1]
          if (is.na(env_data_env_col)) env_data_env_col <- db()$env_col
          
          env_res <- safe_analysis(
            compute_env_pca(db()$env_data, env_data_env_col),
            session
          )
          
          if (!is.null(fw_res$env_means)) {
             # Standardize environment column in env_means for merging
             env_means_std <- fw_res$env_means
             names(env_means_std)[1] <- env_data_env_col
             
             env_cor <- safe_analysis(
               compute_covariate_correlations(db()$env_data, env_means_std, env_data_env_col),
               session
             )
          }
        }
        
        list(
          mega_env = mega_res,
          fw = fw_res,
          env_pca = env_res,
          env_cor = env_cor,
          has_env = !is.null(db()$env_data)
        )
      })
    })
    
    # ---- Phenotypic Adaptation Plots ----
    
    output$plot_mega_env <- plotly::renderPlotly({
      req(adapt_results()$mega_env)
      res <- adapt_results()$mega_env

      df_stat <- res$env_strat
      if (is.null(df_stat)) return(plotly::plotly_empty())

      plotly::plot_ly(
        data = df_stat,
        x = ~ENV,
        y = ~MEAN,
        color = ~MEGA_ENV,
        type = "bar",
        text = ~paste("Winner:", Winning_Genotype, "<br>Mean:", round(MEAN, 3)),
        hovertemplate = "%{text}<extra></extra>"
      ) |>
        meridian_plotly_layout(
          title = paste("Environments Grouped by Mega-Environment (", input$trait, ")", sep = ""),
          xaxis = list(title = "Environment"),
          yaxis = list(title = paste("Max Mean", input$trait)),
          margin = list(l = 70, r = 20, b = 90, t = 55),
          barmode = "group"
        )
    })
    
    output$plot_fw <- plotly::renderPlotly({
      req(adapt_results()$fw)
      fw <- adapt_results()$fw
      req(!is.null(fw$fw_data), !is.null(fw$gen_slopes))

      gen_col <- db()$gen_col

      points_df <- fw$fw_data |>
        dplyr::transmute(
          Genotype = as.character(.data[[gen_col]]),
          EI = as.numeric(EI),
          mean_val = as.numeric(mean_val)
        )

      lines_df <- fw$gen_slopes |>
        dplyr::transmute(
          Genotype = as.character(.data[[gen_col]]),
          slope = as.numeric(slope),
          intercept = as.numeric(intercept)
        )

      ei_rng <- range(points_df$EI, na.rm = TRUE)
      lines_df <- lines_df |>
        dplyr::mutate(
          x0 = ei_rng[1],
          x1 = ei_rng[2],
          y0 = intercept + slope * x0,
          y1 = intercept + slope * x1
        )

      show_legend <- dplyr::n_distinct(points_df$Genotype) <= 25

      p <- plotly::plot_ly() |>
        plotly::add_trace(
          data = points_df,
          x = ~EI, y = ~mean_val,
          split = ~Genotype,
          type = "scattergl", mode = "markers",
          marker = list(size = 6, opacity = 0.6),
          hovertemplate = "Genotype: %{customdata}<br>EI: %{x:.3f}<br>Mean: %{y:.3f}<extra></extra>",
          customdata = ~Genotype,
          showlegend = show_legend
        ) |>
        plotly::add_segments(
          data = lines_df,
          x = ~x0, y = ~y0, xend = ~x1, yend = ~y1,
          split = ~Genotype,
          inherit = FALSE,
          line = list(width = 1.2),
          opacity = 0.7,
          hoverinfo = "skip",
          showlegend = FALSE
        ) |>
        meridian_plotly_layout(
          title = paste("Reaction Norms (Finlay-Wilkinson) -", input$trait),
          xaxis = list(title = "Environmental Index"),
          yaxis = list(title = paste("Genotype Mean", input$trait))
        )

      p
    })

    build_mega_env_gg <- function(res, trait) {
      df_stat <- res$mega_env$env_strat
      if (is.null(df_stat)) stop("Mega-environment results are unavailable.")
      ggplot2::ggplot(df_stat, ggplot2::aes(x = ENV, y = MEAN, fill = MEGA_ENV)) +
        ggplot2::geom_col(alpha = 0.85) +
        scale_fill_meridian_discrete() +
        ggplot2::labs(
          title = paste0("Environments Grouped by Mega-Environment (", trait, ")"),
          x = "Environment",
          y = paste("Max Mean", trait),
          fill = "Mega-env"
        ) +
        theme_meridian_nature() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    }

    build_fw_gg <- function(res, current_db, trait) {
      fw <- res$fw
      req(!is.null(fw$fw_data), !is.null(fw$gen_slopes))
      gen_col <- current_db$gen_col
      ggplot2::ggplot(fw$fw_data, ggplot2::aes(x = EI, y = mean_val, color = .data[[gen_col]])) +
        ggplot2::geom_point(alpha = 0.6, size = 1.8) +
        ggplot2::geom_abline(
          data = fw$gen_slopes,
          ggplot2::aes(intercept = intercept, slope = slope, color = .data[[gen_col]]),
          linewidth = 0.7,
          alpha = 0.75,
          show.legend = dplyr::n_distinct(fw$fw_data[[gen_col]]) <= 25
        ) +
        scale_color_meridian_discrete() +
        ggplot2::labs(
          title = paste("Reaction Norms (Finlay-Wilkinson) -", trait),
          x = "Environmental Index",
          y = paste("Genotype Mean", trait),
          color = "Genotype"
        ) +
        theme_meridian_nature()
    }

    register_adaptation_plot <- function(name, label, builder, metadata = list()) {
      req(report_registry, adapt_results(), input$trait)
      res_snapshot <- shiny::isolate(adapt_results())
      db_snapshot <- shiny::isolate(data_result$data_bundle())
      trait <- shiny::isolate(input$trait)
      sig <- make_dataset_signature(db())
      register_report_plot(
        registry = report_registry,
        id = make_report_item_id("Adaptation", "plot", trait, name),
        label = paste(label, "-", trait),
        module = "Adaptation & Enviromics",
        trait = trait,
        plot_builder = function() {
          if (is.null(res_snapshot) || is.null(db_snapshot)) stop("Adaptation results are unavailable.")
          builder(res_snapshot, db_snapshot, trait)
        },
        metadata = c(metadata, list(dataset_signature = sig))
      )
      shiny::showNotification(paste(label, "sent to Reports."), type = "message")
    }

    shiny::observeEvent(input$send_mega_env_report, {
      register_adaptation_plot("mega_environments", "Mega-environment plot",
        function(res, current_db, trait) build_mega_env_gg(res, trait),
        list(plot_family = "mega_environments")
      )
    })

    shiny::observeEvent(input$send_fw_report, {
      register_adaptation_plot("reaction_norms", "Reaction norms plot",
        function(res, current_db, trait) build_fw_gg(res, current_db, trait),
        list(plot_family = "reaction_norms")
      )
    })
    
    # ---- Enviromics UI & Plots ----
    
    output$enviromics_ui <- shiny::renderUI({
      res <- adapt_results()
      if (is.null(res)) return(shiny::p("Run analysis first.", style = "padding: 20px; font-weight: bold;"))
      
      if (!res$has_env) {
        return(
          shiny::div(
            class = "alert alert-warning m-3",
            shiny::icon("exclamation-triangle"), 
            " No environmental covariates loaded. Please upload environmental data in the Data Upload tab to use these features."
          )
        )
      }
      
      bslib::navset_card_tab(
        title = "Enviromics Analytics",
        full_screen = TRUE,
        bslib::nav_panel(
          title = shiny::tagList(shiny::icon("chart-bar"), "Environmental Characterization"),
          bslib::layout_column_wrap(
            width = 1/2,
            fill = FALSE,
            bslib::card(
              bslib::card_header("Environmental Typology (PCA)"),
              full_screen = TRUE,
              style = "min-height: 400px;",
              plotly::plotlyOutput(ns("plot_env_pca"))
            ),
            bslib::card(
              bslib::card_header("Covariate-Phenotype Correlations"),
              full_screen = TRUE,
              style = "min-height: 400px;",
              plotly::plotlyOutput(ns("plot_env_cor"))
            ),
            bslib::card(
              bslib::card_header("Geo-Spatial Map"),
              full_screen = TRUE,
              style = "min-height: 400px;",
              plotly::plotlyOutput(ns("plot_env_map"))
            )
          )
        ),
        bslib::nav_panel(
          title = shiny::tagList(shiny::icon("temperature-high"), "Climatic Drivers of Traits"),
          shiny::div(
            class = "container-fluid mt-3",
            bslib::layout_column_wrap(
              width = 1,
              fill = FALSE,
              bslib::card(
                bslib::card_body(
                  shiny::selectInput(
                    ns("climatic_covariate"),
                    "Select Climatic Covariate/Index",
                    choices = NULL
                  )
                )
              )
            ),
            bslib::layout_column_wrap(
              width = 1/2,
              fill = FALSE,
              bslib::card(
                bslib::card_header("Trait Response to Climatic Covariate"),
                shiny::div(class = "px-3 pt-2",
                  shiny::actionButton(ns("send_climatic_driver_report"), "Send this plot to Reports",
                    icon = shiny::icon("paper-plane"), class = "btn-success btn-sm w-100")
                ),
                full_screen = TRUE,
                style = "min-height: 400px;",
                plotly::plotlyOutput(ns("plot_climatic_driver"))
              ),
              bslib::card(
                bslib::card_header("Climatic Covariate Importance (Pearson Correlation)"),
                shiny::div(class = "px-3 pt-2",
                  shiny::actionButton(ns("send_climatic_importance_report"), "Send this plot to Reports",
                    icon = shiny::icon("paper-plane"), class = "btn-success btn-sm w-100")
                ),
                full_screen = TRUE,
                style = "min-height: 400px;",
                plotly::plotlyOutput(ns("plot_climatic_importance"))
              )
            )
          )
        )
      )
    })
    
    output$plot_env_pca <- plotly::renderPlotly({
      req(adapt_results()$env_pca)
      pca <- adapt_results()$env_pca$pca_res
      env_labels <- adapt_results()$env_pca$env_labels
      
      # Extract components
      df_pca <- as.data.frame(pca$x)
      df_pca$Environment <- env_labels
      
      pc1_pct <- round(summary(pca)$importance[2,1]*100, 1)
      pc2_pct <- round(summary(pca)$importance[2,2]*100, 1)
      
      p <- plotly::plot_ly(
        data = df_pca,
        x = ~PC1,
        y = ~PC2,
        type = "scatter",
        mode = "markers+text",
        text = ~Environment,
        textposition = "top center",
        marker = list(
          color = meridian_nature_color("signal_blue"),
          size = 8
        ),
        textfont = list(
          color = meridian_nature_color("neutral_dark"),
          size = 11
        ),
        hoverinfo = "text",
        hovertext = ~paste0("Environment: ", Environment, "<br>PC1: ", round(PC1, 3), "<br>PC2: ", round(PC2, 3))
      )
      
      p <- plotly::layout(
        p,
        title = list(
          text = "Environmental PCA",
          font = list(family = "Arial", size = 14, color = meridian_nature_color("neutral_dark"))
        ),
        xaxis = list(
          title = paste0("PC1 (", pc1_pct, "%)"),
          zeroline = TRUE,
          zerolinecolor = meridian_nature_color("neutral_mid"),
          zerolinedash = "dash",
          zerolinewidth = 1,
          showgrid = FALSE
        ),
        yaxis = list(
          title = paste0("PC2 (", pc2_pct, "%)"),
          zeroline = TRUE,
          zerolinecolor = meridian_nature_color("neutral_mid"),
          zerolinedash = "dash",
          zerolinewidth = 1,
          showgrid = FALSE
        ),
        plot_bgcolor = "#ffffff",
        paper_bgcolor = "#ffffff"
      )
      p
    })

    
    output$plot_env_cor <- plotly::renderPlotly({
      req(adapt_results()$env_cor)
      cor_mat <- adapt_results()$env_cor$cor_mat
      color_vec <- grDevices::colorRampPalette(unname(meridian_nature_palette()[c("heat_low", "heat_mid", "heat_high")]))(256)
      
      if (requireNamespace("heatmaply", quietly = TRUE)) {
        heatmaply::heatmaply_cor(
          cor_mat,
          colors = color_vec,
          main = "Covariate Correlation Heatmap",
          margins = c(50, 50, 50, 50)
        )
      } else {
        row_order <- seq_len(nrow(cor_mat))
        col_order <- seq_len(ncol(cor_mat))
        
        if (nrow(cor_mat) > 1) {
          row_order <- tryCatch({
            d_mat <- cor_mat
            d_mat[is.na(d_mat)] <- 0
            stats::hclust(stats::as.dist(1 - d_mat))$order
          }, error = function(e) seq_len(nrow(cor_mat)))
          col_order <- row_order
        }
        
        cor_mat_clustered <- cor_mat[row_order, col_order, drop = FALSE]
        
        n_colors <- length(color_vec)
        colorscale_plotly <- lapply(seq_along(color_vec), function(i) {
          list((i - 1) / (n_colors - 1), color_vec[i])
        })
        
        plotly::plot_ly(
          x = colnames(cor_mat_clustered),
          y = rownames(cor_mat_clustered),
          z = cor_mat_clustered,
          type = "heatmap",
          colorscale = colorscale_plotly,
          zmin = -1, zmax = 1,
          hovertemplate = "Var1: %{y}<br>Var2: %{x}<br>Correlation: %{z:.2f}<extra></extra>"
        ) |>
          meridian_plotly_layout(
            title = "Covariate Correlation Heatmap",
            xaxis = list(title = "", tickangle = -45),
            yaxis = list(title = "", autorange = "reversed"),
            margin = list(l = 100, r = 30, b = 100, t = 80)
          )
      }
    })
    
    output$plot_env_map <- plotly::renderPlotly({
      req(adapt_results()$env_cor)
      # We need Latitude and Longitude to plot map
      merged <- adapt_results()$env_cor$merged_data
      env_col_name <- db()$env_col
      
      # Find Lat/Lon cols (case insensitive)
      lat_col <- grep("lat", names(merged), ignore.case = TRUE, value = TRUE)[1]
      lon_col <- grep("lon", names(merged), ignore.case = TRUE, value = TRUE)[1]
      
      if (is.na(lat_col) || is.na(lon_col)) {
        return(plotly::plotly_empty() |> plotly::layout(title = "Latitude and Longitude columns not found"))
      }
      
      map_df <- merged |>
        dplyr::transmute(
          Environment = as.character(.data[[env_col_name]]),
          Latitude = as.numeric(.data[[lat_col]]),
          Longitude = as.numeric(.data[[lon_col]]),
          env_mean = as.numeric(env_mean)
        ) |>
        dplyr::filter(!is.na(Latitude), !is.na(Longitude), !is.na(env_mean))
      
      if (nrow(map_df) == 0) {
        return(plotly::plotly_empty() |> plotly::layout(title = "No valid coordinates available for mapping"))
      }
      
      plotly::plot_geo(map_df, lat = ~Latitude, lon = ~Longitude) |>
        plotly::add_markers(
          text = ~paste(Environment, "<br>Mean:", round(env_mean, 2)),
          hoverinfo = "text",
          color = ~env_mean,
          colors = unname(meridian_nature_palette()[c("signal_blue", "accent_orange", "accent_red")]),
          size = ~env_mean,
          sizes = c(8, 20),
          marker = list(line = list(width = 0.5, color = meridian_nature_color("neutral_dark")))
        ) |>
        plotly::layout(
          title = "Geo-Spatial Trial Map",
          geo = list(
            scope = "world",
            showland = TRUE,
            landcolor = plotly::toRGB("gray95"),
            countrycolor = plotly::toRGB("gray80")
          )
        )
    })
    
    # ---- Dynamic Covariate Selection Observer ----
    shiny::observe({
      env_df <- db()$env_data
      req(env_df)
      
      # Also take dependency on adapt_results so this updates whenever the analysis is run
      # and the UI is dynamically created in the DOM
      res <- adapt_results()
      req(res)
      
      col_names <- names(env_df)
      env_col <- col_names[grep("^(env|site|loc)", col_names, ignore.case = TRUE)][1]
      if (is.na(env_col)) env_col <- db()$env_col
      lat_col <- col_names[grep("^lat", col_names, ignore.case = TRUE)][1]
      lon_col <- col_names[grep("^lon", col_names, ignore.case = TRUE)][1]
      
      exclude_cols <- c(env_col, lat_col, lon_col, "PlantingDate", "HarvestDate")
      num_cols <- names(env_df)[sapply(env_df, is.numeric)]
      covariate_choices <- setdiff(num_cols, exclude_cols)
      
      shiny::updateSelectInput(
        session,
        "climatic_covariate",
        choices = covariate_choices,
        selected = if ("MeanTemp_C" %in% covariate_choices) "MeanTemp_C" else covariate_choices[1]
      )
    })
    
    # ---- Trait-Environment Mean Reactive ----
    env_means_trait <- shiny::reactive({
      req(db(), input$trait)
      df <- db()$data
      env_col <- db()$env_col
      trait <- input$trait
      
      df_std <- df[!is.na(df[[trait]]), ]
      env_f <- as.factor(df_std[[env_col]])
      y <- as.numeric(df_std[[trait]])
      
      means <- tapply(y, env_f, mean, na.rm = TRUE)
      data.frame(
        Environment = names(means),
        Trait_Mean = as.numeric(means),
        stringsAsFactors = FALSE
      )
    })
    
    merged_climatic_data <- shiny::reactive({
      req(db()$env_data, env_means_trait())
      env_df <- db()$env_data
      trait_means <- env_means_trait()
      
      col_names <- names(env_df)
      env_col <- col_names[grep("^(env|site|loc)", col_names, ignore.case = TRUE)][1]
      if (is.na(env_col)) env_col <- db()$env_col
      
      # Ensure column names are standardized for merge
      names(trait_means)[1] <- env_col
      
      # Merge
      dplyr::inner_join(env_df, trait_means, by = env_col)
    })
    
    # ---- Climatic Drivers Plots ----
    output$plot_climatic_driver <- plotly::renderPlotly({
      req(merged_climatic_data(), input$climatic_covariate, input$trait)
      df_merged <- merged_climatic_data()
      covariate <- input$climatic_covariate
      trait <- input$trait
      
      env_col <- col_names[grep("^(env|site|loc)", col_names, ignore.case = TRUE)][1]
      if (is.na(env_col)) env_col <- db()$env_col
      
      x_vals <- as.numeric(df_merged[[covariate]])
      y_vals <- as.numeric(df_merged$Trait_Mean)
      envs <- as.character(df_merged[[env_col]])
      
      valid_idx <- !is.na(x_vals) & !is.na(y_vals)
      x_vals <- x_vals[valid_idx]
      y_vals <- y_vals[valid_idx]
      envs <- envs[valid_idx]
      
      if (length(x_vals) < 3) {
        return(plotly::plotly_empty() |> plotly::layout(title = "Insufficient data points for regression"))
      }
      
      # Fit linear regression model
      fit <- lm(y_vals ~ x_vals)
      fit_sum <- summary(fit)
      
      r_coef <- cor(x_vals, y_vals, use = "complete.obs")
      r2 <- fit_sum$r.squared
      p_val <- if (nrow(fit_sum$coefficients) > 1) fit_sum$coefficients[2, 4] else NA
      
      # Generate regression line grid
      grid_x <- seq(min(x_vals), max(x_vals), length.out = 100)
      pred_y <- predict(fit, newdata = data.frame(x_vals = grid_x))
      
      # Formatting statistics string
      p_val_str <- if (is.na(p_val)) "N/A" else if (p_val < 0.001) "< 0.001" else sprintf("%.3f", p_val)
      stats_annotation <- sprintf(
        "r = %.3f<br>R² = %.3f<br>p-value = %s",
        r_coef, r2, p_val_str
      )
      
      # Plotly
      plotly::plot_ly() |>
        plotly::add_trace(
          x = x_vals,
          y = y_vals,
          type = "scatter",
          mode = "markers",
          marker = list(
            size = 10,
            color = meridian_nature_color("signal_blue"),
            line = list(width = 1, color = "white")
          ),
          text = envs,
          hovertemplate = "Environment: %{text}<br>Covariate: %{x:.2f}<br>Trait Mean: %{y:.2f}<extra></extra>",
          name = "Observed Mean"
        ) |>
        plotly::add_trace(
          x = grid_x,
          y = pred_y,
          type = "scatter",
          mode = "lines",
          line = list(
            color = meridian_nature_color("accent_red"),
            width = 2
          ),
          hoverinfo = "skip",
          name = "Linear Fit"
        ) |>
        meridian_plotly_layout(
          title = paste("Trait Mean (", trait, ") vs ", covariate, sep = ""),
          xaxis = list(title = covariate),
          yaxis = list(title = paste("Environment Mean (", trait, ")", sep = ""))
        ) |>
        plotly::layout(
          annotations = list(
            list(
              x = 0.05,
              y = 0.95,
              xref = "paper",
              yref = "paper",
              text = stats_annotation,
              showarrow = FALSE,
              align = "left",
              bgcolor = "rgba(255, 255, 255, 0.85)",
              bordercolor = "#ddd",
              borderwidth = 1,
              borderpad = 6,
              font = list(size = 11, family = "Outfit, sans-serif")
            )
          )
        )
    })
    
    output$plot_climatic_importance <- plotly::renderPlotly({
      req(merged_climatic_data(), input$trait)
      df_merged <- merged_climatic_data()
      trait <- input$trait
      
      # Select all covariate columns (numeric and not lat/lon/dates)
      col_names <- names(df_merged)
      env_col <- col_names[grep("^(env|site|loc)", col_names, ignore.case = TRUE)][1]
      if (is.na(env_col)) env_col <- db()$env_col
      lat_col <- col_names[grep("^lat", col_names, ignore.case = TRUE)][1]
      lon_col <- col_names[grep("^lon", col_names, ignore.case = TRUE)][1]
      
      exclude_cols <- c(env_col, lat_col, lon_col, "PlantingDate", "HarvestDate", "Trait_Mean")
      num_cols <- names(df_merged)[sapply(df_merged, is.numeric)]
      covariates <- setdiff(num_cols, exclude_cols)
      
      if (length(covariates) == 0) {
        return(plotly::plotly_empty() |> plotly::layout(title = "No covariates available for correlation analysis"))
      }
      
      # Calculate Pearson correlations
      cor_results <- sapply(covariates, function(cov) {
        cor(as.numeric(df_merged[[cov]]), as.numeric(df_merged$Trait_Mean), use = "complete.obs")
      })
      
      importance_df <- data.frame(
        Covariate = names(cor_results),
        Correlation = as.numeric(cor_results),
        stringsAsFactors = FALSE
      )
      
      importance_df <- importance_df[order(importance_df$Correlation), ]
      importance_df$Covariate <- factor(importance_df$Covariate, levels = importance_df$Covariate)
      
      # Select colors dynamically: positive = emerald green, negative = coral/orange
      importance_df$Color <- ifelse(
        importance_df$Correlation >= 0,
        meridian_nature_color("signal_blue"),
        meridian_nature_color("accent_orange")
      )
      
      plotly::plot_ly(
        data = importance_df,
        y = ~Covariate,
        x = ~Correlation,
        type = "bar",
        orientation = "h",
        marker = list(
          color = ~Color,
          line = list(width = 0.5, color = "white")
        ),
        hovertemplate = "Covariate: %{y}<br>Pearson r: %{x:.3f}<extra></extra>"
      ) |>
        meridian_plotly_layout(
          title = paste("Climatic Driver Influence on ", trait, sep = ""),
          xaxis = list(title = "Pearson Correlation Coefficient (r)", range = c(-1.1, 1.1)),
          yaxis = list(title = "")
        )
    })
    
    build_climatic_driver_gg <- function(res, current_db, trait, covariate) {
      req(current_db$env_data, trait, covariate)
      df <- current_db$data
      env_col_pheno <- current_db$env_col
      df_std <- df[!is.na(df[[trait]]), ]
      means <- tapply(df_std[[trait]], df_std[[env_col_pheno]], mean, na.rm = TRUE)
      
      trait_means <- data.frame(
        Environment = names(means),
        Trait_Mean = as.numeric(means),
        stringsAsFactors = FALSE
      )
      
      env_data_col_names <- names(current_db$env_data)
      env_data_env_col <- env_data_col_names[grep("^(env|site|loc)", env_data_col_names, ignore.case = TRUE)][1]
      if (is.na(env_data_env_col)) env_data_env_col <- env_col_pheno
      
      names(trait_means)[1] <- env_data_env_col
      
      df_merged <- dplyr::inner_join(current_db$env_data, trait_means, by = env_data_env_col)
      
      x_vals <- as.numeric(df_merged[[covariate]])
      y_vals <- as.numeric(df_merged$Trait_Mean)
      
      ggplot2::ggplot(df_merged, ggplot2::aes(x = .data[[covariate]], y = Trait_Mean)) +
        ggplot2::geom_point(color = meridian_nature_color("signal_blue"), size = 3, alpha = 0.8) +
        ggplot2::geom_smooth(method = "lm", formula = y ~ x, color = meridian_nature_color("accent_red"), se = TRUE, fill = "gray90", alpha = 0.5) +
        ggplot2::labs(
          title = paste0("Trait Mean (", trait, ") vs ", covariate),
          x = covariate,
          y = paste("Environment Mean (", trait, ")", sep = "")
        ) +
        theme_meridian_nature()
    }
    
    build_climatic_importance_gg <- function(res, current_db, trait) {
      req(current_db$env_data, trait)
      df <- current_db$data
      env_col_pheno <- current_db$env_col
      df_std <- df[!is.na(df[[trait]]), ]
      means <- tapply(df_std[[trait]], df_std[[env_col_pheno]], mean, na.rm = TRUE)
      
      trait_means <- data.frame(
        Environment = names(means),
        Trait_Mean = as.numeric(means),
        stringsAsFactors = FALSE
      )
      
      env_data_col_names <- names(current_db$env_data)
      env_data_env_col <- env_data_col_names[grep("^(env|site|loc)", env_data_col_names, ignore.case = TRUE)][1]
      if (is.na(env_data_env_col)) env_data_env_col <- env_col_pheno
      
      names(trait_means)[1] <- env_data_env_col
      
      df_merged <- dplyr::inner_join(current_db$env_data, trait_means, by = env_data_env_col)
      
      col_names <- names(df_merged)
      lat_col <- col_names[grep("^lat", col_names, ignore.case = TRUE)][1]
      lon_col <- col_names[grep("^lon", col_names, ignore.case = TRUE)][1]
      
      exclude_cols <- c(env_data_env_col, lat_col, lon_col, "PlantingDate", "HarvestDate", "Trait_Mean")
      num_cols <- names(df_merged)[sapply(df_merged, is.numeric)]
      covariates <- setdiff(num_cols, exclude_cols)
      
      cor_results <- sapply(covariates, function(cov) {
        cor(as.numeric(df_merged[[cov]]), as.numeric(df_merged$Trait_Mean), use = "complete.obs")
      })
      
      importance_df <- data.frame(
        Covariate = names(cor_results),
        Correlation = as.numeric(cor_results),
        stringsAsFactors = FALSE
      )
      
      importance_df <- importance_df[order(importance_df$Correlation), ]
      importance_df$Covariate <- factor(importance_df$Covariate, levels = importance_df$Covariate)
      
      ggplot2::ggplot(importance_df, ggplot2::aes(y = Covariate, x = Correlation, fill = Correlation >= 0)) +
        ggplot2::geom_col(width = 0.7, alpha = 0.85) +
        ggplot2::scale_fill_manual(
          values = c("TRUE" = meridian_nature_color("signal_blue"), "FALSE" = meridian_nature_color("accent_orange")),
          guide = "none"
        ) +
        ggplot2::labs(
          title = paste("Climatic Driver Influence on", trait),
          x = "Pearson Correlation Coefficient (r)",
          y = ""
        ) +
        ggplot2::xlim(-1.1, 1.1) +
        theme_meridian_nature()
    }
    
    shiny::observeEvent(input$send_climatic_driver_report, {
      covariate <- input$climatic_covariate
      req(covariate)
      register_adaptation_plot(
        paste0("climatic_driver_", covariate), 
        paste0("Climatic driver plot (", covariate, ")"),
        function(res, current_db, trait) build_climatic_driver_gg(res, current_db, trait, covariate),
        list(plot_family = "climatic_driver", climatic_covariate = covariate)
      )
    })
    
    shiny::observeEvent(input$send_climatic_importance_report, {
      register_adaptation_plot(
        "climatic_importance", 
        "Climatic driver importance plot",
        function(res, current_db, trait) build_climatic_importance_gg(res, current_db, trait),
        list(plot_family = "climatic_importance")
      )
    })
    
    return(list(
      adapt_results = adapt_results
    ))
  })
}
