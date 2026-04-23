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
            full_screen = TRUE,
            style = "min-height: 400px;",
            shinycssloaders::withSpinner(
              plotly::plotlyOutput(ns("plot_mega_env")),
              type = 6, color = "#2c7a51"
            )
          ),
          bslib::card(
            bslib::card_header("Reaction Norms (Finlay-Wilkinson)"),
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
mod_adaptation_server <- function(id, data_result) {
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
          env_res <- safe_analysis(
            compute_env_pca(db()$env_data, db()$env_col),
            session
          )
          
          if (!is.null(fw_res$env_means)) {
             env_cor <- safe_analysis(
               compute_covariate_correlations(db()$env_data, fw_res$env_means, db()$env_col),
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
      
      # metan env_stratification factor analysis gives us an object we can plot
      # For now, let's just make a simple bar plot of Env vs MegaEnv or network if available.
      # metan has a plot() method for env_stratification, but it might not be a plotly.
      # Let's extract the table and plot
      df_stat <- res$env_strat
      if (is.null(df_stat)) return(plotly::plotly_empty())
      
      p <- ggplot2::ggplot(df_stat, ggplot2::aes(x = ENV, y = MEAN, fill = MEGA_ENV, 
                                                 text = paste("Winner:", Winning_Genotype, "<br>Mean:", round(MEAN, 2)))) +
        ggplot2::geom_col(position = "dodge", color = "black") +
        ggplot2::labs(
          title = paste("Environments Grouped by Mega-Environment (", input$trait, ")", sep=""),
          x = "Environment",
          y = paste("Max Mean", input$trait),
          fill = "Mega-Environment"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
      
      plotly::ggplotly(p, tooltip = "text")
    })
    
    output$plot_fw <- plotly::renderPlotly({
      req(adapt_results()$fw)
      fw <- adapt_results()$fw
      
      p <- ggplot2::ggplot(fw$fw_data, ggplot2::aes(x = EI, y = mean_val, color = !!rlang::sym(db()$gen_col))) +
        ggplot2::geom_point(alpha = 0.5) +
        ggplot2::geom_abline(data = fw$gen_slopes, ggplot2::aes(intercept = intercept, slope = slope, color = !!rlang::sym(db()$gen_col)), alpha = 0.6) +
        ggplot2::labs(
          title = paste("Reaction Norms (Finlay-Wilkinson) -", input$trait),
          x = "Environmental Index",
          y = paste("Genotype Mean", input$trait)
        ) +
        ggplot2::theme_minimal()
      
      plotly::ggplotly(p)
    })
    
    # ---- Enviromics UI & Plots ----
    
    output$enviromics_ui <- shiny::renderUI({
      res <- adapt_results()
      if (is.null(res)) return(shiny::p("Run analysis first."))
      
      if (!res$has_env) {
        return(
          shiny::div(
            class = "alert alert-warning",
            shiny::icon("exclamation-triangle"), 
            " No environmental covariates loaded. Please upload environmental data in the Data Upload tab to use these features."
          )
        )
      }
      
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
    })
    
    output$plot_env_pca <- plotly::renderPlotly({
      req(adapt_results()$env_pca)
      pca <- adapt_results()$env_pca$pca_res
      env_labels <- adapt_results()$env_pca$env_labels
      
      # Extract components
      df_pca <- as.data.frame(pca$x)
      df_pca$Environment <- env_labels
      
      p <- ggplot2::ggplot(df_pca, ggplot2::aes(x = PC1, y = PC2, label = Environment)) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
        ggplot2::geom_point(color = "#2c7a51", size = 3) +
        ggrepel::geom_text_repel() +
        ggplot2::labs(
          title = "Environmental PCA",
          x = paste0("PC1 (", round(summary(pca)$importance[2,1]*100, 1), "%)"),
          y = paste0("PC2 (", round(summary(pca)$importance[2,2]*100, 1), "%)")
        ) +
        ggplot2::theme_minimal()
      
      plotly::ggplotly(p)
    })
    
    output$plot_env_cor <- plotly::renderPlotly({
      req(adapt_results()$env_cor)
      cor_mat <- adapt_results()$env_cor$cor_mat
      
      heatmaply::heatmaply_cor(
        cor_mat,
        colors = heatmaply::cool_warm,
        main = "Covariate Correlation Heatmap",
        margins = c(50, 50, 50, 50)
      )
    })
    
    output$plot_env_map <- plotly::renderPlotly({
      req(adapt_results()$env_cor)
      # We need Latitude and Longitude to plot map
      merged <- adapt_results()$env_cor$merged_data
      
      # Find Lat/Lon cols (case insensitive)
      lat_col <- grep("lat", names(merged), ignore.case = TRUE, value = TRUE)[1]
      lon_col <- grep("lon", names(merged), ignore.case = TRUE, value = TRUE)[1]
      
      if (is.na(lat_col) || is.na(lon_col)) {
        return(plotly::plotly_empty() |> plotly::layout(title = "Latitude and Longitude columns not found"))
      }
      
      # Map
      p <- plotly::plot_geo(merged, lat = as.formula(paste0("~", lat_col)), lon = as.formula(paste0("~", lon_col))) |>
        plotly::add_markers(
          text = ~paste(Environment, "<br>Mean:", round(env_mean, 2)),
          color = ~env_mean,
          colors = "YlOrRd",
          size = ~env_mean,
          hoverinfo = "text"
        ) |>
        plotly::layout(
          title = "Geo-Spatial Trial Map",
          geo = list(
            scope = "world", # Or 'north america' if applicable
            showland = TRUE,
            landcolor = toRGB("gray95"),
            countrycolor = toRGB("gray80")
          )
        )
      p
    })
    
  })
}
