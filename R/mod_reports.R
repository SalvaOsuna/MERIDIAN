# =============================================================================
# MERIDIAN — Module 7: Reports & Export
# =============================================================================

plot_export_controls_ui <- function(ns, prefix = "plt") {
  shiny::tagList(
    shiny::tags$h6("Typography"),
    shiny::selectInput(ns(paste0(prefix, "_font_family")), "Font Family",
      choices = c("sans", "serif", "mono", "Arial", "Times New Roman", "Helvetica"), selected = "sans"),
    shiny::numericInput(ns(paste0(prefix, "_base_size")), "Base Font Size", value = 12, min = 6, step = 1),
    shiny::numericInput(ns(paste0(prefix, "_axis_title_size")), "Axis Title Font Size", value = 12, min = 6, step = 1),
    shiny::numericInput(ns(paste0(prefix, "_axis_text_size")), "Axis Text Font Size", value = 10, min = 6, step = 1),
    shiny::numericInput(ns(paste0(prefix, "_legend_title_size")), "Legend Title Font Size", value = 10, min = 6, step = 1),
    shiny::numericInput(ns(paste0(prefix, "_legend_text_size")), "Legend Text Font Size", value = 9, min = 6, step = 1),
    shiny::numericInput(ns(paste0(prefix, "_plot_title_size")), "Plot Title Font Size", value = 14, min = 6, step = 1),
    shiny::numericInput(ns(paste0(prefix, "_caption_size")), "Caption Font Size", value = 9, min = 6, step = 1),

    shiny::tags$hr(),
    shiny::tags$h6("Labels"),
    shiny::checkboxInput(ns(paste0(prefix, "_show_labels")), "Show Labels", value = TRUE),
    shiny::checkboxInput(ns(paste0(prefix, "_use_repel")), "Use geom_text_repel", value = TRUE),
    shiny::numericInput(ns(paste0(prefix, "_label_size")), "Label Font Size", value = 3.5, min = 1, step = 0.1),
    shiny::selectInput(ns(paste0(prefix, "_label_face")), "Label Font Face",
      choices = c("plain", "bold", "italic", "bold.italic"), selected = "plain"),
    shiny::textInput(ns(paste0(prefix, "_label_color")), "Label Color", value = "#222222"),
    shiny::numericInput(ns(paste0(prefix, "_max_overlaps")), "max.overlaps", value = 30, min = 1),
    shiny::numericInput(ns(paste0(prefix, "_label_force")), "Repulsion Force", value = 1.0, min = 0, step = 0.1),
    shiny::numericInput(ns(paste0(prefix, "_min_seg_len")), "Minimum Segment Length", value = 0.1, min = 0, step = 0.1),
    shiny::textInput(ns(paste0(prefix, "_segment_color")), "Segment Color", value = "#666666"),
    shiny::selectInput(ns(paste0(prefix, "_segment_linetype")), "Segment Linetype",
      choices = c("solid", "dashed", "dotted", "dotdash"), selected = "solid"),
    shiny::textInput(ns(paste0(prefix, "_exclude_labels")), "Exclude Labels (comma-separated)", value = ""),

    shiny::tags$hr(),
    shiny::tags$h6("Theme & Aesthetics"),
    shiny::selectInput(ns(paste0(prefix, "_theme_name")), "Base Theme",
      choices = c("theme_bw", "theme_classic", "theme_minimal", "theme_void", "theme_pubr"),
      selected = "theme_bw"),
    shiny::textInput(ns(paste0(prefix, "_geno_color")), "Genotype Color", value = "#1f77b4"),
    shiny::textInput(ns(paste0(prefix, "_env_color")), "Environment Color", value = "#d62728"),
    shiny::textInput(ns(paste0(prefix, "_pos_fill")), "Positive Fill", value = "#1B5E20"),
    shiny::textInput(ns(paste0(prefix, "_neg_fill")), "Negative Fill", value = "#B71C1C"),
    shiny::sliderInput(ns(paste0(prefix, "_point_size")), "Point Size", min = 0.5, max = 8, value = 2.8, step = 0.1),
    shiny::sliderInput(ns(paste0(prefix, "_point_alpha")), "Point Alpha", min = 0.1, max = 1, value = 0.9, step = 0.05),
    shiny::sliderInput(ns(paste0(prefix, "_line_width")), "Line Width", min = 0.1, max = 3, value = 0.8, step = 0.1),
    shiny::sliderInput(ns(paste0(prefix, "_arrow_size")), "Arrow Size", min = 0.05, max = 0.6, value = 0.2, step = 0.05),
    shiny::checkboxInput(ns(paste0(prefix, "_show_grid_major")), "Show Major Grid", value = TRUE),
    shiny::checkboxInput(ns(paste0(prefix, "_show_grid_minor")), "Show Minor Grid", value = FALSE),
    shiny::checkboxInput(ns(paste0(prefix, "_show_legend")), "Show Legend", value = TRUE),
    shiny::selectInput(ns(paste0(prefix, "_legend_position")), "Legend Position",
      choices = c("top", "bottom", "left", "right"), selected = "right"),
    shiny::checkboxInput(ns(paste0(prefix, "_show_plot_title")), "Show Plot Title", value = TRUE),
    shiny::checkboxInput(ns(paste0(prefix, "_show_axis_titles")), "Show Axis Titles", value = TRUE),

    shiny::tags$hr(),
    shiny::tags$h6("Export"),
    shiny::numericInput(ns(paste0(prefix, "_width")), "Width (in)", value = 7, min = 1, step = 0.5),
    shiny::numericInput(ns(paste0(prefix, "_height")), "Height (in)", value = 5, min = 1, step = 0.5),
    shiny::numericInput(ns(paste0(prefix, "_dpi")), "DPI", value = 300, min = 72, step = 10),
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      shiny::actionButton(ns(paste0(prefix, "_dpi150")), "150"),
      shiny::actionButton(ns(paste0(prefix, "_dpi300")), "300"),
      shiny::actionButton(ns(paste0(prefix, "_dpi600")), "600")
    ),
    shiny::selectInput(ns(paste0(prefix, "_format")), "Format", choices = c("PNG", "PDF", "SVG", "TIFF"), selected = "PNG")
  )
}

mod_reports_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::navset_card_tab(
    id = ns("reports_tabs"),
    title = shiny::tagList(shiny::icon("file-alt"), " Reports & Export"),
    bslib::nav_panel(
      "Table Export",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 320,
          shiny::selectInput(ns("table_trait_filter"), "Trait Filter", choices = c("All"), selected = "All"),
          shiny::selectInput(ns("table_select"), "Select Table", choices = NULL),
          shiny::downloadButton(ns("dl_table_csv"), "Download CSV", class = "btn-outline-primary w-100 mb-2"),
          shiny::downloadButton(ns("dl_table_xlsx"), "Download Excel", class = "btn-outline-success w-100")
        ),
        DT::dataTableOutput(ns("table_export_preview"))
      )
    ),
    bslib::nav_panel(
      "Plot Export",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 360,
          shiny::selectInput(ns("export_analysis_type"), "Analysis Type", choices = AVAILABLE_ANALYSIS_TYPES),
          shiny::selectInput(ns("export_trait"), "Trait", choices = NULL),
          shiny::div(
            class = "d-flex align-items-center gap-2 mb-2",
            shiny::actionButton(ns("generate_plot"), "Generate Plot", class = "btn-primary flex-grow-1"),
            shiny::uiOutput(ns("plot_status_indicator"))
          ),
          plot_export_controls_ui(ns, "plt"),
          shiny::downloadButton(ns("dl_plot_file"), "Download Plot", class = "btn-success w-100")
        ),
        shiny::plotOutput(ns("plot_export_preview"), height = "560px"),
        shiny::tags$div(
          class = "mt-3 d-flex align-items-center gap-3",
          shiny::actionButton(ns("include_in_composer"), "+ Include in Figure Composer", class = "btn-success"),
          shiny::uiOutput(ns("composer_registry_status"))
        )
      )
    ),
    bslib::nav_panel(
      "Figure Composer",
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::h5("Plot Library"),
          shiny::uiOutput(ns("composer_library_ui"))
        )
      ),
      shiny::tags$hr(),
      bslib::layout_column_wrap(
        width = 1/3,
        fill = FALSE,
        shiny::numericInput(ns("comp_ncol"), "Number of Columns", value = 2, min = 1),
        shiny::textInput(ns("comp_width_ratios"), "Column Width Ratios", value = ""),
        shiny::textInput(ns("comp_height_ratios"), "Row Height Ratios", value = ""),
        shiny::selectInput(ns("comp_operator"), "Combination Operator", choices = c("|", "/", "+"), selected = "+"),
        shiny::checkboxInput(ns("comp_collect_guides"), "Collect Guides", value = TRUE)
      ),
      bslib::layout_column_wrap(
        width = 1/3,
        fill = FALSE,
        shiny::checkboxInput(ns("comp_auto_labels"), "Automatic Panel Labels", value = TRUE),
        shiny::selectInput(ns("comp_label_style"), "Label Style",
          choices = c("A/B/C", "a/b/c", "1/2/3", "(A)/(B)/(C)", "(a)/(b)/(c)"), selected = "A/B/C"),
        shiny::numericInput(ns("comp_label_size"), "Label Font Size", value = 13, min = 6),
        shiny::selectInput(ns("comp_label_face"), "Label Font Face", choices = c("bold", "plain", "italic", "bold.italic"), selected = "bold"),
        shiny::textInput(ns("comp_label_color"), "Label Color", value = "#111111"),
        shiny::selectInput(ns("comp_label_pos"), "Label Position", choices = c("top-left", "top-right"), selected = "top-left")
      ),
      bslib::layout_column_wrap(
        width = 1/3,
        fill = FALSE,
        shiny::textInput(ns("comp_title"), "Figure Title", value = ""),
        shiny::textInput(ns("comp_subtitle"), "Figure Subtitle", value = ""),
        shiny::textInput(ns("comp_caption"), "Figure Caption", value = ""),
        shiny::checkboxInput(ns("comp_shared_theme"), "Apply Shared Theme", value = FALSE),
        shiny::selectInput(ns("comp_shared_font"), "Shared Font", choices = c("sans", "serif", "mono"), selected = "serif"),
        shiny::numericInput(ns("comp_shared_base"), "Shared Base Size", value = 11, min = 6),
        colourpicker::colourInput(ns("comp_bg_color"), "Figure Background", value = "#FFFFFF")
      ),
      shiny::actionButton(ns("comp_refresh"), "Refresh Preview", class = "btn-outline-primary"),
      shiny::uiOutput(ns("comp_warning_ui")),
      shiny::plotOutput(ns("composer_preview"), height = "600px"),
      shiny::tags$hr(),
      bslib::layout_column_wrap(
        width = 1/5,
        fill = FALSE,
        shiny::numericInput(ns("comp_width"), "Width (in)", value = 10, min = 2),
        shiny::numericInput(ns("comp_height"), "Height (in)", value = 7, min = 2),
        shiny::numericInput(ns("comp_dpi"), "DPI", value = 300, min = 72),
        shiny::selectInput(ns("comp_format"), "Format", choices = c("PNG", "PDF", "SVG", "TIFF"), selected = "PNG"),
        shiny::downloadButton(ns("dl_comp_figure"), "Download Composed Figure", class = "btn-success")
      ),
      shiny::downloadButton(ns("dl_comp_panels_zip"), "Download Panels ZIP", class = "btn-outline-secondary")
    ),
    bslib::nav_panel(
      "Generate Report",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 360,
          shiny::checkboxGroupInput(
            ns("report_sections"), "Include Sections",
            choices = c(
              "Executive Summary", "Data Description", "Exploratory Analysis",
              "ANOVA & Variance Components", "Stability Analysis",
              "Enviromics", "Adapted Genotypes per Environment",
              "Methods Description", "Appendix"
            ),
            selected = c("Executive Summary", "Data Description", "ANOVA & Variance Components", "Stability Analysis", "Methods Description")
          ),
          shiny::textInput(ns("report_title"), "Report Title", value = "MERIDIAN Analysis Report"),
          shiny::textInput(ns("report_author"), "Author Name", value = ""),
          shiny::textInput(ns("report_institution"), "Institution", value = ""),
          shiny::textAreaInput(ns("report_notes"), "Notes / Comments", value = "", rows = 4),
          shiny::radioButtons(ns("report_format"), "Output Format", choices = c("HTML", "PDF"), selected = "HTML", inline = TRUE),
          shiny::checkboxInput(ns("report_include_date"), "Include Date", value = TRUE),
          shiny::actionButton(ns("generate_report"), "Generate Report", class = "btn-success w-100"),
          shiny::downloadButton(ns("download_report_manual"), "Download Last Report", class = "btn-outline-primary w-100 mt-2"),
          shiny::downloadButton(ns("download_report_auto"), "auto", style = "display:none;")
        ),
        shiny::uiOutput(ns("report_status_ui_main")),
        shiny::verbatimTextOutput(ns("report_error"))
      )
    ),
    header = shiny::tags$head(
      shiny::tags$script(shiny::HTML("
        Shiny.addCustomMessageHandler('meridian-auto-download', function(id) {
          var el = document.getElementById(id);
          if (el) { el.click(); }
        });
      "))
    )
  )
}

mod_reports_server <- function(id, data_result, anova_result = NULL, stab_result = NULL,
                               adapt_result = NULL, plot_registry = NULL, results_store = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (is.null(plot_registry)) {
      plot_registry <- shiny::reactiveValues()
    }

    db <- shiny::reactive({
      req(data_result$data_bundle())
      data_result$data_bundle()
    })
    anova_res <- shiny::reactive({
      if (is.null(anova_result) || is.null(anova_result$anova_results)) return(NULL)
      anova_result$anova_results()
    })
    stab_res <- shiny::reactive({
      if (is.null(stab_result) || is.null(stab_result$stab_results)) return(NULL)
      stab_result$stab_results()
    })
    adapt_res <- shiny::reactive({
      if (is.null(adapt_result) || is.null(adapt_result$adapt_results)) return(NULL)
      adapt_result$adapt_results()
    })

    observeEvent(input$plt_dpi150, shiny::updateNumericInput(session, "plt_dpi", value = 150))
    observeEvent(input$plt_dpi300, shiny::updateNumericInput(session, "plt_dpi", value = 300))
    observeEvent(input$plt_dpi600, shiny::updateNumericInput(session, "plt_dpi", value = 600))

    plot_cfg <- shiny::reactive({
      list(
        theme_name = input$plt_theme_name,
        font_family = input$plt_font_family,
        base_size = input$plt_base_size,
        axis_title_size = input$plt_axis_title_size,
        axis_text_size = input$plt_axis_text_size,
        legend_title_size = input$plt_legend_title_size,
        legend_text_size = input$plt_legend_text_size,
        plot_title_size = input$plt_plot_title_size,
        caption_size = input$plt_caption_size,
        label_size = input$plt_label_size,
        label_face = input$plt_label_face,
        label_color = input$plt_label_color,
        max_overlaps = input$plt_max_overlaps,
        label_force = input$plt_label_force,
        min_seg_len = input$plt_min_seg_len,
        segment_color = input$plt_segment_color,
        segment_linetype = input$plt_segment_linetype,
        use_repel = isTRUE(input$plt_use_repel),
        show_labels = isTRUE(input$plt_show_labels),
        exclude_labels = parse_label_exclusions(input$plt_exclude_labels),
        geno_color = input$plt_geno_color,
        env_color = input$plt_env_color,
        pos_fill = input$plt_pos_fill,
        neg_fill = input$plt_neg_fill,
        point_size = input$plt_point_size,
        point_alpha = input$plt_point_alpha,
        line_width = input$plt_line_width,
        arrow_size = input$plt_arrow_size,
        show_grid_major = isTRUE(input$plt_show_grid_major),
        show_grid_minor = isTRUE(input$plt_show_grid_minor),
        show_legend = isTRUE(input$plt_show_legend),
        legend_position = input$plt_legend_position,
        show_plot_title = isTRUE(input$plt_show_plot_title),
        show_axis_titles = isTRUE(input$plt_show_axis_titles)
      )
    })

    tables_avail <- shiny::reactive({
      collect_available_tables(db(), anova_res = anova_res(), stab_res = stab_res(), adapt_res = adapt_res())
    })

    observe({
      tr <- c("All", db()$traits %||% character(0))
      shiny::updateSelectInput(session, "table_trait_filter", choices = tr, selected = input$table_trait_filter %||% "All")
    })

    tables_filtered <- shiny::reactive({
      t_all <- tables_avail()
      tf <- input$table_trait_filter %||% "All"
      if (identical(tf, "All")) return(t_all)
      keep <- vapply(names(t_all), function(nm) grepl(paste0("—\\s*", tf, "$"), nm), logical(1))
      t_all[keep]
    })

    observe({
      nm <- names(tables_filtered())
      cur <- isolate(input$table_select)
      sel <- if (!is.null(cur) && cur %in% nm) cur else (nm[1] %||% "")
      shiny::updateSelectInput(session, "table_select", choices = nm, selected = sel)
    })

    output$table_export_preview <- DT::renderDataTable({
      req(input$table_select)
      tbl <- tables_filtered()[[input$table_select]]
      req(!is.null(tbl))
      DT::datatable(tbl, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
    })

    output$dl_table_csv <- shiny::downloadHandler(
      filename = function() paste0(gsub("[^A-Za-z0-9_\\-]+", "_", input$table_select), ".csv"),
      content = function(file) {
        tbl <- tables_filtered()[[input$table_select]]
        utils::write.csv(tbl, file, row.names = FALSE)
      }
    )

    output$dl_table_xlsx <- shiny::downloadHandler(
      filename = function() paste0(gsub("[^A-Za-z0-9_\\-]+", "_", input$table_select), ".xlsx"),
      content = function(file) {
        tbl <- tables_filtered()[[input$table_select]]
        metadata <- list(
          analysis_date = as.character(Sys.time()),
          selected_table = input$table_select,
          traits = paste(db()$traits %||% character(0), collapse = ", "),
          n_genotypes = dplyr::n_distinct(db()$data[[db()$gen_col]]),
          n_environments = dplyr::n_distinct(db()$data[[db()$env_col]]),
          design = db()$design$design %||% "Unknown"
        )
        write_table_excel_with_metadata(tbl, file, input$table_select, metadata)
      }
    )

    current_base_plot <- shiny::reactiveVal(NULL)
    composer_registry <- shiny::reactiveValues()
    composer_order <- shiny::reactiveVal(character(0))

    observe({
      tr <- db()$traits %||% character(0)
      cur <- shiny::isolate(input$export_trait)
      sel <- if (!is.null(cur) && cur %in% tr) cur else (tr[1] %||% "")
      shiny::updateSelectInput(session, "export_trait", choices = tr, selected = sel)
    })
    
    observeEvent(input$export_trait, {
      current_base_plot(list(error = "Trait changed. Please click 'Generate Plot' to re-run the analysis for the new trait."))
    }, ignoreInit = TRUE)
    observeEvent(input$export_analysis_type, {
      current_base_plot(list(error = "Analysis type changed. Please click 'Generate Plot' to run this analysis."))
    }, ignoreInit = TRUE)

    observeEvent(input$generate_plot, {
      shiny::updateSelectInput(session, "plt_theme_name", selected = "theme_bw")
      shiny::updateSelectInput(session, "plt_font_family", selected = "sans")
      shiny::updateNumericInput(session, "plt_base_size", value = 12)
      shiny::updateNumericInput(session, "plt_axis_title_size", value = 12)
      shiny::updateNumericInput(session, "plt_axis_text_size", value = 10)
      shiny::updateNumericInput(session, "plt_legend_title_size", value = 10)
      shiny::updateNumericInput(session, "plt_legend_text_size", value = 9)
      shiny::updateNumericInput(session, "plt_plot_title_size", value = 14)
      shiny::updateNumericInput(session, "plt_caption_size", value = 9)
      shiny::updateCheckboxInput(session, "plt_show_labels", value = TRUE)
      shiny::updateCheckboxInput(session, "plt_use_repel", value = TRUE)
      shiny::updateNumericInput(session, "plt_label_size", value = 3.5)
      shiny::updateSelectInput(session, "plt_label_face", selected = "plain")
      shiny::updateTextInput(session, "plt_label_color", value = "#222222")
      shiny::updateNumericInput(session, "plt_max_overlaps", value = 30)
      shiny::updateNumericInput(session, "plt_label_force", value = 1.0)
      shiny::updateNumericInput(session, "plt_min_seg_len", value = 0.1)
      shiny::updateTextInput(session, "plt_segment_color", value = "#666666")
      shiny::updateSelectInput(session, "plt_segment_linetype", selected = "solid")
      shiny::updateTextInput(session, "plt_exclude_labels", value = "")
      shiny::updateTextInput(session, "plt_geno_color", value = "#1f77b4")
      shiny::updateTextInput(session, "plt_env_color", value = "#d62728")
      shiny::updateTextInput(session, "plt_pos_fill", value = "#1B5E20")
      shiny::updateTextInput(session, "plt_neg_fill", value = "#B71C1C")
      shiny::updateSliderInput(session, "plt_point_size", value = 2.8)
      shiny::updateSliderInput(session, "plt_point_alpha", value = 0.9)
      shiny::updateSliderInput(session, "plt_line_width", value = 0.8)
      shiny::updateSliderInput(session, "plt_arrow_size", value = 0.2)
      shiny::updateCheckboxInput(session, "plt_show_grid_major", value = TRUE)
      shiny::updateCheckboxInput(session, "plt_show_grid_minor", value = FALSE)
      shiny::updateCheckboxInput(session, "plt_show_legend", value = TRUE)
      shiny::updateSelectInput(session, "plt_legend_position", selected = "right")
      shiny::updateCheckboxInput(session, "plt_show_plot_title", value = TRUE)
      shiny::updateCheckboxInput(session, "plt_show_axis_titles", value = TRUE)

      req(input$export_analysis_type, input$export_trait)
      p <- generate_specific_plot(
        analysis_type = input$export_analysis_type,
        trait = input$export_trait,
        db = db(),
        results_store = results_store,
        cfg = default_plot_cfg()
      )
      
      if (is.null(p)) {
        current_base_plot(list(error = sprintf("Results for %s — %s have not been computed yet. Please run the analysis first in the corresponding module.", input$export_analysis_type, input$export_trait)))
      } else {
        attr(p, "meridian_analysis") <- input$export_analysis_type
        attr(p, "meridian_trait") <- input$export_trait
        current_base_plot(p)
      }
    })

    output$plot_status_indicator <- shiny::renderUI({
      shiny::req(input$export_analysis_type, input$export_trait)
      store_type <- map_analysis_to_store_key(input$export_analysis_type)
      key <- make_results_key(store_type, input$export_trait)
      
      if (!is.null(results_store[[key]])) {
        shiny::tags$span(class = "badge bg-success", shiny::icon("check-circle"), " Results available")
      } else {
        shiny::tags$span(class = "badge bg-warning text-dark", shiny::icon("exclamation-triangle"), " Not yet computed")
      }
    })

    output$plot_export_preview <- shiny::renderPlot({
      p <- current_base_plot()
      if (is.null(p)) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, "Please select an Analysis Type and Trait, then click 'Generate Plot'.")
        return(invisible(NULL))
      }
      if (is.list(p) && !is.null(p$error)) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, p$error)
        return(invisible(NULL))
      }
      
      customized_p <- apply_common_theme_controls(p, plot_cfg())
      print(customized_p)
    }, res = 96)

    observe({
      p <- current_base_plot()
      is_valid_plot <- inherits(p, "gg")
      if (requireNamespace("shinyjs", quietly = TRUE)) {
        shinyjs::toggleState("include_in_composer", condition = is_valid_plot)
      }
    })

    observeEvent(input$include_in_composer, {
      p <- current_base_plot()
      req(inherits(p, "gg"))
      
      key_base <- paste0(input$export_analysis_type, "__", input$export_trait)
      customized_p <- apply_common_theme_controls(p, plot_cfg())
      
      if (key_base %in% names(composer_registry)) {
        shiny::showModal(shiny::modalDialog(
          title = "Duplicate Plot Detected",
          shiny::tags$p(sprintf("A plot for '%s' already exists in the Figure Composer. Do you want to replace it or keep both?", key_base)),
          footer = shiny::tagList(
            shiny::actionButton(ns("modal_replace_plot"), "Replace", class = "btn-danger"),
            shiny::actionButton(ns("modal_keep_both_plot"), "Keep Both", class = "btn-primary"),
            shiny::modalButton("Cancel")
          )
        ))
      } else {
        composer_registry[[key_base]] <- customized_p
        composer_order(c(composer_order(), key_base))
        shiny::showNotification(paste("Plot added to Figure Composer:", key_base), type = "message")
      }
    })

    observeEvent(input$modal_replace_plot, {
      shiny::removeModal()
      p <- current_base_plot()
      req(inherits(p, "gg"))
      key_base <- paste0(input$export_analysis_type, "__", input$export_trait)
      customized_p <- apply_common_theme_controls(p, plot_cfg())
      composer_registry[[key_base]] <- customized_p
      if (!(key_base %in% composer_order())) {
        composer_order(c(composer_order(), key_base))
      }
      shiny::showNotification(paste("Plot updated in Figure Composer:", key_base), type = "message")
    })

    observeEvent(input$modal_keep_both_plot, {
      shiny::removeModal()
      p <- current_base_plot()
      req(inherits(p, "gg"))
      key_base <- paste0(input$export_analysis_type, "__", input$export_trait)
      
      suffix <- 2
      new_key <- paste0(key_base, "__", suffix)
      while (new_key %in% names(composer_registry)) {
        suffix <- suffix + 1
        new_key <- paste0(key_base, "__", suffix)
      }
      
      customized_p <- apply_common_theme_controls(p, plot_cfg())
      composer_registry[[new_key]] <- customized_p
      composer_order(c(composer_order(), new_key))
      shiny::showNotification(paste("Plot added to Figure Composer:", new_key), type = "message")
    })

    output$composer_registry_status <- shiny::renderUI({
      keys <- names(composer_registry)
      if (length(keys) == 0) {
        shiny::tags$span(class = "badge bg-secondary", "0 plots in composer")
      } else {
        shiny::tags$span(class = "badge bg-info", sprintf("%d plots in composer", length(keys)))
      }
    })

    output$dl_plot_file <- shiny::downloadHandler(
      filename = function() {
        ext <- tolower(input$plt_format %||% "png")
        paste0(gsub("[^A-Za-z0-9_\\-]+", "_", paste0(input$export_analysis_type, "_", input$export_trait)), ".", ext)
      },
      content = function(file) {
        p <- current_base_plot()
        if (is.null(p) || (is.list(p) && !is.null(p$error))) stop("This plot has not been generated yet. Please run the analysis for this trait first.")
        
        fmt <- input$plt_format
        if (toupper(fmt) == "PDF" && !requireNamespace("Cairo", quietly = TRUE)) {
          shiny::showNotification("Cairo not available — font embedding may be limited. Install the Cairo package for publication-quality PDF output.", type = "warning", duration = 10)
        }
        save_ggplot_by_format(
          plot_obj = apply_common_theme_controls(p, plot_cfg()),
          file = file,
          format = fmt,
          width = input$plt_width,
          height = input$plt_height,
          dpi = input$plt_dpi
        )
      }
    )

    # ---- Figure Composer ----
    composer_selection_state <- shiny::reactiveValues()

    output$composer_library_ui <- shiny::renderUI({
      ids <- composer_order()
      if (length(ids) == 0) return(shiny::tags$div(class = "alert alert-secondary", "No plots added to Composer yet. Go to Plot Export to include some."))
      rows <- split(seq_along(ids), ceiling(seq_along(ids) / 3))
      shiny::tagList(lapply(rows, function(ixs) {
        bslib::layout_column_wrap(
          width = 1/3,
          fill = FALSE,
          lapply(ixs, function(i) {
            id <- ids[i]
            p <- composer_registry[[id]]
            lbl <- paste0(
              id,
              if (inherits(p, "gg")) {
                t <- p$labels$title %||% ""
                if (nzchar(t)) paste0(" | ", t) else ""
              } else ""
            )
            bslib::card(
              bslib::card_header(
                shiny::tags$div(
                  class = "d-flex justify-content-between align-items-center",
                  shiny::tags$span(lbl, style = "font-size: 0.85em; font-weight: bold; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; max-width: 60%;", title = lbl),
                  shiny::tags$div(
                    shiny::actionButton(ns(paste0("comp_up_", i)), shiny::icon("arrow-up"), class = "btn-sm p-1", title = "Move Up"),
                    shiny::actionButton(ns(paste0("comp_down_", i)), shiny::icon("arrow-down"), class = "btn-sm p-1", title = "Move Down"),
                    shiny::actionButton(ns(paste0("comp_remove_", i)), shiny::icon("times"), class = "btn-sm btn-danger p-1", title = "Remove")
                  )
                )
              ),
              shiny::plotOutput(ns(paste0("comp_thumb_", i)), height = "180px"),
              shiny::checkboxInput(ns(paste0("comp_sel_", i)), "Selected for Composition", value = shiny::isolate(composer_selection_state[[id]] %||% TRUE))
            )
          })
        )
      }))
    })

    observe({
      ids <- composer_order()
      for (i in seq_along(ids)) {
        local({
          ii <- i
          cur_id <- ids[ii]
          
          output[[paste0("comp_thumb_", ii)]] <- shiny::renderPlot({
            tryCatch({
              op <- graphics::par(no.readonly = TRUE)
              on.exit(graphics::par(op), add = TRUE)
              graphics::par(mar = c(2, 2, 2, 1))
              p <- composer_registry[[cur_id]]
              validate(need(inherits(p, "gg"), paste0("Invalid plot object for key: ", cur_id)))
              print(p)
            }, error = function(e) {
              graphics::plot.new()
              graphics::text(x = 0.5, y = 0.5, labels = "Preview unavailable", cex = 0.9)
            })
          })
          
          shiny::observeEvent(input[[paste0("comp_sel_", ii)]], {
            composer_selection_state[[cur_id]] <- input[[paste0("comp_sel_", ii)]]
          }, ignoreNULL = FALSE, ignoreInit = TRUE)
          
          shiny::observeEvent(input[[paste0("comp_remove_", ii)]], {
            shiny::req(cur_id %in% names(composer_registry))
            composer_registry[[cur_id]] <- NULL
            composer_selection_state[[cur_id]] <- NULL
            composer_order(setdiff(composer_order(), cur_id))
          }, ignoreInit = TRUE)
          
          shiny::observeEvent(input[[paste0("comp_up_", ii)]], {
            current_order <- composer_order()
            idx <- match(cur_id, current_order)
            if (idx > 1) {
              current_order[c(idx - 1, idx)] <- current_order[c(idx, idx - 1)]
              composer_order(current_order)
            }
          }, ignoreInit = TRUE)
          
          shiny::observeEvent(input[[paste0("comp_down_", ii)]], {
            current_order <- composer_order()
            idx <- match(cur_id, current_order)
            if (idx > 0 && idx < length(current_order)) {
              current_order[c(idx, idx + 1)] <- current_order[c(idx + 1, idx)]
              composer_order(current_order)
            }
          }, ignoreInit = TRUE)
        })
      }
    })

    selected_comp_ids <- shiny::reactive({
      ids <- composer_order()
      keep <- vapply(ids, function(id) isTRUE(composer_selection_state[[id]] %||% TRUE), logical(1))
      ids[keep]
    })

    comp_cfg <- shiny::reactive({
      list(
        ncol = input$comp_ncol,
        width_ratios = input$comp_width_ratios,
        height_ratios = input$comp_height_ratios,
        operator = input$comp_operator,
        collect_guides = isTRUE(input$comp_collect_guides),
        auto_labels = isTRUE(input$comp_auto_labels),
        label_style = input$comp_label_style,
        label_size = input$comp_label_size,
        label_face = input$comp_label_face,
        label_color = input$comp_label_color,
        label_position = input$comp_label_pos,
        title = input$comp_title,
        subtitle = input$comp_subtitle,
        caption = input$comp_caption,
        shared_theme = isTRUE(input$comp_shared_theme),
        shared_font_family = input$comp_shared_font,
        shared_base_size = input$comp_shared_base,
        bg_color = input$comp_bg_color
      )
    })

    composed_plot <- shiny::eventReactive({
      input$comp_refresh
      selected_comp_ids()
    }, {
      ids <- selected_comp_ids()
      if (length(ids) < 2) return(NULL)
      built <- lapply(ids, function(id) {
        p <- composer_registry[[id]]
        if (!inherits(p, "gg")) stop("Invalid plot object for key: ", id)
        p
      })
      names(built) <- ids
      compose_patchwork_figure(built, ids, comp_cfg())
    }, ignoreNULL = FALSE)

    output$composer_preview <- shiny::renderPlot({
      p_comp <- composed_plot()
      if (is.null(p_comp)) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, "Please select at least two plots and click 'Refresh Preview'.")
        return(invisible(NULL))
      }
      tryCatch({
        w <- session$clientData[[paste0("output_", ns("composer_preview"), "_width")]] %||% 0
        h <- session$clientData[[paste0("output_", ns("composer_preview"), "_height")]] %||% 0
        if (w < 120 || h < 120) {
          graphics::plot.new()
          graphics::text(0.5, 0.5, "Preview initializing...")
          return(invisible(NULL))
        }
        op <- graphics::par(no.readonly = TRUE)
        on.exit(graphics::par(op), add = TRUE)
        graphics::par(mar = c(0.5, 0.5, 1.5, 0.5))
        print(p_comp)
      }, error = function(e) {
        graphics::plot.new()
        graphics::text(
          x = 0.5, y = 0.5,
          labels = paste("Preview error:", conditionMessage(e)),
          cex = 1
        )
      })
    }, width = function() {
      w <- session$clientData[[paste0("output_", ns("composer_preview"), "_width")]] %||% 900
      max(400, as.numeric(w))
    }, height = function() {
      h <- session$clientData[[paste0("output_", ns("composer_preview"), "_height")]] %||% 600
      max(300, as.numeric(h))
    }, res = 96)

    output$comp_warning_ui <- shiny::renderUI({
      ids <- selected_comp_ids()
      if (length(ids) < 2) return(shiny::tags$div(class = "alert alert-info", "Select at least two plots to compose."))
      built <- lapply(ids, function(id) composer_registry[[id]])
      warns <- check_composer_warnings(built)
      if (length(warns) == 0) return(NULL)
      shiny::tagList(lapply(warns, function(w) shiny::tags$div(class = "alert alert-warning", w)))
    })

    output$dl_comp_figure <- shiny::downloadHandler(
      filename = function() {
        ext <- tolower(input$comp_format %||% "png")
        paste0("composed_figure.", ext)
      },
      content = function(file) {
        fmt <- input$comp_format
        if (toupper(fmt) == "PDF" && !requireNamespace("Cairo", quietly = TRUE)) {
          shiny::showNotification("Cairo not available — font embedding may be limited. Install the Cairo package for publication-quality PDF output.", type = "warning", duration = 10)
        }
        save_ggplot_by_format(
          plot_obj = composed_plot(),
          file = file,
          format = fmt,
          width = input$comp_width,
          height = input$comp_height,
          dpi = input$comp_dpi,
          bg = comp_cfg()$bg_color
        )
      }
    )

    output$dl_comp_panels_zip <- shiny::downloadHandler(
      filename = function() "figure_panels.zip",
      content = function(file) {
        ids <- selected_comp_ids()
        req(length(ids) >= 1)
        built <- lapply(ids, function(id) {
          p <- composer_registry[[id]]
          if (!inherits(p, "gg")) stop("Invalid plot object for key: ", id)
          p
        })
        names(built) <- ids
        export_panels_zip(
          plot_list = built,
          selected_ids = ids,
          cfg = list(format = input$comp_format, width = input$comp_width, height = input$comp_height, dpi = input$comp_dpi, bg_color = comp_cfg()$bg_color),
          zip_file = file
        )
      }
    )

    # ---- Report generation ----
    report_file <- shiny::reactiveVal(NULL)
    report_meta <- shiny::reactiveVal(list(
      filename = "MERIDIAN_Report.html",
      content_type = "text/html"
    ))
    report_error <- shiny::reactiveVal("")
    report_status <- shiny::reactiveVal("No report generated yet.")

    output$report_status_ui_main <- shiny::renderUI({
      shiny::tags$div(class = "alert alert-secondary", report_status())
    })
    output$report_status_ui_modal <- shiny::renderUI({
      shiny::tags$div(class = "alert alert-secondary", report_status())
    })
    output$report_error <- shiny::renderText(report_error())

    output$download_report_auto <- shiny::downloadHandler(
      filename = function() report_meta()$filename %||% "MERIDIAN_Report.html",
      content = function(file) {
        req(report_file(), file.exists(report_file()))
        file.copy(report_file(), file, overwrite = TRUE)
      }
    )
    output$download_report_manual <- shiny::downloadHandler(
      filename = function() report_meta()$filename %||% "MERIDIAN_Report.html",
      content = function(file) {
        req(report_file(), file.exists(report_file()))
        file.copy(report_file(), file, overwrite = TRUE)
      }
    )

    observeEvent(input$generate_report, {
      report_error("")
      showModal(shiny::modalDialog(
        title = "Generating Report",
        shiny::tags$p("Please wait while MERIDIAN compiles your report..."),
        shiny::uiOutput(ns("report_status_ui_modal")),
        shiny::verbatimTextOutput(ns("report_error")),
        easyClose = FALSE,
        footer = NULL
      ))

      tryCatch({
        report_status("Rendering plots...")
        plot_objs <- shiny::reactiveValuesToList(composer_registry)
        plot_objs <- plot_objs[vapply(plot_objs, function(x) inherits(x, "gg"), logical(1))]

        report_status("Compiling tables...")
        tbls <- tables_avail()
        methods_text <- build_methods_text(db(), anova_res = anova_res(), stab_res = stab_res(), adapt_res = adapt_res())

        report_status("Building document...")
        fmt <- input$report_format %||% "HTML"
        ext <- if (identical(fmt, "PDF")) ".pdf" else ".html"
        out <- file.path(tempdir(), paste0("MERIDIAN_Report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ext))

        params <- list(
          report_title = input$report_title,
          author = input$report_author,
          institution = input$report_institution,
          notes = input$report_notes,
          include_date = isTRUE(input$report_include_date),
          sections = input$report_sections %||% character(0),
          tables = tbls,
          plots = plot_objs,
          methods_text = methods_text,
          meta = list(
            n_gen = dplyr::n_distinct(db()$data[[db()$gen_col]]),
            n_env = dplyr::n_distinct(db()$data[[db()$env_col]]),
            design = db()$design$design %||% "Unknown"
          )
        )

        render_meridian_report(out, fmt, params, session = session)
        report_file(out)
        report_meta(list(
          filename = basename(out),
          content_type = if (identical(fmt, "PDF")) "application/pdf" else "text/html"
        ))
        report_status(paste0("Report ready: ", basename(out)))
        removeModal()
        session$onFlushed(function() {
          session$sendCustomMessage("meridian-auto-download", ns("download_report_auto"))
        }, once = TRUE)
      }, error = function(e) {
        report_error(conditionMessage(e))
        report_status("Report generation failed.")
      })
    })

    invisible(list(
      tables = tables_avail,
      plots = composer_order
    ))
  })
}
