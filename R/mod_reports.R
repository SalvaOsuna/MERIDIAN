# =============================================================================
# MERIDIAN - Module 7: Reports & Export
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
      "Registered Tables",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 320,
          shiny::selectInput(ns("table_select"), "Registered Table", choices = NULL),
          shiny::actionButton(ns("remove_table_item"), "Remove Table",
            icon = shiny::icon("trash"), class = "btn-outline-danger w-100 mb-2"),
          shiny::actionButton(ns("clear_table_items"), "Clear All Tables",
            icon = shiny::icon("broom"), class = "btn-outline-danger w-100 mb-2"),
          shiny::downloadButton(ns("dl_table_csv"), "Download CSV", class = "btn-outline-primary w-100 mb-2"),
          shiny::downloadButton(ns("dl_table_xlsx"), "Download Excel", class = "btn-outline-success w-100")
        ),
        shiny::uiOutput(ns("registered_tables_ui")),
        DT::dataTableOutput(ns("table_export_preview"))
      )
    ),
    bslib::nav_panel(
      "Registered Plots",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 360,
          shiny::selectInput(ns("plot_select"), "Registered Plot", choices = NULL),
          shiny::actionButton(ns("plot_update_preview"), "Update Preview",
            icon = shiny::icon("sync"), class = "btn-primary w-100 mb-2"),
          shiny::actionButton(ns("remove_plot_item"), "Remove Plot",
            icon = shiny::icon("trash"), class = "btn-outline-danger w-100 mb-2"),
          shiny::actionButton(ns("clear_plot_items"), "Clear All Plots",
            icon = shiny::icon("broom"), class = "btn-outline-danger w-100 mb-2"),
          plot_export_controls_ui(ns, "plt"),
          shiny::downloadButton(ns("dl_plot_file"), "Download Plot", class = "btn-success w-100")
        ),
        shiny::div(
          class = "registered-plots-library",
          shiny::uiOutput(ns("registered_plots_ui"))
        ),
        shiny::uiOutput(ns("plot_preview_error_ui")),
        shiny::div(
          class = "report-preview-stage",
          shiny::plotOutput(ns("plot_export_preview"), width = "100%", height = "760px")
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
        shiny::numericInput(ns("comp_nrow"), "Number of Rows (0 = Auto)", value = 0, min = 0),
        shiny::checkboxInput(ns("comp_byrow"), "Fill Layout by Row", value = TRUE),
        shiny::textAreaInput(ns("comp_design"), "Custom Design", value = "", rows = 3),
        shiny::textInput(ns("comp_width_ratios"), "Column Width Ratios", value = ""),
        shiny::textInput(ns("comp_height_ratios"), "Row Height Ratios", value = ""),
        shiny::selectInput(ns("comp_operator"), "Combination Operator", choices = c("|", "/", "+"), selected = "+"),
        shiny::checkboxInput(ns("comp_collect_guides"), "Collect Guides", value = TRUE),
        shiny::selectInput(ns("comp_axes"), "Collect Axes",
          choices = c("keep", "collect", "collect_x", "collect_y"), selected = "keep"),
        shiny::selectInput(ns("comp_axis_titles"), "Collect Axis Titles",
          choices = c("keep", "collect", "collect_x", "collect_y"), selected = "keep")
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
        shiny::selectInput(ns("comp_label_pos"), "Label Position", choices = c("top-left", "top-right"), selected = "top-left"),
        shiny::textInput(ns("comp_tag_prefix"), "Tag Prefix", value = ""),
        shiny::textInput(ns("comp_tag_suffix"), "Tag Suffix", value = ""),
        shiny::textInput(ns("comp_tag_sep"), "Tag Separator", value = "")
      ),
      bslib::layout_column_wrap(
        width = 1/3,
        fill = FALSE,
        shiny::textInput(ns("comp_title"), "Figure Title", value = ""),
        shiny::textInput(ns("comp_subtitle"), "Figure Subtitle", value = ""),
        shiny::textInput(ns("comp_caption"), "Figure Caption", value = ""),
        shiny::numericInput(ns("comp_title_size"), "Title Font Size", value = 16, min = 6),
        shiny::selectInput(ns("comp_title_face"), "Title Font Face",
          choices = c("bold", "plain", "italic", "bold.italic"), selected = "bold"),
        shiny::selectInput(ns("comp_title_hjust"), "Title Alignment",
          choices = c("left" = 0, "center" = 0.5, "right" = 1), selected = 0),
        shiny::selectInput(ns("comp_caption_hjust"), "Caption Alignment",
          choices = c("left" = 0, "center" = 0.5, "right" = 1), selected = 1),
        shiny::checkboxInput(ns("comp_shared_theme"), "Apply Shared Theme", value = FALSE),
        shiny::selectInput(ns("comp_shared_theme_name"), "Shared Theme",
          choices = c("theme_minimal", "theme_bw", "theme_classic", "theme_void"), selected = "theme_minimal"),
        shiny::selectInput(ns("comp_shared_font"), "Shared Font", choices = c("sans", "serif", "mono"), selected = "serif"),
        shiny::numericInput(ns("comp_shared_base"), "Shared Base Size", value = 11, min = 6),
        shiny::selectInput(ns("comp_shared_legend"), "Shared Legend Position",
          choices = c("right", "bottom", "top", "left", "none"), selected = "right"),
        shiny::checkboxInput(ns("comp_shared_grid_major"), "Shared Major Grid", value = TRUE),
        shiny::checkboxInput(ns("comp_shared_grid_minor"), "Shared Minor Grid", value = FALSE),
        shiny::numericInput(ns("comp_outer_margin"), "Outer Margin", value = 8, min = 0)
      ),
      shiny::actionButton(ns("comp_refresh"), "Refresh Preview", class = "btn-outline-primary"),
      shiny::uiOutput(ns("comp_warning_ui")),
      shiny::div(
        class = "composer-preview-stage",
        shiny::plotOutput(ns("composer_preview"), width = "100%", height = "780px")
      ),
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
          shiny::checkboxGroupInput(ns("report_plot_ids"), "Include Registered Plots", choices = NULL),
          shiny::checkboxGroupInput(ns("report_table_ids"), "Include Registered Tables", choices = NULL),
          shiny::checkboxInput(ns("report_include_composed"), "Include composed figure", value = FALSE),
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
                               adapt_result = NULL, plot_registry = NULL) {
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
      keep <- vapply(names(t_all), function(nm) grepl(paste0("-\\s*", tf, "$"), nm), logical(1))
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

    if (FALSE) observeEvent(list(db(), anova_res(), stab_res(), adapt_res()), {
      new_plots <- collect_plot_registry_entries(
        db(),
        anova_res = anova_res(),
        stab_res = stab_res(),
        adapt_res = adapt_res()
      )
      if (length(new_plots) > 0) {
        for (k in names(new_plots)) {
          plot_registry[[k]] <- new_plots[[k]]
        }
      }
    }, ignoreInit = FALSE)

    registry_keys <- shiny::reactive({
      names(shiny::reactiveValuesToList(plot_registry))
    })

    registry_modules <- shiny::reactive({
      keys <- registry_keys()
      if (length(keys) == 0) return(character(0))
      unique(vapply(strsplit(keys, "__", fixed = TRUE), function(x) x[1], character(1)))
    })

    observe({
      mods <- registry_modules()
      cur <- isolate(input$plot_module)
      sel <- if (!is.null(cur) && cur %in% mods) cur else (mods[1] %||% "")
      shiny::updateSelectInput(session, "plot_module", choices = mods, selected = sel)
    })

    module_traits <- shiny::reactive({
      req(input$plot_module)
      keys <- registry_keys()
      if (length(keys) == 0) return(character(0))
      parts <- strsplit(keys, "__", fixed = TRUE)
      traits <- vapply(parts, function(x) if (length(x) >= 2 && x[1] == input$plot_module) x[2] else NA_character_, character(1))
      unique(stats::na.omit(traits))
    })

    observe({
      tr <- module_traits()
      cur <- isolate(input$plot_trait)
      sel <- if (!is.null(cur) && cur %in% tr) cur else (tr[1] %||% "")
      shiny::updateSelectInput(session, "plot_trait", choices = tr, selected = sel)
    })

    selected_plot_key <- shiny::reactive({
      req(input$plot_module, input$plot_trait)
      paste(input$plot_module, input$plot_trait, sep = "__")
    })

    selected_plot <- shiny::reactive({
      key <- selected_plot_key()
      obj <- plot_registry[[key]]
      if (is.null(obj)) return(NULL)
      obj
    })

    assert_plot_matches_trait <- function(p, trait, key) {
      if (!inherits(p, "gg")) stop("Retrieved object is not a ggplot for key: ", key)
      key_attr <- attr(p, "meridian_key")
      tr_attr <- attr(p, "meridian_trait")
      if (!is.null(key_attr) && !identical(key_attr, key)) {
        stop("Registry mismatch for selected key ", key, ". Please regenerate this trait and try again.")
      }
      if (!is.null(tr_attr) && !identical(as.character(tr_attr), as.character(trait))) {
        stop("Registry mismatch for selected key ", key, ". Please regenerate this trait and try again.")
      }
      TRUE
    }

    output$plot_export_preview <- shiny::renderPlot({
      key <- selected_plot_key()
      p <- selected_plot()
      if (is.null(p)) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, "This plot has not been generated yet.\nPlease run the analysis for this trait first.")
        return(invisible(NULL))
      }
      validate(need(inherits(p, "gg"), paste0("Invalid plot object at key: ", key)))
      ok <- tryCatch(assert_plot_matches_trait(p, input$plot_trait, key), error = function(e) e)
      if (inherits(ok, "error")) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, conditionMessage(ok))
        return(invisible(NULL))
      }
      print(apply_common_theme_controls(p, plot_cfg()))
    }, res = 96)

    output$dl_plot_file <- shiny::downloadHandler(
      filename = function() {
        ext <- tolower(input$plt_format %||% "png")
        paste0(gsub("[^A-Za-z0-9_\\-]+", "_", selected_plot_key()), ".", ext)
      },
      content = function(file) {
        key <- selected_plot_key()
        p <- selected_plot()
        if (is.null(p)) stop("This plot has not been generated yet. Please run the analysis for this trait first.")
        assert_plot_matches_trait(p, input$plot_trait, key)
        save_ggplot_by_format(
          plot_obj = apply_common_theme_controls(p, plot_cfg()),
          file = file,
          format = input$plt_format,
          width = input$plt_width,
          height = input$plt_height,
          dpi = input$plt_dpi
        )
      }
    )

    # ---- Legacy Figure Composer (disabled; registry composer below is authoritative) ----
    if (FALSE) {
    output$composer_library_ui <- shiny::renderUI({
      ids <- registry_keys()
      if (length(ids) == 0) return(shiny::tags$div("No plots available yet. Run analyses first."))
      rows <- split(seq_along(ids), ceiling(seq_along(ids) / 3))
      shiny::tagList(lapply(rows, function(ixs) {
        bslib::layout_column_wrap(
          width = 1/3,
          fill = FALSE,
          lapply(ixs, function(i) {
            id <- ids[i]
            p <- plot_registry[[id]]
            lbl <- paste0(
              id,
              if (inherits(p, "gg")) {
                t <- p$labels$title %||% ""
                if (nzchar(t)) paste0(" | ", t) else ""
              } else ""
            )
            bslib::card(
              bslib::card_header(lbl),
              shiny::plotOutput(ns(paste0("thumb_", i)), height = "180px"),
              shiny::checkboxInput(ns(paste0("sel_", i)), "Include", value = FALSE)
            )
          })
        )
      }))
    })

    observe({
      ids <- registry_keys()
      for (i in seq_along(ids)) {
        local({
          ii <- i
          output[[paste0("thumb_", ii)]] <- shiny::renderPlot({
            tryCatch({
              op <- graphics::par(no.readonly = TRUE)
              on.exit(graphics::par(op), add = TRUE)
              graphics::par(mar = c(2, 2, 2, 1))
              p <- plot_registry[[ids[ii]]]
              validate(need(inherits(p, "gg"), paste0("Invalid plot object for key: ", ids[ii])))
              print(p)
            }, error = function(e) {
              graphics::plot.new()
              graphics::text(
                x = 0.5, y = 0.5,
                labels = "Preview unavailable",
                cex = 0.9
              )
            })
          })
        })
      }
    })

    selected_comp_ids <- shiny::reactive({
      ids <- registry_keys()
      keep <- vapply(seq_along(ids), function(i) isTRUE(input[[paste0("sel_", i)]]), logical(1))
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
        shared_base_size = input$comp_shared_base
      )
    })

    comp_refresh_tick <- shiny::reactiveVal(0L)
    observeEvent(input$comp_refresh, comp_refresh_tick(comp_refresh_tick() + 1L))

    composed_plot <- shiny::reactive({
      comp_refresh_tick()
      ids <- selected_comp_ids()
      req(length(ids) >= 2)
      built <- lapply(ids, function(id) {
        p <- plot_registry[[id]]
        if (!inherits(p, "gg")) stop("Invalid plot object for key: ", id)
        apply_common_theme_controls(p, plot_cfg())
      })
      names(built) <- ids
      compose_patchwork_figure(built, ids, comp_cfg())
    })

    output$composer_preview <- shiny::renderPlot({
      req(composed_plot())
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
        print(composed_plot())
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
      built <- lapply(ids, function(id) plot_registry[[id]])
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
        save_ggplot_by_format(
          plot_obj = composed_plot(),
          file = file,
          format = input$comp_format,
          width = input$comp_width,
          height = input$comp_height,
          dpi = input$comp_dpi
        )
      }
    )

    output$dl_comp_panels_zip <- shiny::downloadHandler(
      filename = function() "figure_panels.zip",
      content = function(file) {
        ids <- selected_comp_ids()
        req(length(ids) >= 1)
        built <- lapply(ids, function(id) {
          p <- plot_registry[[id]]
          if (!inherits(p, "gg")) stop("Invalid plot object for key: ", id)
          apply_common_theme_controls(p, plot_cfg())
        })
        names(built) <- ids
        export_panels_zip(
          plot_list = built,
          selected_ids = ids,
          cfg = list(format = input$comp_format, width = input$comp_width, height = input$comp_height, dpi = input$comp_dpi),
          zip_file = file
        )
      }
    )
    }

    # ---- User-driven Report Registry ----
    registry_items <- shiny::reactive(get_report_registry_items(plot_registry))
    registry_plot_items <- shiny::reactive(get_report_plot_items(plot_registry))
    registry_table_items <- shiny::reactive(get_report_table_items(plot_registry))
    current_dataset_signature <- shiny::reactive(make_dataset_signature(db()))

    plot_choices <- shiny::reactive({
      items <- registry_plot_items()
      stats::setNames(names(items), vapply(items, function(x) x$label, character(1)))
    })
    table_choices <- shiny::reactive({
      items <- registry_table_items()
      stats::setNames(names(items), vapply(items, function(x) x$label, character(1)))
    })

    observe({
      choices <- plot_choices()
      cur <- isolate(input$plot_select)
      sel <- if (!is.null(cur) && cur %in% unname(choices)) cur else (unname(choices)[1] %||% "")
      shiny::updateSelectInput(session, "plot_select", choices = choices, selected = sel)
    })
    observe({
      choices <- table_choices()
      cur <- isolate(input$table_select)
      sel <- if (!is.null(cur) && cur %in% unname(choices)) cur else (unname(choices)[1] %||% "")
      shiny::updateSelectInput(session, "table_select", choices = choices, selected = sel)
    })
    observe({
      choices <- plot_choices()
      current <- isolate(input$report_plot_ids %||% character(0))
      shiny::updateCheckboxGroupInput(session, "report_plot_ids",
        choices = choices, selected = intersect(current, unname(choices)))
    })
    observe({
      choices <- table_choices()
      current <- isolate(input$report_table_ids %||% character(0))
      shiny::updateCheckboxGroupInput(session, "report_table_ids",
        choices = choices, selected = intersect(current, unname(choices)))
    })

    build_registry_plot <- function(id, customize = TRUE) {
      item <- registry_plot_items()[[id]]
      if (is.null(item)) stop("Select a registered plot.")
      validate_report_item(item)
      assert_report_item_dataset(item, current_dataset_signature())
      p <- item$builder()
      if (!inherits(p, "gg")) stop("Registered plot did not return a ggplot object.")
      if (isTRUE(customize)) p <- apply_common_theme_controls(p, plot_cfg())
      p
    }
    build_registry_table <- function(id) {
      item <- registry_table_items()[[id]]
      if (is.null(item)) stop("Select a registered table.")
      validate_report_item(item)
      assert_report_item_dataset(item, current_dataset_signature())
      tbl <- item$builder()
      if (!is.data.frame(tbl)) stop("Registered table did not return a data.frame or tibble.")
      as.data.frame(tbl)
    }

    output$registered_plots_ui <- shiny::renderUI({
      items <- registry_plot_items()
      if (length(items) == 0) {
        return(shiny::tags$div(class = "alert alert-info", "No registered plots yet. Use 'Send this plot to Reports' from an analysis module."))
      }
      rows <- lapply(items, function(item) {
        shiny::tags$tr(
          shiny::tags$td(item$label),
          shiny::tags$td(item$module),
          shiny::tags$td(item$trait),
          shiny::tags$td(format(item$created_at, "%Y-%m-%d %H:%M"))
        )
      })
      shiny::tags$table(
        class = "table table-sm table-striped",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th("Label"), shiny::tags$th("Module"),
          shiny::tags$th("Trait"), shiny::tags$th("Created")
        )),
        shiny::tags$tbody(rows)
      )
    })
    output$registered_tables_ui <- shiny::renderUI({
      items <- registry_table_items()
      if (length(items) == 0) {
        return(shiny::tags$div(class = "alert alert-info", "No registered tables yet. Use 'Send table to Reports' from an analysis module."))
      }
      rows <- lapply(items, function(item) {
        shiny::tags$tr(
          shiny::tags$td(item$label),
          shiny::tags$td(item$module),
          shiny::tags$td(item$trait),
          shiny::tags$td(format(item$created_at, "%Y-%m-%d %H:%M"))
        )
      })
      shiny::tags$table(
        class = "table table-sm table-striped",
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th("Label"), shiny::tags$th("Module"),
          shiny::tags$th("Trait"), shiny::tags$th("Created")
        )),
        shiny::tags$tbody(rows)
      )
    })

    preview_plot <- shiny::reactiveVal(NULL)
    preview_error <- shiny::reactiveVal(NULL)
    composed_preview <- shiny::reactiveVal(NULL)
    composed_error <- shiny::reactiveVal(NULL)

    observeEvent(input$remove_plot_item, {
      req(input$plot_select)
      unregister_report_item(plot_registry, input$plot_select)
      preview_plot(NULL)
      composed_preview(NULL)
    })
    observeEvent(input$remove_table_item, {
      req(input$table_select)
      unregister_report_item(plot_registry, input$table_select)
    })
    observeEvent(input$clear_plot_items, {
      clear_report_registry(plot_registry, "plot")
      preview_plot(NULL)
      composed_preview(NULL)
    })
    observeEvent(input$clear_table_items, clear_report_registry(plot_registry, "table"))

    output$table_export_preview <- DT::renderDataTable({
      req(input$table_select)
      DT::datatable(build_registry_table(input$table_select),
        options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
    })
    output$dl_table_csv <- shiny::downloadHandler(
      filename = function() paste0(gsub("[^A-Za-z0-9_\\-]+", "_", input$table_select), ".csv"),
      content = function(file) utils::write.csv(build_registry_table(input$table_select), file, row.names = FALSE)
    )
    output$dl_table_xlsx <- shiny::downloadHandler(
      filename = function() paste0(gsub("[^A-Za-z0-9_\\-]+", "_", input$table_select), ".xlsx"),
      content = function(file) {
        item <- registry_table_items()[[input$table_select]]
        metadata <- list(
          analysis_date = as.character(Sys.time()),
          selected_table = item$label %||% input$table_select,
          module = item$module %||% "",
          trait = item$trait %||% ""
        )
        write_table_excel_with_metadata(build_registry_table(input$table_select), file, item$label, metadata)
      }
    )

    observeEvent(input$plot_update_preview, {
      preview_error(NULL)
      tryCatch({
        req(input$plot_select)
        preview_plot(build_registry_plot(input$plot_select, customize = TRUE))
      }, error = function(e) {
        preview_plot(NULL)
        preview_error(conditionMessage(e))
      })
    })
    output$plot_preview_error_ui <- shiny::renderUI({
      if (is.null(preview_error())) return(NULL)
      shiny::tags$div(class = "alert alert-warning", preview_error())
    })
    output$plot_export_preview <- shiny::renderPlot({
      req(preview_plot())
      print(preview_plot() + ggplot2::theme(plot.margin = ggplot2::margin(8, 8, 8, 8)))
    }, width = function() {
      w <- session$clientData[[paste0("output_", ns("plot_export_preview"), "_width")]] %||% 1000
      max(800, as.numeric(w))
    }, height = 760, res = 96)
    output$dl_plot_file <- shiny::downloadHandler(
      filename = function() {
        ext <- tolower(input$plt_format %||% "png")
        paste0(gsub("[^A-Za-z0-9_\\-]+", "_", input$plot_select), ".", ext)
      },
      content = function(file) {
        p <- preview_plot()
        if (is.null(p)) p <- build_registry_plot(input$plot_select, customize = TRUE)
        save_ggplot_by_format(
          plot_obj = p,
          file = file,
          format = input$plt_format,
          width = input$plt_width,
          height = input$plt_height,
          dpi = input$plt_dpi
        )
      }
    )

    output$composer_library_ui <- shiny::renderUI({
      items <- registry_plot_items()
      ids <- names(items)
      if (length(ids) == 0) return(shiny::tags$div(class = "alert alert-info", "No registered plots yet."))
      shiny::checkboxGroupInput(
        ns("comp_selected_ids"),
        "Selected Plots",
        choices = stats::setNames(ids, vapply(items, function(x) x$label, character(1))),
        selected = isolate(input$comp_selected_ids %||% character(0))
      )
    })
    selected_comp_ids <- shiny::reactive(input$comp_selected_ids %||% character(0))
    comp_cfg <- shiny::reactive({
      list(
        ncol = input$comp_ncol,
        nrow = input$comp_nrow,
        byrow = isTRUE(input$comp_byrow),
        design = input$comp_design,
        width_ratios = input$comp_width_ratios,
        height_ratios = input$comp_height_ratios,
        operator = input$comp_operator,
        collect_guides = isTRUE(input$comp_collect_guides),
        axes = input$comp_axes,
        axis_titles = input$comp_axis_titles,
        auto_labels = isTRUE(input$comp_auto_labels),
        label_style = input$comp_label_style,
        label_size = input$comp_label_size,
        label_face = input$comp_label_face,
        label_color = input$comp_label_color,
        label_position = input$comp_label_pos,
        tag_prefix = input$comp_tag_prefix,
        tag_suffix = input$comp_tag_suffix,
        tag_sep = input$comp_tag_sep,
        title = input$comp_title,
        subtitle = input$comp_subtitle,
        caption = input$comp_caption,
        title_size = input$comp_title_size,
        title_face = input$comp_title_face,
        title_hjust = as.numeric(input$comp_title_hjust),
        caption_hjust = as.numeric(input$comp_caption_hjust),
        shared_theme = isTRUE(input$comp_shared_theme),
        shared_theme_name = input$comp_shared_theme_name,
        shared_font_family = input$comp_shared_font,
        shared_base_size = input$comp_shared_base,
        shared_legend_position = input$comp_shared_legend,
        shared_grid_major = isTRUE(input$comp_shared_grid_major),
        shared_grid_minor = isTRUE(input$comp_shared_grid_minor),
        outer_margin = input$comp_outer_margin
      )
    })

    observeEvent(input$comp_refresh, {
      composed_error(NULL)
      tryCatch({
        ids <- selected_comp_ids()
        req(length(ids) >= 2)
        built <- lapply(ids, function(id) build_registry_plot(id, customize = TRUE))
        names(built) <- ids
        composed_preview(compose_patchwork_figure(built, ids, comp_cfg()))
      }, error = function(e) {
        composed_preview(NULL)
        composed_error(conditionMessage(e))
      })
    })
    output$composer_preview <- shiny::renderPlot({
      req(composed_preview())
      tryCatch({
        print(composed_preview() & ggplot2::theme(plot.margin = ggplot2::margin(8, 8, 8, 8)))
      }, error = function(e) {
        graphics::par(mar = c(1, 1, 1, 1))
        graphics::plot.new()
        graphics::text(0.5, 0.5, paste("Preview error:", conditionMessage(e)), cex = 1)
      })
    }, width = function() {
      w <- session$clientData[[paste0("output_", ns("composer_preview"), "_width")]] %||% 1100
      max(900, as.numeric(w))
    }, height = 780, res = 96)
    output$comp_warning_ui <- shiny::renderUI({
      ids <- selected_comp_ids()
      warns <- character(0)
      if (length(ids) < 2) warns <- c(warns, "Select at least two plots to compose.")
      if (!is.null(composed_error())) warns <- c(warns, composed_error())
      if (length(warns) == 0 && length(ids) >= 2) {
        built <- lapply(ids, function(id) tryCatch(build_registry_plot(id, customize = FALSE), error = function(e) NULL))
        built <- Filter(Negate(is.null), built)
        warns <- check_composer_warnings(built)
      }
      if (length(warns) == 0) return(NULL)
      shiny::tagList(lapply(warns, function(w) shiny::tags$div(class = "alert alert-warning", w)))
    })
    output$dl_comp_figure <- shiny::downloadHandler(
      filename = function() {
        ext <- tolower(input$comp_format %||% "png")
        paste0("composed_figure.", ext)
      },
      content = function(file) {
        p <- composed_preview()
        if (is.null(p)) {
          ids <- selected_comp_ids()
          built <- lapply(ids, function(id) build_registry_plot(id, customize = TRUE))
          names(built) <- ids
          p <- compose_patchwork_figure(built, ids, comp_cfg())
        }
        save_ggplot_by_format(
          plot_obj = p,
          file = file,
          format = input$comp_format,
          width = input$comp_width,
          height = input$comp_height,
          dpi = input$comp_dpi
        )
      }
    )
    output$dl_comp_panels_zip <- shiny::downloadHandler(
      filename = function() "figure_panels.zip",
      content = function(file) {
        ids <- selected_comp_ids()
        req(length(ids) >= 1)
        built <- lapply(ids, function(id) build_registry_plot(id, customize = TRUE))
        names(built) <- ids
        export_panels_zip(
          plot_list = built,
          selected_ids = ids,
          cfg = list(format = input$comp_format, width = input$comp_width, height = input$comp_height, dpi = input$comp_dpi),
          zip_file = file
        )
      }
    )

    tables_avail <- shiny::reactive({
      ids <- names(registry_table_items())
      out <- lapply(ids, build_registry_table)
      names(out) <- vapply(registry_table_items(), function(x) x$label, character(1))
      out
    })

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
        plot_ids <- input$report_plot_ids %||% character(0)
        plot_items <- registry_plot_items()[plot_ids]
        plot_objs <- lapply(plot_ids, function(id) build_registry_plot(id, customize = TRUE))
        names(plot_objs) <- vapply(plot_items, function(x) x$label, character(1))

        report_status("Compiling tables...")
        table_ids <- input$report_table_ids %||% character(0)
        tbls <- lapply(table_ids, build_registry_table)
        names(tbls) <- vapply(registry_table_items()[table_ids], function(x) x$label, character(1))
        if (isTRUE(input$report_include_composed) && !is.null(composed_preview())) {
          plot_objs[["Composed figure"]] <- composed_preview()
        }
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
      plots = function() names(registry_plot_items())
    ))
  })
}
