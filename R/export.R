# =============================================================================
# MERIDIAN — Export Helpers (Tables + Plots)
# =============================================================================

default_plot_cfg <- function() {
  list(
    theme_name = "theme_bw",
    font_family = "sans",
    base_size = 12,
    axis_title_size = 12,
    axis_text_size = 10,
    legend_title_size = 10,
    legend_text_size = 9,
    plot_title_size = 14,
    caption_size = 9,
    label_size = 3.5,
    label_face = "plain",
    label_color = "#222222",
    max_overlaps = 30,
    label_force = 1.0,
    min_seg_len = 0.1,
    segment_color = "#666666",
    segment_linetype = "solid",
    use_repel = TRUE,
    show_labels = TRUE,
    exclude_labels = character(0),
    geno_color = "#1f77b4",
    env_color = "#d62728",
    pos_fill = "#1B5E20",
    neg_fill = "#B71C1C",
    point_size = 2.8,
    point_alpha = 0.9,
    line_width = 0.8,
    arrow_size = 0.2,
    show_grid_major = TRUE,
    show_grid_minor = FALSE,
    show_legend = TRUE,
    legend_position = "right",
    show_plot_title = TRUE,
    show_axis_titles = TRUE
  )
}

parse_label_exclusions <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) return(character(0))
  out <- trimws(unlist(strsplit(x, ",")))
  out[nzchar(out)]
}

base_theme_from_name <- function(name, base_size = 12, base_family = "sans") {
  switch(
    name %||% "theme_bw",
    "theme_bw" = ggplot2::theme_bw(base_size = base_size, base_family = base_family),
    "theme_classic" = ggplot2::theme_classic(base_size = base_size, base_family = base_family),
    "theme_minimal" = ggplot2::theme_minimal(base_size = base_size, base_family = base_family),
    "theme_void" = ggplot2::theme_void(base_size = base_size, base_family = base_family),
    "theme_pubr" = {
      if (requireNamespace("ggpubr", quietly = TRUE)) {
        ggpubr::theme_pubr(base_size = base_size, base_family = base_family)
      } else {
        ggplot2::theme_minimal(base_size = base_size, base_family = base_family)
      }
    },
    ggplot2::theme_bw(base_size = base_size, base_family = base_family)
  )
}

apply_common_theme_controls <- function(p, cfg) {
  th <- base_theme_from_name(cfg$theme_name, cfg$base_size, cfg$font_family)
  p +
    th +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = cfg$axis_title_size, family = cfg$font_family),
      axis.text = ggplot2::element_text(size = cfg$axis_text_size, family = cfg$font_family),
      legend.title = ggplot2::element_text(size = cfg$legend_title_size, family = cfg$font_family),
      legend.text = ggplot2::element_text(size = cfg$legend_text_size, family = cfg$font_family),
      plot.caption = ggplot2::element_text(size = cfg$caption_size, family = cfg$font_family),
      panel.grid.major = if (isTRUE(cfg$show_grid_major)) ggplot2::element_line() else ggplot2::element_blank(),
      panel.grid.minor = if (isTRUE(cfg$show_grid_minor)) ggplot2::element_line() else ggplot2::element_blank(),
      legend.position = if (isTRUE(cfg$show_legend)) cfg$legend_position else "none",
      plot.title = if (isTRUE(cfg$show_plot_title)) ggplot2::element_text(size = cfg$plot_title_size, family = cfg$font_family, face = "bold") else ggplot2::element_blank(),
      axis.title.x = if (isTRUE(cfg$show_axis_titles)) ggplot2::element_text(size = cfg$axis_title_size, family = cfg$font_family) else ggplot2::element_blank(),
      axis.title.y = if (isTRUE(cfg$show_axis_titles)) ggplot2::element_text(size = cfg$axis_title_size, family = cfg$font_family) else ggplot2::element_blank()
    )
}

add_label_layer <- function(p, data, x, y, label, cfg, color = NULL) {
  if (!isTRUE(cfg$show_labels) || is.null(data) || nrow(data) == 0) return(p)
  excl <- cfg$exclude_labels %||% character(0)
  if (length(excl) > 0) {
    data <- data[!(as.character(data[[label]]) %in% excl), , drop = FALSE]
  }
  if (nrow(data) == 0) return(p)

  aes_map <- ggplot2::aes(x = .data[[x]], y = .data[[y]], label = .data[[label]])
  if (isTRUE(cfg$use_repel)) {
    p + ggrepel::geom_text_repel(
      data = data, mapping = aes_map,
      size = cfg$label_size, color = color %||% cfg$label_color, fontface = cfg$label_face,
      max.overlaps = cfg$max_overlaps, force = cfg$label_force,
      min.segment.length = cfg$min_seg_len,
      segment.color = cfg$segment_color, segment.linetype = cfg$segment_linetype,
      inherit.aes = FALSE
    )
  } else {
    p + ggplot2::geom_text(
      data = data, mapping = aes_map,
      size = cfg$label_size, color = color %||% cfg$label_color, fontface = cfg$label_face,
      inherit.aes = FALSE, check_overlap = TRUE
    )
  }
}

collect_available_tables <- function(db, anova_res = NULL, stab_res = NULL, adapt_res = NULL) {
  out <- list()
  trait_anova <- anova_res$trait %||% "Trait"
  trait_stab <- stab_res$trait %||% "Trait"
  trait_adapt <- adapt_res$trait %||% "Trait"

  if (!is.null(stab_res$ammi$stability)) out[[paste0("Stability Metrics — ", trait_stab)]] <- as.data.frame(stab_res$ammi$stability)
  if (!is.null(stab_res$all$stats_table)) out[[paste0("Combined Stability Indices — ", trait_stab)]] <- as.data.frame(stab_res$all$stats_table)
  if (!is.null(anova_res$anova$anova_table)) out[[paste0("ANOVA Table — ", trait_anova)]] <- as.data.frame(anova_res$anova$anova_table)
  if (!is.null(anova_res$blues)) out[[paste0("BLUEs — ", trait_anova)]] <- as.data.frame(anova_res$blues)
  if (!is.null(anova_res$blups)) out[[paste0("BLUPs — ", trait_anova)]] <- as.data.frame(anova_res$blups)
  if (!is.null(stab_res$ammi$gen_means)) out[[paste0("AMMI Genotype Means — ", trait_stab)]] <- as.data.frame(stab_res$ammi$gen_means)
  if (!is.null(stab_res$ammi$env_means)) out[[paste0("AMMI Environment Means — ", trait_stab)]] <- as.data.frame(stab_res$ammi$env_means)
  if (!is.null(stab_res$ammi$variance_explained)) out[[paste0("AMMI Variance Explained — ", trait_stab)]] <- as.data.frame(stab_res$ammi$variance_explained)
  if (!is.null(stab_res$ammi$interaction_matrix)) {
    m <- as.data.frame(stab_res$ammi$interaction_matrix)
    m$Genotype <- rownames(m)
    m <- m[, c("Genotype", setdiff(names(m), "Genotype")), drop = FALSE]
    out[[paste0("GxE Interaction Matrix — ", trait_stab)]] <- m
  }
  if (!is.null(adapt_res$fw$env_means)) out[[paste0("Finlay-Wilkinson Env Means — ", trait_adapt)]] <- as.data.frame(adapt_res$fw$env_means)
  if (!is.null(adapt_res$mega_env$env_strat)) out[[paste0("Mega-Environment Table — ", trait_adapt)]] <- as.data.frame(adapt_res$mega_env$env_strat)
  out
}

write_table_excel_with_metadata <- function(df, file, table_name, metadata) {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Cover")
  openxlsx::addWorksheet(wb, "Data")

  meta_df <- data.frame(
    Field = names(metadata),
    Value = vapply(metadata, function(x) paste(x, collapse = ", "), character(1)),
    stringsAsFactors = FALSE
  )

  openxlsx::writeData(wb, "Cover", x = data.frame(Report = "MERIDIAN Table Export", stringsAsFactors = FALSE), startRow = 1, colNames = FALSE)
  openxlsx::writeData(wb, "Cover", x = data.frame(Table = table_name, stringsAsFactors = FALSE), startRow = 3, colNames = FALSE)
  openxlsx::writeData(wb, "Cover", x = meta_df, startRow = 5)

  header_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#DCE6F1", halign = "center", border = "Bottom")
  odd_style <- openxlsx::createStyle(fgFill = "#FFFFFF")
  even_style <- openxlsx::createStyle(fgFill = "#F7F9FC")

  openxlsx::addStyle(wb, "Cover", header_style, rows = 5, cols = 1:2, gridExpand = TRUE)

  openxlsx::writeData(wb, "Data", x = df, startRow = 1, colNames = TRUE)
  openxlsx::addStyle(wb, "Data", header_style, rows = 1, cols = seq_len(ncol(df)), gridExpand = TRUE)
  if (nrow(df) > 0) {
    odd_rows <- seq(2, nrow(df) + 1, by = 2)
    even_rows <- seq(3, nrow(df) + 1, by = 2)
    if (length(odd_rows) > 0) openxlsx::addStyle(wb, "Data", odd_style, rows = odd_rows, cols = seq_len(ncol(df)), gridExpand = TRUE)
    if (length(even_rows) > 0) openxlsx::addStyle(wb, "Data", even_style, rows = even_rows, cols = seq_len(ncol(df)), gridExpand = TRUE)
  }
  openxlsx::setColWidths(wb, "Cover", cols = 1:2, widths = c(28, 60))
  openxlsx::setColWidths(wb, "Data", cols = seq_len(ncol(df)), widths = "auto")

  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
}

save_ggplot_by_format <- function(plot_obj, file, format, width, height, dpi, bg = "white") {
  fmt <- toupper(format %||% "PNG")
  if (fmt == "PDF") {
    ggplot2::ggsave(file, plot = plot_obj, width = width, height = height,
      device = grDevices::cairo_pdf, bg = bg)
  } else if (fmt == "SVG") {
    ggplot2::ggsave(file, plot = plot_obj, width = width, height = height,
      device = svglite::svglite, bg = bg)
  } else if (fmt == "TIFF") {
    ggplot2::ggsave(file, plot = plot_obj, width = width, height = height, dpi = dpi,
      device = "tiff", compression = "lzw", bg = bg)
  } else {
    ggplot2::ggsave(file, plot = plot_obj, width = width, height = height, dpi = dpi, device = "png", bg = bg)
  }
}

normalize_module_name <- function(x) {
  gsub("[^A-Za-z0-9]+", "", x)
}

make_registry_key <- function(module, trait) {
  paste0(normalize_module_name(module), "__", trait %||% "Global")
}

make_gge_static_plot <- function(gge_model, trait, cfg) {
  g <- gge_model[[trait]] %||% gge_model[[1]]
  gen_df <- data.frame(label = g$labelgen, PC1 = g$coordgen[, 1], PC2 = g$coordgen[, 2], type = "Genotype")
  env_df <- data.frame(label = g$labelenv, PC1 = g$coordenv[, 1], PC2 = g$coordenv[, 2], type = "Environment")
  base <- ggplot2::ggplot() +
    ggplot2::geom_point(data = gen_df, ggplot2::aes(PC1, PC2), color = cfg$geno_color, size = cfg$point_size, alpha = cfg$point_alpha) +
    ggplot2::geom_segment(data = env_df, ggplot2::aes(x = 0, y = 0, xend = PC1, yend = PC2),
      color = cfg$env_color, linewidth = cfg$line_width,
      arrow = ggplot2::arrow(length = grid::unit(cfg$arrow_size, "cm"))) +
    ggplot2::geom_point(data = env_df, ggplot2::aes(PC1, PC2), color = cfg$env_color, size = cfg$point_size + 0.7, alpha = cfg$point_alpha) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
    ggplot2::coord_equal() +
    ggplot2::labs(
      title = paste0("GGE Biplot — ", trait),
      x = paste0("PC1 (", round(g$varexpl[1], 1), "%)"),
      y = paste0("PC2 (", round(g$varexpl[2], 1), "%)")
    )

  base <- add_label_layer(base, gen_df, "PC1", "PC2", "label", cfg, cfg$geno_color)
  add_label_layer(base, env_df, "PC1", "PC2", "label", cfg, cfg$env_color)
}

AVAILABLE_ANALYSIS_TYPES <- c(
  "Exploratory Analysis — Boxplot",
  "Exploratory Analysis — Heatmap G×E",
  "Exploratory Analysis — Correlation Matrix",
  "Exploratory Analysis — Outlier Detection",
  "ANOVA — Variance Components",
  "ANOVA — BLUEs/BLUPs",
  "Stability — AMMI1 Biplot",
  "Stability — AMMI2 Biplot",
  "Stability — Variance Explained (IPCA)",
  "Stability — GGE Biplot",
  "Stability — Eberhart & Russell",
  "Stability — Wricke / Shukla",
  "Stability — Stability Ranking",
  "Enviromics — Environmental PCA",
  "Enviromics — Env Typology Heatmap",
  "Enviromics — Reaction Norms",
  "Enviromics — RDA Triplot",
  "Enviromics — Sensitivity Ranking",
  "Adaptation — Winner per Environment",
  "Adaptation — Mega-environment Map"
)

map_analysis_to_store_key <- function(analysis_type) {
  if (is.null(analysis_type)) return(NULL)
  
  if (startsWith(analysis_type, "Exploratory Analysis — Boxplot")) return("EDA_Boxplot")
  if (startsWith(analysis_type, "Exploratory Analysis — Heatmap G×E")) return("EDA_Heatmap")
  if (startsWith(analysis_type, "Exploratory Analysis — Correlation Matrix")) return("EDA_Correlation")
  if (startsWith(analysis_type, "Exploratory Analysis — Outlier Detection")) return("EDA_Outlier")
  if (startsWith(analysis_type, "ANOVA")) return("ANOVA")
  if (grepl("AMMI", analysis_type)) return("AMMI")
  if (grepl("GGE", analysis_type)) return("GGE")
  if (grepl("Eberhart", analysis_type)) return("Eberhart_Russell")
  if (grepl("Wricke", analysis_type)) return("Wricke")
  if (grepl("Shukla", analysis_type)) return("Shukla")
  if (grepl("Stability Ranking", analysis_type)) return("Combined_Stability")
  if (grepl("Environmental PCA", analysis_type)) return("Enviromics_PCA")
  if (grepl("Env Typology Heatmap", analysis_type)) return("Enviromics_Heatmap")
  if (grepl("Reaction Norms", analysis_type)) return("Reaction_Norms")
  if (grepl("RDA Triplot", analysis_type)) return("RDA_Triplot")
  if (grepl("Sensitivity Ranking", analysis_type)) return("Sensitivity_Ranking")
  if (grepl("Winner per Environment", analysis_type)) return("Mega_Env")
  if (grepl("Mega-environment Map", analysis_type)) return("Mega_Env")
  if (grepl("Spatial", analysis_type)) return("Spatial")
  
  return("Unknown")
}

generate_specific_plot <- function(analysis_type, trait, db, results_store, cfg = default_plot_cfg()) {
  if (is.null(analysis_type) || is.null(trait)) return(NULL)
  
  store_type <- map_analysis_to_store_key(analysis_type)
  key_trait <- if (store_type %in% c("Enviromics_PCA", "Enviromics_Heatmap")) "Global" else trait
  key <- make_results_key(store_type, key_trait)
  res <- results_store[[key]]
  
  if (is.null(res)) return(NULL)
  
  if (analysis_type == "Exploratory Analysis — Boxplot") {
    env_col <- res$env_col %||% db$env_col
    p <- ggplot2::ggplot(res$data, ggplot2::aes(x = .data[[env_col]], y = .data[[trait]], fill = .data[[env_col]])) +
      ggplot2::geom_boxplot(alpha = cfg$point_alpha, outlier.shape = NA) +
      ggplot2::geom_jitter(width = 0.15, size = cfg$point_size * 0.5, alpha = cfg$point_alpha * 0.6) +
      ggplot2::labs(title = paste0("EDA Boxplot — ", trait), x = "Environment", y = trait) +
      ggplot2::theme(legend.position = "none")
    return(p)
  }
  
  if (analysis_type == "Stability — AMMI1 Biplot") {
    ammi <- res
    gen_df <- ammi$gen_scores |> dplyr::rename(mean_val = gen_mean, ipca = IPCA1)
    env_df <- ammi$env_scores |> dplyr::rename(mean_val = env_mean, ipca = IPCA1)
    p <- ggplot2::ggplot() +
      ggplot2::geom_point(data = gen_df, ggplot2::aes(mean_val, ipca), color = cfg$geno_color, size = cfg$point_size, alpha = cfg$point_alpha) +
      ggplot2::geom_point(data = env_df, ggplot2::aes(mean_val, ipca), color = cfg$env_color, size = cfg$point_size + 0.5, alpha = cfg$point_alpha) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey60", linewidth = cfg$line_width) +
      ggplot2::geom_vline(xintercept = ammi$grand_mean, linetype = "dashed", color = "grey60", linewidth = cfg$line_width) +
      ggplot2::labs(title = paste0("AMMI1 — ", trait), x = paste0("Mean (", trait, ")"), y = "IPCA1")
    p <- add_label_layer(p, dplyr::transmute(gen_df, x = mean_val, y = ipca, label = GEN), "x", "y", "label", cfg, cfg$geno_color)
    p <- add_label_layer(p, dplyr::transmute(env_df, x = mean_val, y = ipca, label = ENV), "x", "y", "label", cfg, cfg$env_color)
    return(p)
  }
  
  if (analysis_type == "Stability — AMMI2 Biplot") {
    ammi <- res
    gen_df <- ammi$gen_scores |> dplyr::transmute(label = GEN, x = IPCA1, y = IPCA2)
    env_df <- ammi$env_scores |> dplyr::transmute(label = ENV, x = IPCA1, y = IPCA2)
    p <- ggplot2::ggplot() +
      ggplot2::geom_segment(data = env_df, ggplot2::aes(x = 0, y = 0, xend = x, yend = y),
        color = cfg$env_color, linewidth = cfg$line_width,
        arrow = ggplot2::arrow(length = grid::unit(cfg$arrow_size, "cm"))) +
      ggplot2::geom_point(data = gen_df, ggplot2::aes(x, y), color = cfg$geno_color, size = cfg$point_size, alpha = cfg$point_alpha) +
      ggplot2::geom_point(data = env_df, ggplot2::aes(x, y), color = cfg$env_color, size = cfg$point_size + 0.5, alpha = cfg$point_alpha) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
      ggplot2::coord_equal() +
      ggplot2::labs(title = paste0("AMMI2 — ", trait), x = "IPCA1", y = "IPCA2")
    p <- add_label_layer(p, gen_df, "x", "y", "label", cfg, cfg$geno_color)
    p <- add_label_layer(p, env_df, "x", "y", "label", cfg, cfg$env_color)
    return(p)
  }
  
  if (analysis_type == "Stability — Stability Ranking") {
    ammi <- res
    df <- ammi$stability
    p <- ggplot2::ggplot(df, ggplot2::aes(gen_mean, WAAS)) +
      ggplot2::geom_point(color = cfg$geno_color, size = cfg$point_size, alpha = cfg$point_alpha) +
      ggplot2::geom_hline(yintercept = mean(df$WAAS, na.rm = TRUE), linetype = "dashed", color = "grey60") +
      ggplot2::geom_vline(xintercept = mean(df$gen_mean, na.rm = TRUE), linetype = "dashed", color = "grey60") +
      ggplot2::labs(title = paste0("Stability Ranking — ", trait), x = paste0("Mean (", trait, ")"), y = "WAAS")
    p <- add_label_layer(p, dplyr::transmute(df, x = gen_mean, y = WAAS, label = GEN), "x", "y", "label", cfg, cfg$geno_color)
    return(p)
  }
  
  if (analysis_type == "Exploratory Analysis — Heatmap G×E") {
    ammi <- res
    df <- ammi$interaction_long
    p <- ggplot2::ggplot(df, ggplot2::aes(x = ENV, y = GEN, fill = interaction)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.2) +
      ggplot2::scale_fill_gradient2(low = cfg$neg_fill, mid = "white", high = cfg$pos_fill, midpoint = 0) +
      ggplot2::labs(title = paste0("GxE Interaction Heatmap — ", trait), x = "Environment", y = "Genotype")
    return(p)
  }
  
  if (analysis_type == "Stability — GGE Biplot") {
    return(make_gge_static_plot(res$model, trait, cfg))
  }
  
  if (analysis_type == "Enviromics — Reaction Norms") {
    fw_data <- res$fw_data
    slopes <- res$gen_slopes
    gen_col <- db$gen_col
    p <- ggplot2::ggplot(fw_data, ggplot2::aes(x = EI, y = mean_val, color = .data[[gen_col]])) +
      ggplot2::geom_point(size = cfg$point_size, alpha = cfg$point_alpha) +
      ggplot2::geom_abline(data = slopes, ggplot2::aes(intercept = intercept, slope = slope, color = .data[[gen_col]]),
        linewidth = cfg$line_width, alpha = 0.8, show.legend = FALSE) +
      ggplot2::labs(title = paste0("Reaction Norms — ", trait), x = "Environmental Index", y = paste0("Mean ", trait))
    return(p)
  }
  
  if (analysis_type == "Adaptation — Mega-environment Map") {
    df <- res$env_strat
    p <- ggplot2::ggplot(df, ggplot2::aes(x = ENV, y = MEAN, fill = MEGA_ENV)) +
      ggplot2::geom_col(alpha = cfg$point_alpha) +
      ggplot2::labs(title = paste0("Mega-Environments — ", trait), x = "Environment", y = "Winning Mean")
    return(p)
  }
  
  if (analysis_type == "Enviromics — Environmental PCA") {
    pca <- res$pca_res
    df <- as.data.frame(pca$x)
    df$label <- res$env_labels
    p <- ggplot2::ggplot(df, ggplot2::aes(PC1, PC2)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
      ggplot2::geom_point(color = cfg$env_color, size = cfg$point_size, alpha = cfg$point_alpha) +
      ggplot2::coord_equal() +
      ggplot2::labs(title = "Environmental PCA", x = "PC1", y = "PC2")
    p <- add_label_layer(p, df, "PC1", "PC2", "label", cfg, cfg$label_color)
    return(p)
  }
  
  # Stubs for missing builders (return NULL if uncomputed or unsupported)
  # - Exploratory Analysis — Correlation Matrix
  # - Exploratory Analysis — Outlier Detection
  # - ANOVA — Variance Components
  # - ANOVA — BLUEs/BLUPs
  # - Stability — Variance Explained (IPCA)
  # - Stability — Eberhart & Russell
  # - Stability — Wricke / Shukla
  # - Enviromics — Env Typology Heatmap
  # - Enviromics — RDA Triplot
  # - Enviromics — Sensitivity Ranking
  # - Adaptation — Winner per Environment
  return(NULL)
}
