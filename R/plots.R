# =============================================================================
# MERIDIAN — Plotting Functions
# Pure ggplot2/plotly functions for data visualization
# =============================================================================


#' Create boxplots for trait by grouping variable
#' @param df Data frame
#' @param trait Trait column name
#' @param group_col Column to group by (environment or genotype)
#' @param color_col Column to color by (optional, defaults to group_col)
#' @param show_points Logical, show jittered points
#' @return ggplot object
plot_boxplots <- function(df, trait, group_col, color_col = NULL, show_points = FALSE) {
  if (is.null(color_col)) color_col <- group_col

  p <- ggplot2::ggplot(df, ggplot2::aes(
    x    = reorder(.data[[group_col]], .data[[trait]], FUN = median, na.rm = TRUE),
    y    = .data[[trait]],
    fill = .data[[color_col]]
  )) +
    ggplot2::geom_boxplot(
      alpha    = 0.75,
      outlier.shape = if (show_points) NA else 19,
      outlier.alpha = 0.5
    ) +
    ggplot2::labs(
      x     = group_col,
      y     = trait,
      title = paste(trait, "by", group_col),
      fill  = color_col
    ) +
    scale_fill_meridian_discrete() +
    theme_meridian_nature() +
    ggplot2::theme(
      axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )

  if (show_points) {
    p <- p + ggplot2::geom_jitter(
      width = 0.2, alpha = 0.35, size = 0.9,
      ggplot2::aes(color = .data[[color_col]])
    ) +
      scale_color_meridian_discrete()
  }

  p
}


#' Create a genotype × environment heatmap of means
#' @param df Data frame
#' @param gen_col Genotype column
#' @param env_col Environment column
#' @param trait Trait column
#' @param cluster_rows Logical
#' @param cluster_cols Logical
#' @param palette Color palette name for heatmaply
#' @return heatmaply object (htmlwidget) or plotly object
plot_ge_heatmap <- function(df, gen_col, env_col, trait,
                            cluster_rows = TRUE, cluster_cols = TRUE,
                            palette = "meridian") {
  ge_matrix <- pivot_ge_means(df, gen_col, env_col, trait)
  plot_title <- paste0("GxE Means: ", trait)

  # Resolve palette: convert all names to actual color vectors
  viridis_palettes <- c("viridis", "inferno", "plasma", "magma", "cividis")
  if (tolower(palette) %in% c("meridian", "nature", "default")) {
    pal <- meridian_nature_palette()
    color_vec <- grDevices::colorRampPalette(c("#EEF3F7", "#A9CFE2", pal[["signal_blue"]], "#264653"))(256)
  } else if (tolower(palette) %in% viridis_palettes &&
      requireNamespace("viridisLite", quietly = TRUE)) {
    color_vec <- viridisLite::viridis(256, option = substr(tolower(palette), 1, 1))
  } else if (requireNamespace("RColorBrewer", quietly = TRUE)) {
    # RColorBrewer palettes: get max colors and interpolate to 256
    n_colors <- tryCatch(
      RColorBrewer::brewer.pal.info[palette, "maxcolors"],
      error = function(e) 11
    )
    color_vec <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(n_colors, palette))(256)
  } else {
    pal <- meridian_nature_palette()
    color_vec <- grDevices::colorRampPalette(c(pal[["heat_low"]], pal[["heat_mid"]], pal[["heat_high"]]))(256)
  }

  if (requireNamespace("heatmaply", quietly = TRUE)) {
    return(heatmaply::heatmaply(
      ge_matrix,
      colors = color_vec,
      Rowv = cluster_rows,
      Colv = cluster_cols,
      main = plot_title,
      margins = c(80, 120, 50, 50),
      label_names = c("Genotype", "Environment", "Value")
    ))
  }

  # Fallback to plotly if heatmaply is not installed
  mat <- as.matrix(ge_matrix)
  row_order <- seq_len(nrow(mat))
  col_order <- seq_len(ncol(mat))

  if (isTRUE(cluster_rows) && nrow(mat) > 1) {
    row_order <- tryCatch({
      dist_mat <- mat
      dist_mat[is.na(dist_mat)] <- mean(dist_mat, na.rm = TRUE)
      stats::hclust(stats::dist(dist_mat))$order
    }, error = function(e) seq_len(nrow(mat)))
  }

  if (isTRUE(cluster_cols) && ncol(mat) > 1) {
    col_order <- tryCatch({
      dist_mat <- mat
      dist_mat[is.na(dist_mat)] <- mean(dist_mat, na.rm = TRUE)
      stats::hclust(stats::dist(t(dist_mat)))$order
    }, error = function(e) seq_len(ncol(mat)))
  }

  mat_clustered <- mat[row_order, col_order, drop = FALSE]

  n_colors <- length(color_vec)
  colorscale_plotly <- lapply(seq_along(color_vec), function(i) {
    list((i - 1) / (n_colors - 1), color_vec[i])
  })

  plotly::plot_ly(
    x = colnames(mat_clustered),
    y = rownames(mat_clustered),
    z = mat_clustered,
    type = "heatmap",
    colorscale = colorscale_plotly,
    hovertemplate = "Genotype: %{y}<br>Environment: %{x}<br>Value: %{z:.2f}<extra></extra>"
  ) |>
    meridian_plotly_layout(
      title = plot_title,
      xaxis = list(title = "Environment", tickangle = -45),
      yaxis = list(title = "Genotype", autorange = "reversed"),
      margin = list(l = 120, r = 30, b = 100, t = 80)
    )
}


#' Create an environment correlation matrix plot
#' @param df Data frame
#' @param gen_col Genotype column
#' @param env_col Environment column
#' @param trait Trait column
#' @param method Correlation method: "pearson", "spearman", or "kendall"
#' @return plotly object
#' @export
plot_env_correlation <- function(df, gen_col, env_col, trait, method = "pearson") {
  ge_matrix <- pivot_ge_means(df, gen_col, env_col, trait)
  cor_matrix <- stats::cor(ge_matrix, use = "pairwise.complete.obs", method = method)
  cor_matrix <- round(cor_matrix, 2)

  # Fetch palette and colors
  pal <- meridian_palette("meridian")
  color_vec <- grDevices::colorRampPalette(c(pal[["heat_low"]], pal[["heat_mid"]], pal[["heat_high"]]))(256)
  n_colors <- length(color_vec)
  colorscale_plotly <- lapply(seq_along(color_vec), function(i) {
    list((i - 1) / (n_colors - 1), color_vec[i])
  })

  # Create plotly heatmap
  plotly::plot_ly(
    x = colnames(cor_matrix),
    y = rownames(cor_matrix),
    z = cor_matrix,
    type   = "heatmap",
    colorscale = colorscale_plotly,
    zmin = -1, zmax = 1,
    text = cor_matrix,
    hovertemplate = "Env1: %{y}<br>Env2: %{x}<br>r = %{z:.2f}<extra></extra>"
  ) |>
    meridian_plotly_layout(
      title = paste("Environment Correlations:", trait, "(", method, ")"),
      xaxis = list(title = "", tickangle = -45),
      yaxis = list(title = "", autorange = "reversed"),
      margin = list(l = 100, r = 30, b = 100, t = 80)
    )
}


#' Create an outlier detection scatter plot
#' @param df Data frame with 'is_outlier' column
#' @param trait Trait column name
#' @param group_col Group column (usually environment)
#' @param gen_col Genotype column (for labeling)
#' @return ggplot object
plot_outlier_scatter <- function(df, trait, group_col, gen_col) {
  # Add point size column based on outlier status
  df$point_size <- ifelse(df$is_outlier, 3, 1.5)

  # Build hover text
  df$hover_text <- paste0(
    gen_col, ": ", df[[gen_col]], "\n",
    group_col, ": ", df[[group_col]], "\n",
    trait, ": ", round(df[[trait]], 2)
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(
    x     = .data[[group_col]],
    y     = .data[[trait]],
    color = is_outlier,
    size  = point_size,
    text  = hover_text
  )) +
    ggplot2::geom_jitter(
      width = 0.2, alpha = 0.7
    ) +
    ggplot2::scale_size_identity() +
    ggplot2::scale_color_manual(
      values = c("FALSE" = meridian_nature_color("neutral_mid"), "TRUE" = meridian_nature_color("accent_red")),
      labels = c("Normal", "Outlier"),
      name   = "Status"
    ) +
    ggplot2::labs(
      x     = group_col,
      y     = trait,
      title = paste("Outlier Detection:", trait)
    ) +
    theme_meridian_nature() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  p
}
