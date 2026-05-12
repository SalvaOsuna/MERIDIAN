# =============================================================================
# AMMI Plotting Functions
# Uses native plotly for interactive mode (avoids ggrepel/ggplotly issues)
# Uses ggplot2 + ggrepel for static mode
# =============================================================================


# -- AMMI1 Biplot: Mean vs IPCA axis -----------------------------------------
# Classic biplot for evaluating stability vs performance

plot_ammi1 <- function(ammi, axis = 1, label_size = 2.1,
                       show_lines = TRUE, interactive = FALSE) {

  ipca_col <- paste0("IPCA", axis)
  pct      <- ammi$variance_explained$Percent[axis]
  trait    <- ammi$params$trait
  pal <- meridian_nature_palette()

  gen_df <- ammi$gen_scores %>%
    rename(mean_val = gen_mean, ipca = all_of(ipca_col)) %>%
    mutate(type = "Genotype")

  env_df <- ammi$env_scores %>%
    rename(mean_val = env_mean, ipca = all_of(ipca_col)) %>%
    mutate(type = "Environment")

  if (interactive) {
    # ---- Native plotly: no label overlap ----
    p <- plotly::plot_ly() |>
      plotly::add_trace(
        data = gen_df, x = ~mean_val, y = ~ipca, text = ~GEN,
        type = "scatter", mode = "markers+text",
        marker = list(color = pal[["signal_blue"]], size = 7, symbol = "circle"),
        textposition = "top center",
        textfont = list(size = 9, color = pal[["signal_blue"]], family = "Arial"),
        name = "Genotype",
        hovertemplate = "%{text}<br>Mean: %{x:.2f}<br>IPCA: %{y:.3f}<extra></extra>"
      ) |>
      plotly::add_trace(
        data = env_df, x = ~mean_val, y = ~ipca, text = ~ENV,
        type = "scatter", mode = "markers+text",
        marker = list(color = pal[["accent_orange"]], size = 9, symbol = "triangle-up"),
        textposition = "bottom center",
        textfont = list(size = 9, color = pal[["accent_orange"]], family = "Arial"),
        name = "Environment",
        hovertemplate = "%{text}<br>Mean: %{x:.2f}<br>IPCA: %{y:.3f}<extra></extra>"
      ) |>
      meridian_plotly_layout(
        title = paste0("AMMI1 Biplot - ", trait),
        xaxis = list(title = paste0("Mean (", trait, ")"),
                     zeroline = FALSE),
        yaxis = list(title = paste0("IPCA", axis, " (", pct, "% GxE variance)"),
                     zeroline = FALSE),
        legend = list(orientation = "h", y = 1.05, x = 0.3),
        shapes = list(
          list(type = "line", y0 = 0, y1 = 0, x0 = 0, x1 = 1,
               xref = "paper", line = list(dash = "dash", color = "grey60", width = 1)),
          list(type = "line", x0 = ammi$grand_mean, x1 = ammi$grand_mean,
               y0 = 0, y1 = 1, yref = "paper",
               line = list(dash = "dash", color = "grey60", width = 1))
        )
      )
    return(p)
  }

  # ---- Static: ggplot2 + ggrepel ----
  plot_df <- bind_rows(
    gen_df %>% select(label = GEN, mean_val, ipca, type),
    env_df %>% select(label = ENV, mean_val, ipca, type)
  )

  p <- ggplot(plot_df, aes(x = mean_val, y = ipca,
                           color = type, shape = type, label = label)) +
    geom_point(size = 3) +
    ggrepel::geom_text_repel(size = label_size, max.overlaps = 20,
                             show.legend = FALSE) +
    labs(
      title = paste0("AMMI1 Biplot - ", trait),
      x     = paste0("Mean (", trait, ")"),
      y     = paste0("IPCA", axis, " (", pct, "% GxE variance)"),
      color = NULL, shape = NULL
    ) +
    scale_color_manual(values = c("Genotype" = pal[["signal_blue"]], "Environment" = pal[["accent_orange"]])) +
    scale_shape_manual(values = c("Genotype" = 16, "Environment" = 17)) +
    theme_meridian_nature() +
    theme(legend.position = "top")

  if (show_lines) {
    p <- p +
      geom_hline(yintercept = 0, linetype = "dashed", color = pal[["neutral_mid"]], linewidth = 0.35) +
      geom_vline(xintercept = ammi$grand_mean, linetype = "dashed", color = pal[["neutral_mid"]], linewidth = 0.35)
  }

  p
}


# -- AMMI2 Biplot: IPCA1 vs IPCA2 -------------------------------------------
# Shows GxE interaction patterns between genotypes and environments

plot_ammi2 <- function(ammi, axis_x = 1, axis_y = 2, label_size = 2.1,
                       show_arrows = TRUE, interactive = FALSE) {

  ipcax <- paste0("IPCA", axis_x)
  ipcay <- paste0("IPCA", axis_y)
  pct_x <- ammi$variance_explained$Percent[axis_x]
  pct_y <- ammi$variance_explained$Percent[axis_y]
  trait <- ammi$params$trait
  pal <- meridian_nature_palette()

  gen_df <- ammi$gen_scores %>%
    select(label = GEN, x = all_of(ipcax), y = all_of(ipcay)) %>%
    mutate(type = "Genotype")

  env_df <- ammi$env_scores %>%
    select(label = ENV, x = all_of(ipcax), y = all_of(ipcay)) %>%
    mutate(type = "Environment")

  if (interactive) {
    p <- plotly::plot_ly() |>
      plotly::add_trace(
        data = gen_df, x = ~x, y = ~y, text = ~label,
        type = "scatter", mode = "markers+text",
        marker = list(color = pal[["signal_blue"]], size = 7, symbol = "circle"),
        textposition = "top center",
        textfont = list(size = 9, color = pal[["signal_blue"]], family = "Arial"),
        name = "Genotype",
        hovertemplate = "%{text}<br>PC%{x:.3f}, %{y:.3f}<extra></extra>"
      ) |>
      plotly::add_trace(
        data = env_df, x = ~x, y = ~y, text = ~label,
        type = "scatter", mode = "markers+text",
        marker = list(color = pal[["accent_orange"]], size = 9, symbol = "triangle-up"),
        textposition = "bottom center",
        textfont = list(size = 9, color = pal[["accent_orange"]], family = "Arial"),
        name = "Environment",
        hovertemplate = "%{text}<br>PC%{x:.3f}, %{y:.3f}<extra></extra>"
      )

    if (show_arrows) {
      for (i in seq_len(nrow(env_df))) {
        p <- p |> plotly::add_annotations(
          x = env_df$x[i], y = env_df$y[i],
          ax = 0, ay = 0, axref = "x", ayref = "y",
          text = "", showarrow = TRUE,
          arrowhead = 2, arrowsize = 1, arrowwidth = 1.5,
          arrowcolor = paste0(pal[["accent_orange"]], "80")
        )
      }
    }

    p <- p |>
      meridian_plotly_layout(
        title = paste0("AMMI2 Biplot - ", trait),
        xaxis = list(title = paste0("IPCA", axis_x, " (", pct_x, "%)"),
                     zeroline = TRUE, zerolinecolor = "grey70",
                     zerolinewidth = 1),
        yaxis = list(title = paste0("IPCA", axis_y, " (", pct_y, "%)"),
                     zeroline = TRUE, zerolinecolor = "grey70",
                     zerolinewidth = 1),
        legend = list(orientation = "h", y = 1.05, x = 0.3)
      )
    return(p)
  }

  # ---- Static: ggplot2 + ggrepel ----
  p <- ggplot() +
    { if (show_arrows)
      geom_segment(data = env_df,
                   aes(x = 0, y = 0, xend = x, yend = y),
                   arrow = arrow(length = unit(0.2, "cm")),
                   color = pal[["accent_orange"]], alpha = 0.65, linewidth = 0.35)
    } +
    geom_point(data = gen_df, aes(x = x, y = y),
               color = pal[["signal_blue"]], size = 1.8) +
    ggrepel::geom_text_repel(data = gen_df, aes(x = x, y = y, label = label),
                  color = pal[["signal_blue"]], size = label_size, max.overlaps = 20) +
    ggrepel::geom_text_repel(data = env_df, aes(x = x, y = y, label = label),
                  color = pal[["accent_orange"]], size = label_size,
                  fontface = "bold", max.overlaps = 20) +
    geom_hline(yintercept = 0, linetype = "dashed", color = pal[["neutral_mid"]], linewidth = 0.35) +
    geom_vline(xintercept = 0, linetype = "dashed", color = pal[["neutral_mid"]], linewidth = 0.35) +
    labs(
      title = paste0("AMMI2 Biplot - ", trait),
      x     = paste0("IPCA", axis_x, " (", pct_x, "%)"),
      y     = paste0("IPCA", axis_y, " (", pct_y, "%)")
    ) +
    theme_meridian_nature()

  p
}


# -- Variance Explained per IPCA axis -----------------------------------------

plot_ammi_variance <- function(ammi, n_show = NULL) {

  df <- ammi$variance_explained
  if (!is.null(n_show)) df <- df[1:min(n_show, nrow(df)), ]
  pal <- meridian_nature_palette()

  ggplot(df, aes(x = reorder(Axis, -Percent), y = Percent)) +
    geom_col(fill = pal[["signal_blue"]], alpha = 0.9) +
    geom_line(aes(y = Cumulative, group = 1), color = pal[["accent_red"]], linewidth = 0.45) +
    geom_point(aes(y = Cumulative), color = pal[["accent_red"]], size = 1.4) +
    geom_text(aes(label = paste0(Percent, "%")), vjust = -0.5, size = 2.1) +
    scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 20)) +
    labs(
      title = "GxE Variance Explained by IPCA Axis",
      x = "IPCA Axis", y = "Variance Explained (%)"
    ) +
    theme_meridian_nature()
}


# -- GxE Interaction Heatmap --------------------------------------------------

plot_ge_heatmap_ammi <- function(ammi, interactive = FALSE) {

  df <- ammi$interaction_long %>%
    mutate(GEN = reorder(GEN, -gen_effect),
           ENV = reorder(ENV, -env_effect))

  p <- ggplot(df, aes(x = ENV, y = GEN, fill = interaction)) +
    geom_tile(color = "white", linewidth = 0.2) +
    scale_fill_meridian_diverging(midpoint = 0, name = "GxE\ninteraction") +
    geom_text(aes(label = round(interaction, 1)), size = 1.8) +
    labs(
      title = "GxE Interaction (residuals from additive model)",
      x = "Environment", y = "Genotype"
    ) +
    theme_meridian_nature() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  if (interactive) plotly::ggplotly(p) else p
}


# -- Stability Ranking Plot ---------------------------------------------------

plot_stability_ranking <- function(ammi, metric = "WAAS", n_top = NULL,
                                   interactive = FALSE) {

  df    <- ammi$stability
  trait <- ammi$params$trait
  if (!is.null(n_top)) df <- df %>% slice_min(order_by = .data[[metric]], n = n_top)
  pal <- meridian_nature_palette()

  if (interactive) {
    plotly::plot_ly(
      data = df, x = ~gen_mean, y = ~get(metric), text = ~GEN,
      type = "scatter", mode = "markers+text",
      marker = list(
        size = 10,
        color = df[[metric]],
        colorscale = list(c(0, pal[["signal_teal"]]), c(1, pal[["accent_red"]])),
        showscale = TRUE,
        colorbar = list(title = metric)
      ),
      textposition = "top center",
      textfont = list(size = 9),
      hovertemplate = paste0(
        "%{text}<br>Mean: %{x:.2f}<br>", metric, ": %{y:.3f}<extra></extra>"
      )
    ) |>
      meridian_plotly_layout(
        title = paste0("Mean vs Stability (", metric, ") - ", trait),
        xaxis = list(title = paste0("Mean (", trait, ")")),
        yaxis = list(title = paste0(metric, " (lower = more stable)")),
        shapes = list(
          list(type = "line", y0 = mean(df[[metric]]), y1 = mean(df[[metric]]),
               x0 = 0, x1 = 1, xref = "paper",
               line = list(dash = "dash", color = pal[["neutral_mid"]], width = 1)),
          list(type = "line", x0 = mean(df$gen_mean), x1 = mean(df$gen_mean),
               y0 = 0, y1 = 1, yref = "paper",
               line = list(dash = "dash", color = pal[["neutral_mid"]], width = 1))
        ),
        legend = list(orientation = "h", y = 1.05, x = 0)
      )
  } else {
    ggplot(df, aes(x = gen_mean, y = .data[[metric]], label = GEN)) +
      geom_point(aes(color = .data[[metric]]), size = 1.8) +
      scale_color_meridian_diverging(name = metric) +
      ggrepel::geom_text_repel(size = 2.1, max.overlaps = 20) +
      geom_hline(yintercept = mean(df[[metric]]),
                 linetype = "dashed", color = pal[["neutral_mid"]], linewidth = 0.35) +
      geom_vline(xintercept = mean(df$gen_mean),
                 linetype = "dashed", color = pal[["neutral_mid"]], linewidth = 0.35) +
      labs(
        title = paste0("Mean vs Stability (", metric, ") - ", trait),
        x     = paste0("Mean (", trait, ")"),
        y     = paste0(metric, " (lower = more stable)")
      ) +
      theme_meridian_nature()
  }
}


# =============================================================================
# Custom GGE Biplot (Yan & Tinker 2006)
# Each biplot type has distinct geometric overlays
# Type 1: Basic biplot
# Type 2: Mean vs Stability (AEC axis + perpendicular)
# Type 3: Which-Won-Where (convex hull + sector lines)
# Type 4: Discriminativeness & Representativeness (circles + AEC)
# Type 6: Ranking Genotypes (AEC + ideal genotype)
# Type 8: Ranking Environments (AEC + ideal environment)
# =============================================================================

plot_gge_custom <- function(gge_model, trait, biplot_type = 2) {

  gge_data <- gge_model[[trait]]
  if (is.null(gge_data)) gge_data <- gge_model[[1]]

  gen_coords <- as.data.frame(gge_data$coordgen)
  env_coords <- as.data.frame(gge_data$coordenv)
  varexpl    <- gge_data$varexpl
  pal <- meridian_nature_palette()

  gen_df <- data.frame(label = gge_data$labelgen,
    PC1 = gen_coords[, 1], PC2 = gen_coords[, 2], stringsAsFactors = FALSE)
  env_df <- data.frame(label = gge_data$labelenv,
    PC1 = env_coords[, 1], PC2 = env_coords[, 2], stringsAsFactors = FALSE)

  type_labels <- c("1" = "Basic GGE Biplot", "2" = "Mean vs Stability",
    "3" = "Which-Won-Where", "4" = "Discriminativeness & Representativeness",
    "6" = "Ranking Genotypes", "8" = "Ranking Environments")
  title <- type_labels[as.character(biplot_type)]
  if (is.na(title)) title <- "GGE Biplot"

  # ---- Base traces: genotypes + environments + arrows ----
  p <- plotly::plot_ly() |>
    plotly::add_trace(data = gen_df, x = ~PC1, y = ~PC2, text = ~label,
      type = "scatter", mode = "markers+text",
      marker = list(color = pal[["signal_blue"]], size = 7, symbol = "circle"),
      textposition = "top center", textfont = list(size = 9, color = pal[["signal_blue"]], family = "Arial"),
      name = "Genotype",
      hovertemplate = "%{text}<br>PC1: %{x:.3f}<br>PC2: %{y:.3f}<extra></extra>") |>
    plotly::add_trace(data = env_df, x = ~PC1, y = ~PC2, text = ~label,
      type = "scatter", mode = "markers+text",
      marker = list(color = pal[["accent_orange"]], size = 9, symbol = "triangle-up"),
      textposition = "bottom center",
      textfont = list(size = 9, color = pal[["accent_orange"]], family = "Arial"),
      name = "Environment",
      hovertemplate = "%{text}<br>PC1: %{x:.3f}<br>PC2: %{y:.3f}<extra></extra>")

  # Environment arrows
  for (i in seq_len(nrow(env_df))) {
    p <- p |> plotly::add_annotations(x = env_df$PC1[i], y = env_df$PC2[i],
      ax = 0, ay = 0, axref = "x", ayref = "y", text = "", showarrow = TRUE,
      arrowhead = 2, arrowsize = 1, arrowwidth = 1.2, arrowcolor = paste0(pal[["accent_orange"]], "80"))
  }

  # ---- Geometry helpers ----
  all_x <- c(gen_df$PC1, env_df$PC1, 0)
  all_y <- c(gen_df$PC2, env_df$PC2, 0)
  x_pad <- (max(all_x) - min(all_x)) * 0.12
  y_pad <- (max(all_y) - min(all_y)) * 0.12
  if (!is.finite(x_pad) || x_pad <= 0) x_pad <- 0.5
  if (!is.finite(y_pad) || y_pad <= 0) y_pad <- 0.5
  x_lim <- c(min(all_x) - x_pad, max(all_x) + x_pad)
  y_lim <- c(min(all_y) - y_pad, max(all_y) + y_pad)

  # AEC (Average Environment Coordinate)
  ae_x <- mean(env_df$PC1); ae_y <- mean(env_df$PC2)
  ae_len <- sqrt(ae_x^2 + ae_y^2)
  ae_ux <- ae_x / max(ae_len, 1e-9)
  ae_uy <- ae_y / max(ae_len, 1e-9)
  ext <- max(abs(c(all_x, all_y))) * 2

  shapes <- list()

  # ================================================================
  # Type 2, 6, 8: AEC abscissa (green dash) + ordinate (orange dot)
  # ================================================================
  if (biplot_type %in% c(2, 6, 8)) {
    # AEC abscissa: mean performance axis
    shapes <- c(shapes, list(list(type = "line",
      x0 = -ae_ux * ext, y0 = -ae_uy * ext,
      x1 = ae_ux * ext, y1 = ae_uy * ext,
      line = list(color = pal[["signal_teal"]], width = 1.2, dash = "dash"))))
    # AEC ordinate: stability axis (perpendicular)
    shapes <- c(shapes, list(list(type = "line",
      x0 = ae_uy * ext, y0 = -ae_ux * ext,
      x1 = -ae_uy * ext, y1 = ae_ux * ext,
      line = list(color = pal[["accent_orange"]], width = 1, dash = "dot"))))
    # AEC marker
    p <- p |> plotly::add_trace(x = ae_x, y = ae_y, type = "scatter",
      mode = "markers", marker = list(color = pal[["signal_teal"]], size = 9,
      symbol = "circle-open", line = list(width = 1.5, color = pal[["signal_teal"]])),
      name = "Avg. Env.", showlegend = TRUE,
      hovertemplate = "AEC<br>PC1: %{x:.3f}<br>PC2: %{y:.3f}<extra></extra>")
  }

  # ================================================================
  # Type 6: Ideal genotype (green star)
  # ================================================================
  if (biplot_type == 6) {
    proj <- gen_df$PC1 * ae_ux + gen_df$PC2 * ae_uy
    ideal_x <- max(proj) * ae_ux; ideal_y <- max(proj) * ae_uy
    p <- p |> plotly::add_trace(x = ideal_x, y = ideal_y, type = "scatter",
      mode = "markers", marker = list(color = pal[["signal_teal"]], size = 12,
      symbol = "star", line = list(width = 1, color = pal[["neutral_dark"]])),
      name = "Ideal Genotype", showlegend = TRUE,
      hovertemplate = "Ideal Genotype<extra></extra>")
  }

  # ================================================================
  # Type 8: Ideal environment (orange star)
  # ================================================================
  if (biplot_type == 8) {
    env_proj <- env_df$PC1 * ae_ux + env_df$PC2 * ae_uy
    ideal_x <- max(env_proj) * ae_ux; ideal_y <- max(env_proj) * ae_uy
    p <- p |> plotly::add_trace(x = ideal_x, y = ideal_y, type = "scatter",
      mode = "markers", marker = list(color = pal[["accent_orange"]], size = 12,
      symbol = "star", line = list(width = 1, color = pal[["neutral_dark"]])),
      name = "Ideal Environment", showlegend = TRUE,
      hovertemplate = "Ideal Environment<extra></extra>")
  }

  # ================================================================
  # Type 3: Which-Won-Where (convex hull + sector dividers)
  # ================================================================
  if (biplot_type == 3) {
    hull_idx <- chull(gen_df$PC1, gen_df$PC2)
    hull_idx <- c(hull_idx, hull_idx[1])
    hull_x <- gen_df$PC1[hull_idx]; hull_y <- gen_df$PC2[hull_idx]
    # Convex hull polygon
    p <- p |> plotly::add_trace(x = hull_x, y = hull_y, type = "scatter",
      mode = "lines", line = list(color = pal[["signal_blue"]], width = 1.2),
      fill = "toself", fillcolor = "rgba(49,130,189,0.07)",
      name = "Convex Hull", showlegend = FALSE, hoverinfo = "skip")
    # Sector lines: perpendicular to each hull edge, from origin
    for (k in seq_len(length(hull_idx) - 1)) {
      dx <- hull_x[k + 1] - hull_x[k]; dy <- hull_y[k + 1] - hull_y[k]
      px <- -dy; py <- dx
      plen <- sqrt(px^2 + py^2)
      if (plen < 1e-9) next
      px <- px / plen; py <- py / plen
      shapes <- c(shapes, list(list(type = "line", x0 = 0, y0 = 0,
        x1 = px * ext * 1.5, y1 = py * ext * 1.5,
        line = list(color = pal[["neutral_mid"]], width = 0.8, dash = "dot"))))
    }
  }

  # ================================================================
  # Type 4: Discriminativeness & Representativeness (circles + AEC)
  # ================================================================
  if (biplot_type == 4) {
    # AEC line
    shapes <- c(shapes, list(list(type = "line",
      x0 = -ae_ux * ext, y0 = -ae_uy * ext,
      x1 = ae_ux * ext, y1 = ae_uy * ext,
      line = list(color = pal[["signal_teal"]], width = 1.2, dash = "dash"))))
    # Concentric circles showing discriminating power
    env_dists <- sqrt(env_df$PC1^2 + env_df$PC2^2)
    radii <- seq(min(env_dists) * 0.5, max(env_dists) * 1.1, length.out = 4)
    for (r in radii) {
      theta <- seq(0, 2 * pi, length.out = 60)
      p <- p |> plotly::add_trace(x = r * cos(theta), y = r * sin(theta),
        type = "scatter", mode = "lines",
        line = list(color = "grey75", width = 0.7, dash = "dot"),
        showlegend = FALSE, hoverinfo = "skip")
    }
    # AEC marker
    p <- p |> plotly::add_trace(x = ae_x, y = ae_y, type = "scatter",
      mode = "markers", marker = list(color = pal[["signal_teal"]], size = 9,
      symbol = "circle-open", line = list(width = 1.5, color = pal[["signal_teal"]])),
      name = "Avg. Env.", showlegend = TRUE,
      hovertemplate = "AEC<extra></extra>")
  }

  # ---- Final layout ----
  p <- p |> meridian_plotly_layout(
    title = paste0(title, " - ", trait),
    xaxis = list(title = paste0("PC1 (", round(varexpl[1], 1), "%)"),
      zeroline = TRUE, zerolinecolor = "grey70", zerolinewidth = 1,
      range = x_lim),
    yaxis = list(title = paste0("PC2 (", round(varexpl[2], 1), "%)"),
      zeroline = TRUE, zerolinecolor = "grey70", zerolinewidth = 1,
      range = y_lim,
      scaleanchor = "x",
      scaleratio = 1),
    legend = list(orientation = "h", y = 1.05, x = 0.3),
    shapes = shapes)

  p
}
