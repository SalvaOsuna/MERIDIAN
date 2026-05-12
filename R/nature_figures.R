# =============================================================================
# MERIDIAN - Nature-style figure contract
# =============================================================================

meridian_nature_palette <- function() {
  c(
    neutral_dark = "#272727",
    neutral_mid = "#767676",
    neutral_light = "#D8D8D8",
    neutral_pale = "#F2F2F2",
    signal_blue = "#3182BD",
    signal_teal = "#33B5A5",
    accent_red = "#D24B40",
    accent_orange = "#E28E2C",
    heat_low = "#3B6EA8",
    heat_mid = "#F7F7F7",
    heat_high = "#D24B40"
  )
}

meridian_nature_color <- function(name) {
  pal <- meridian_nature_palette()
  unname(pal[[name]])
}

meridian_nature_discrete <- function(n) {
  base <- unname(meridian_nature_palette()[c(
    "signal_blue", "signal_teal", "accent_orange", "accent_red",
    "neutral_mid", "neutral_dark", "neutral_light"
  )])
  if (n <= length(base)) return(base[seq_len(n)])
  grDevices::colorRampPalette(base)(n)
}

theme_meridian_nature <- function(base_size = 6.5, base_family = "Arial",
                                  show_grid = FALSE) {
  ggplot2::theme_classic(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      axis.line = ggplot2::element_line(linewidth = 0.35, colour = "black"),
      axis.ticks = ggplot2::element_line(linewidth = 0.35, colour = "black"),
      axis.title = ggplot2::element_text(size = base_size),
      axis.text = ggplot2::element_text(size = base_size - 0.5),
      legend.title = ggplot2::element_text(size = base_size - 0.3),
      legend.text = ggplot2::element_text(size = base_size - 0.7),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = base_size - 0.3, face = "bold"),
      plot.title = ggplot2::element_text(size = base_size + 0.5, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = base_size - 0.2),
      plot.caption = ggplot2::element_text(size = base_size - 0.7),
      panel.grid.major = if (isTRUE(show_grid)) {
        ggplot2::element_line(linewidth = 0.2, colour = meridian_nature_color("neutral_light"))
      } else {
        ggplot2::element_blank()
      },
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA)
    )
}

theme_meridian_nature_map <- function(base_size = 6.5, base_family = "Arial") {
  theme_meridian_nature(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
}

scale_fill_meridian_discrete <- function(..., name = ggplot2::waiver()) {
  ggplot2::discrete_scale(
    aesthetics = "fill",
    scale_name = "meridian_nature",
    palette = meridian_nature_discrete,
    name = name,
    ...
  )
}

scale_color_meridian_discrete <- function(..., name = ggplot2::waiver()) {
  ggplot2::discrete_scale(
    aesthetics = "colour",
    scale_name = "meridian_nature",
    palette = meridian_nature_discrete,
    name = name,
    ...
  )
}

scale_fill_meridian_sequential <- function(..., name = ggplot2::waiver(),
                                           na.value = "#E6E6E6") {
  pal <- meridian_nature_palette()
  ggplot2::scale_fill_gradientn(
    colours = c("#EEF3F7", "#A9CFE2", pal[["signal_blue"]], "#264653"),
    name = name,
    na.value = na.value,
    ...
  )
}

scale_fill_meridian_diverging <- function(..., low = NULL, mid = NULL, high = NULL,
                                          midpoint = 0,
                                          limits = NULL,
                                          name = ggplot2::waiver(),
                                          na.value = "#E6E6E6") {
  pal <- meridian_nature_palette()
  ggplot2::scale_fill_gradient2(
    low = low %||% pal[["heat_low"]],
    mid = mid %||% pal[["heat_mid"]],
    high = high %||% pal[["heat_high"]],
    midpoint = midpoint,
    limits = limits,
    name = name,
    na.value = na.value,
    ...
  )
}

scale_color_meridian_diverging <- function(..., name = ggplot2::waiver()) {
  pal <- meridian_nature_palette()
  ggplot2::scale_color_gradient(
    low = pal[["signal_teal"]],
    high = pal[["accent_red"]],
    name = name,
    ...
  )
}

meridian_plotly_layout <- function(p, title = NULL, xaxis = list(), yaxis = list(),
                                   margin = list(l = 70, r = 20, b = 80, t = 55),
                                   legend = list(orientation = "h", y = 1.05, x = 0),
                                   ...) {
  pal <- meridian_nature_palette()
  args <- c(
    list(
      p = p,
      font = list(family = "Arial", size = 11, color = pal[["neutral_dark"]]),
      xaxis = utils::modifyList(
        list(showline = TRUE, linewidth = 1, linecolor = "black", zerolinecolor = pal[["neutral_light"]]),
        xaxis
      ),
      yaxis = utils::modifyList(
        list(showline = TRUE, linewidth = 1, linecolor = "black", zerolinecolor = pal[["neutral_light"]]),
        yaxis
      ),
      legend = legend,
      margin = margin,
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    ),
    list(...)
  )
  if (!is.null(title)) {
    args$title <- list(
      text = title,
      font = list(family = "Arial", size = 13, color = pal[["neutral_dark"]])
    )
  }
  do.call(plotly::layout, args)
}
