# =============================================================================
# MERIDIAN — Patchwork Figure Composer Helpers
# =============================================================================

parse_num_ratio <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) return(NULL)
  parts <- trimws(unlist(strsplit(x, ",")))
  vals <- suppressWarnings(as.numeric(parts))
  vals <- vals[!is.na(vals) & vals > 0]
  if (length(vals) == 0) NULL else vals
}

map_tag_levels <- function(style) {
  switch(
    style %||% "A/B/C",
    "A/B/C" = "A",
    "a/b/c" = "a",
    "1/2/3" = "1",
    "(A)/(B)/(C)" = list(c("(", "A", ")")),
    "(a)/(b)/(c)" = list(c("(", "a", ")")),
    "A"
  )
}

compose_patchwork_figure <- function(plot_list, selected_ids, cfg) {
  selected_ids <- selected_ids[selected_ids %in% names(plot_list)]
  if (length(selected_ids) < 2) stop("Select at least two plots for composition.")

  plots <- lapply(selected_ids, function(id) plot_list[[id]])
  widths <- parse_num_ratio(cfg$width_ratios)
  heights <- parse_num_ratio(cfg$height_ratios)

  op <- cfg$operator %||% "+"
  ncol <- max(1, as.integer(cfg$ncol %||% 2))

  composed <- if (op == "|") {
    patchwork::wrap_plots(plots, nrow = 1)
  } else if (op == "/") {
    patchwork::wrap_plots(plots, ncol = 1)
  } else {
    patchwork::wrap_plots(plots, ncol = ncol)
  }

  composed <- composed + patchwork::plot_layout(
    ncol = ncol,
    widths = widths,
    heights = heights,
    guides = if (isTRUE(cfg$collect_guides)) "collect" else "keep"
  )

  if (isTRUE(cfg$auto_labels)) {
    tag_levels <- map_tag_levels(cfg$label_style)
    tag_pos <- if ((cfg$label_position %||% "top-left") == "top-right") c(1, 1) else c(0, 1)
    composed <- composed + patchwork::plot_annotation(tag_levels = tag_levels) &
      ggplot2::theme(
        plot.tag = ggplot2::element_text(
          size = cfg$label_size %||% 12,
          face = cfg$label_face %||% "bold",
          colour = cfg$label_color %||% "#111111"
        ),
        plot.tag.position = tag_pos
      )
  }

  composed <- composed + patchwork::plot_annotation(
    title = cfg$title %||% NULL,
    subtitle = cfg$subtitle %||% NULL,
    caption = cfg$caption %||% NULL
  )

  if (isTRUE(cfg$shared_theme)) {
    composed <- composed &
      ggplot2::theme_minimal(base_family = cfg$shared_font_family %||% "serif",
                             base_size = cfg$shared_base_size %||% 11)
  }

  bg_color <- cfg$bg_color %||% "white"
  composed <- composed &
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = bg_color, color = NA),
                   panel.background = ggplot2::element_rect(fill = bg_color, color = NA))

  composed
}

check_composer_warnings <- function(plot_objs) {
  if (length(plot_objs) <= 1) return(character(0))
  has_fixed <- vapply(plot_objs, function(p) inherits(p$coordinates, "CoordFixed"), logical(1))
  warns <- character(0)
  if (any(has_fixed) && !all(has_fixed)) {
    warns <- c(warns, "Some selected panels use fixed aspect ratio while others do not. Layout may look uneven.")
  }
  warns
}

export_panels_zip <- function(plot_list, selected_ids, cfg, zip_file) {
  dir.create(dirname(zip_file), recursive = TRUE, showWarnings = FALSE)
  tmp_dir <- file.path(tempdir(), paste0("meridian_panels_", as.integer(Sys.time())))
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

  ext <- tolower(cfg$format %||% "png")
  files <- c()
  for (id in selected_ids) {
    if (!id %in% names(plot_list)) next
    safe_id <- gsub("[^A-Za-z0-9_\\-]+", "_", id)
    f <- file.path(tmp_dir, paste0(safe_id, ".", ext))
    save_ggplot_by_format(
      plot_obj = plot_list[[id]],
      file = f,
      format = cfg$format,
      width = cfg$width,
      height = cfg$height,
      dpi = cfg$dpi
    )
    files <- c(files, f)
  }
  if (length(files) == 0) stop("No panel files generated.")
  zip::zipr(zip_file, files = files, recurse = FALSE)
}
