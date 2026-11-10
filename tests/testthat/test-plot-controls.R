test_that("plot styling and label controls affect exports without mutating snapshots", {
  p <- ggplot2::ggplot(data.frame(GEN = c("A", "B"), x = 1:2, y = 3:4), ggplot2::aes(x, y)) +
    ggplot2::geom_point(size = 1) + ggrepel::geom_text_repel(ggplot2::aes(label = GEN))
  cfg <- default_plot_cfg(); cfg$point_size <- 5; cfg$geno_color <- "#FF0000"; cfg$exclude_labels <- "A"
  changed <- customize_registered_plot(p, cfg)
  built <- ggplot2::ggplot_build(changed)
  expect_equal(built$data[[1]]$size, c(5, 5))
  expect_equal(built$data[[1]]$colour, rep("#FF0000", 2))
  expect_equal(built$data[[2]]$label, "B")
  expect_equal(p$layers[[1]]$aes_params$size, 1)
  cfg$show_labels <- FALSE
  expect_length(customize_registered_plot(p, cfg)$layers, 1L)
})
