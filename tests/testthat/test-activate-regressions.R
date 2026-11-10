test_that("spreadsheet errors do not turn otherwise numeric traits into text", {
  file <- tempfile(fileext = ".csv")
  writeLines(c("gen,env,rep,y,z,id,note", "G1,E1,1,12.5,4,001,normal",
    "G2,E1,1,#VALUE!,5,002,#VALUE!", "G1,E2,1,13,#DIV/0!,003,review",
    "G2,E2,1,14,6,004,normal"), file)
  d <- read_met_file(file)
  expect_equal(d$y, c(12.5, NA, 13, 14))
  expect_equal(d$z, c(4, 5, NA, 6))
  expect_equal(d$note[2], "#VALUE!")
  expect_equal(attr(d, "import_issues")$Row, c(2L, 3L))
  expect_equal(attr(d, "import_issues")$Column, c("y", "z"))
  expect_match(paste(validate_met_data(d, "gen", "env", "rep", c("y", "z"))$warnings,
    collapse = " "), "Spreadsheet error cells imported as missing")
  expect_match(readLines(file)[3], "#VALUE!", fixed = TRUE)
})

test_that("AMMI heatmaps use current effect tables and retain interaction values", {
  d <- balanced_fixture()
  a <- run_ammi(d, "gen", "env", "rep", "y")
  expect_false("gen_effect" %in% names(a$interaction_long))
  p <- plot_ge_heatmap_ammi(a)
  expect_equal(p$data$interaction, a$interaction_long$interaction)
  expect_equal(levels(p$data$GEN), as.character(a$gen_means$GEN[order(a$gen_means$gen_effect)]))
  expect_s3_class(plotly::plotly_build(plot_ge_heatmap_ammi(a, interactive = TRUE)), "plotly")
  file <- tempfile(fileext = ".svg")
  save_ggplot_by_format(p, file, "SVG", 7, 5, 150)
  expect_match(paste(readLines(file), collapse = ""), "<svg", fixed = TRUE)
})

test_that("winner tooltip carries literal values for singleton winner groups", {
  bundle <- example_bundle()
  shiny::testServer(mod_adaptation_server, args = list(data_result = list(data_bundle = shiny::reactive(bundle))), {
    session$setInputs(trait = "GrainYield", direction = "higher", run_adaptation = 1)
    widget <- jsonlite::fromJSON(output$plot_mega_env, simplifyVector = FALSE)
    traces <- widget$x$data
    expect_true(length(traces) > 0L)
    templates <- unlist(lapply(traces, `[[`, "hovertemplate"))
    expect_true(all(grepl("Winner:", templates, fixed = TRUE)))
    expect_false(any(grepl("%{text}", templates, fixed = TRUE)))
  })
})

test_that("large heatmaps thin labels while retaining all cells", {
  base <- balanced_fixture()
  d <- do.call(rbind, lapply(1:13, function(i) {
    x <- base; x$gen <- paste0(x$gen, "_", i); x$y <- x$y + i; x
  }))
  a <- run_ammi(d, "gen", "env", trait = "y")
  p <- plot_ge_heatmap_ammi(a)
  built <- ggplot2::ggplot_build(p)
  expect_equal(nrow(built$data[[1]]), 416L)
  expect_lte(length(built$layout$panel_params[[1]]$y$get_breaks()), 30L)
  expect_equal(length(unique(p$data$GEN)), 104L)
})

test_that("means heatmap can be registered and exported with incomplete cells", {
  bundle <- example_bundle()
  bundle$data$GrainYield[bundle$data$Genotype == "G01" & bundle$data$Environment == "Env1"] <- NA_real_
  registry <- shiny::reactiveValues()
  shiny::testServer(mod_eda_server, args = list(data_result = list(data_bundle = shiny::reactive(bundle)), report_registry = registry), {
    session$setInputs(heat_trait = "GrainYield", heat_cluster_rows = TRUE, heat_cluster_cols = TRUE,
      heat_palette = "meridian", box_trait = "GrainYield", box_group = "Environment", box_points = FALSE)
    p <- build_ge_heatmap_gg(bundle, "GrainYield")
    expect_s3_class(p, "ggplot")
    expect_equal(nrow(p$data), 100L)
    mat <- as.matrix(pivot_ge_means(bundle$data, bundle$gen_col, bundle$env_col, "GrainYield"))
    expect_equal(levels(p$data$Genotype), rownames(mat)[stats::hclust(stats::dist(mat))$order])
    file <- tempfile(fileext = ".svg")
    save_ggplot_by_format(p, file, "SVG", 7, 5, 150)
    expect_gt(file.info(file)$size, 100)
    session$setInputs(send_heatmap_report = 1)
    expect_true(length(shiny::reactiveValuesToList(registry)) > 0L)
  })
})
