test_that("spatial predictions retain identity and fixed-genotype mode is functional", {
  db <- example_bundle()
  # The bundled layout repeats genotype positions, confounding fixed genotype
  # effects with spatial trends. Use a randomized, identifiable layout in ENV2.
  set.seed(491)
  for (rep in unique(db$data$Rep)) {
    rows <- which(db$data$Environment == "ENV2" & db$data$Rep == rep)
    permutation <- sample(rows)
    db$data[rows, c("Row", "Col")] <- db$data[permutation, c("Row", "Col")]
  }
  data_result <- list(data_bundle = shiny::reactive(db))
  registry <- shiny::reactiveValues()
  shiny::testServer(mod_spatial_server, args = list(data_result = data_result, report_registry = registry), {
    invisible(capture.output(session$setInputs(trait = "GrainYield", env = "ENV1", fixed_terms = character(),
      random_terms = character(), nseg_row = 10, nseg_col = 6, gen_random = TRUE, run_spats = 1)))
    expect_equal(unique(adjusted_means()$Environment), "ENV1")
    expect_equal(nrow(adjusted_means()), 20L)
    session$setInputs(env = "ENV2")
    expect_null(spats_model())
    invisible(capture.output(session$setInputs(gen_random = FALSE, run_spats = 2)))
    expect_equal(unique(adjusted_means()$Environment), "ENV2")
    expect_false(spats_model()$model$geno$as.random)
    expect_false(is.null(output$spats_summary_boxes))
    session$setInputs(send_means_table_report = 1)
    saved <- get_report_table_items(registry)[[1]]$builder()
    session$setInputs(env = "ENV3")
    expect_equal(unique(saved$Environment), "ENV2")
    expect_null(spats_model())
  })
})
