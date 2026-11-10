test_that("validation rejects ambiguous units and nonfinite measurements", {
  d <- balanced_fixture()
  valid <- function(x) validate_met_data(x, "gen", "env", "rep", "y")$valid
  expect_true(valid(d))
  expect_false(valid(rbind(d, d[1, ])))
  bad <- d; bad$gen[1] <- NA; expect_false(valid(bad))
  bad <- d; bad$y[1] <- Inf; expect_false(valid(bad))
  bad <- d; bad$y <- as.character(bad$y); expect_false(valid(bad))
  bad <- d; bad$y[1] <- NA; expect_true(valid(bad))
})

test_that("signatures include values and snapshots are immutable", {
  db <- example_bundle(); changed <- db
  changed$data$GrainYield[1] <- changed$data$GrainYield[1] + 1
  expect_false(identical(make_dataset_signature(db), make_dataset_signature(changed)))
  registry <- shiny::reactiveValues(); value <- data.frame(x = 1)
  item <- register_report_table(registry, "test", "Test", "Test", "y", function() value,
    list(dataset_signature = make_dataset_signature(db)))
  value$x <- 2
  expect_equal(item$builder()$x, 1)
  expect_error(assert_report_item_dataset(item, make_dataset_signature(changed)), "previous dataset")
  p <- ggplot2::ggplot(data.frame(x = 1, y = 2), ggplot2::aes(x, y)) + ggplot2::geom_point()
  plot_item <- register_report_plot(registry, "plot", "Plot", "Test", "y", function() p)
  p <- NULL
  expect_s3_class(plot_item$builder(), "ggplot")
})

test_that("changing data or settings invalidates fitted reactive results", {
  server <- function(input, output, session) {
    result <- analysis_event(input$run, list(input$trait, input$value), input$value * 2)
  }
  shiny::testServer(server, {
    session$setInputs(trait = "a", value = 2)
    expect_null(result())
    session$setInputs(run = 1)
    expect_equal(as.numeric(result()), 4)
    session$setInputs(value = 3)
    expect_null(result())
    session$setInputs(run = 2)
    expect_equal(as.numeric(result()), 6)
    session$setInputs(trait = "b")
    expect_null(result())
  })
})

test_that("example data complete every statistical Shiny module", {
  db <- example_bundle()
  data_result <- list(data_bundle = shiny::reactive(db))
  shiny::testServer(mod_anova_server, args = list(data_result = data_result), {
    session$setInputs(trait = "GrainYield", direction = "higher", run_anova = 1)
    result <- anova_results()
    expect_false(is.null(result$anova)); expect_false(is.null(result$mixed))
    expect_equal(nrow(result$blues), 20L); expect_equal(nrow(result$blups), 20L)
    fitted_time <- attr(fitted_results(), "analysis_time")
    session$setInputs(direction = "lower")
    expect_equal(anova_results()$blups$BLUP, sort(result$blups$BLUP))
    expect_equal(attr(fitted_results(), "analysis_time"), fitted_time)
    session$setInputs(trait = "PlantHeight")
    expect_null(anova_results())
  })
  shiny::testServer(mod_stability_server, args = list(data_result = data_result), {
    session$setInputs(trait = "GrainYield", ammi_n_axes = 3, ammi1_axis = "IPCA1", ammi2_x = "1", ammi2_y = "2", run_stability = 1)
    result <- stab_results()
    for (name in c("ammi", "gge", "er", "wricke", "shukla", "all")) expect_false(is.null(result[[name]]), info = name)
    expect_equal(nrow(result$ammi$stability), 20L)
  })
  shiny::testServer(mod_adaptation_server, args = list(data_result = data_result), {
    session$setInputs(trait = "GrainYield", direction = "higher", run_adaptation = 1)
    result <- adapt_results()
    for (name in c("mega_env", "fw", "env_pca", "env_cor")) expect_false(is.null(result[[name]]), info = name)
  })
})
