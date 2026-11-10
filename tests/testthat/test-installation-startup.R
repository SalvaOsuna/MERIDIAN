test_that("the public launcher returns an app and renders its UI offline", {
  withr::local_envvar(c(
    http_proxy = "http://127.0.0.1:9",
    https_proxy = "http://127.0.0.1:9"
  ))
  app <- run_app()
  expect_s3_class(app, "shiny.appobj")
  rendered <- htmltools::renderTags(app_ui(NULL))
  expect_match(rendered$html, "MERIDIAN", fixed = TRUE)
  expect_match(rendered$head, "assets/styles.css", fixed = TRUE)
})

test_that("the bundled example loads with covariates without a weather request", {
  local_mocked_bindings(fetch_nasa_weather = function(...) stop("Unexpected weather request"))
  shiny::testServer(mod_data_upload_server, {
    load_example()
    session$setInputs(col_gen = "Genotype", col_env = "Environment", col_rep = "Rep",
      col_traits = "GrainYield", design_manual = "RCBD")
    bundle <- data_bundle()
    expect_true(bundle$validation$valid)
    expect_equal(nrow(bundle$data), 300L)
    expect_equal(nrow(bundle$env_data), 5L)
    expect_true(all(c("MeanTemp_C", "TotalRainfall_mm") %in% names(bundle$env_data)))
    expect_setequal(bundle$env_data$Environment, unique(bundle$data$Environment))
  })
})

test_that("Excel uploads use the original filename when the temporary path has no extension", {
  path <- tempfile()
  expected <- data.frame(Genotype = c("G1", "G2"), Yield = c(5, 8))
  openxlsx::write.xlsx(expected, path)
  shiny::testServer(mod_data_upload_server, {
    session$setInputs(file_upload = list(datapath = path, name = "trial.xlsx"), header = TRUE)
    expect_equal(source_data(), expected)
  })
})

test_that("weather retrieval failure preserves uploaded environmental covariates", {
  local_mocked_bindings(process_environmental_covariates = function(...) stop("Weather service offline"))
  expected <- read.csv(app_sys("extdata/example_envdata.csv"))
  path <- tempfile(fileext = ".csv")
  utils::write.csv(expected, path, row.names = FALSE)
  expected <- read_met_file(path)
  shiny::testServer(mod_data_upload_server, {
    session$setInputs(file_env_upload = list(datapath = path, name = "environments.csv"),
      csv_sep_env = ",", header = TRUE)
    expect_equal(processed_env_data(), expected)
  })
})
