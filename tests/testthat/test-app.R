test_that("run_app is exported", {
  expect_true(is.function(run_app))
})

test_that("app_ui can be built", {
  ui <- app_ui(NULL)
  expect_s3_class(ui, "shiny.tag.list")
})

test_that("bundled package assets are discoverable", {
  expect_true(file.exists(app_sys("app/www/styles.css")))
  expect_true(file.exists(app_sys("extdata/example_phenotypic.csv")))
  expect_true(file.exists(app_sys("report_template.Rmd")))
})

test_that("compute_blues handles asymptotic or normal emmeans limits", {
  set.seed(123)
  df <- expand.grid(
    Genotype = paste0("G", 1:5),
    Environment = paste0("E", 1:3),
    Rep = paste0("R", 1:2)
  )
  df$Yield <- rnorm(nrow(df), mean = 10, sd = 1.5)
  
  blues_res <- compute_blues(df, "Genotype", "Environment", "Rep", "Yield")
  
  expect_true(is.data.frame(blues_res))
  expect_true(all(c("Genotype", "BLUE", "SE", "CI_lower", "CI_upper", "Rank") %in% names(blues_res)))
  expect_equal(nrow(blues_res), 5)
  expect_true(all(!is.na(blues_res$BLUE)))
  expect_true(all(!is.na(blues_res$CI_lower)))
  expect_true(all(!is.na(blues_res$CI_upper)))
})

test_that("plot_ge_heatmap handles R input and returns a valid interactive object", {
  set.seed(123)
  df <- expand.grid(
    Genotype = paste0("G", 1:5),
    Environment = paste0("E", 1:3)
  )
  df$Yield <- rnorm(nrow(df), mean = 10, sd = 1.5)
  
  p <- plot_ge_heatmap(df, "Genotype", "Environment", "Yield")
  
  expect_true(inherits(p, "htmlwidget"))
})

test_that("parse_date_robustly parses diverse formats correctly", {
  dates <- c("2026-05-21", "2026/05/21", "21/05/2026", "05/21/2026", "46163") # 46163 is 2026-05-21 in Excel serial
  parsed <- parse_date_robustly(dates)
  expect_equal(length(parsed), 5)
  expect_true(all(parsed == as.Date("2026-05-21")))
  
  p_ct <- parse_date_robustly(as.POSIXct("2026-05-21"))
  expect_equal(p_ct, as.Date("2026-05-21"))
})

test_that("calculate_enviromic_indices aggregates daily weather data correctly", {
  mock_daily <- data.frame(
    T2M_MAX = c(25, 27, 24),
    T2M_MIN = c(15, 17, 14),
    PRECTOTCORR = c(2, 5, 0),
    RH2M = c(60, 70, 50),
    ALLSKY_SFC_SW_DWN = c(18, 20, 15),
    stringsAsFactors = FALSE
  )
  
  indices <- calculate_enviromic_indices(mock_daily)
  expect_equal(nrow(indices), 1)
  expect_true(all(c("MeanTemp_C", "TotalRainfall_mm", "GDD", "MeanRH_pct", "TotalSolar_MJ") %in% names(indices)))
  expect_equal(indices$MeanTemp_C, 20.33)
  expect_equal(indices$TotalRainfall_mm, 7.00)
  expect_equal(indices$GDD, 31.00)
  expect_equal(indices$MeanRH_pct, 60.00)
  expect_equal(indices$TotalSolar_MJ, 53.00)
})

test_that("process_environmental_covariates handles validation and offline status", {
  expect_equal(process_environmental_covariates(NULL), NULL)
  
  bad_data <- data.frame(
    Environment = "E1"
  )
  expect_equal(process_environmental_covariates(bad_data), bad_data)
})
