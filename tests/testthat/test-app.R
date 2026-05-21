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

test_that("calculate_enviromic_indices supports growth-stage partitioning", {
  mock_daily <- data.frame(
    T2M_MAX = c(25, 27, 24, 28, 31, 29),
    T2M_MIN = c(15, 17, 14, 18, 21, 19),
    PRECTOTCORR = c(2, 5, 0, 10, 1, 0),
    RH2M = c(60, 70, 50, 65, 80, 75),
    ALLSKY_SFC_SW_DWN = c(18, 20, 15, 22, 19, 21),
    stringsAsFactors = FALSE
  )
  
  # flowering at Day 2, maturity at Day 4 (total length 6 days)
  indices <- calculate_enviromic_indices(mock_daily, dtf_mean = 2, dtm_mean = 4)
  
  # Standard variables
  expect_true(all(c("MeanTemp_C", "TotalRainfall_mm", "GDD", "MeanRH_pct", "TotalSolar_MJ") %in% names(indices)))
  
  # Vegetative phase variables: Days 1 to 2
  expect_true(all(c("GDD_Veg", "TotalRainfall_Veg", "MeanTemp_Veg") %in% names(indices)))
  # GDD_Veg: Day 1 (avg 20 -> GDD = 10) + Day 2 (avg 22 -> GDD = 12) = 22
  expect_equal(indices$GDD_Veg, 22.00)
  expect_equal(indices$TotalRainfall_Veg, 7.00) # 2 + 5
  expect_equal(indices$MeanTemp_Veg, 21.00) # (20 + 22) / 2
  
  # Reproductive phase variables: Days 3 to 4
  expect_true(all(c("GDD_Rep", "TotalRainfall_Rep", "MeanTemp_Rep", "HeatStressDays_Rep") %in% names(indices)))
  # GDD_Rep: Day 3 (avg 19 -> GDD = 9) + Day 4 (avg 23 -> GDD = 13) = 22
  expect_equal(indices$GDD_Rep, 22.00)
  expect_equal(indices$TotalRainfall_Rep, 10.00) # 0 + 10
  expect_equal(indices$MeanTemp_Rep, 21.00) # (19 + 23) / 2
  # HeatStressDays_Rep: T2M_MAX > 30. Day 3 (24), Day 4 (28) -> 0 days
  expect_equal(indices$HeatStressDays_Rep, 0)
  
  # Late phase variables: Days 5 to 6
  expect_true(all(c("GDD_Late", "TotalRainfall_Late", "MeanTemp_Late") %in% names(indices)))
  # GDD_Late: Day 5 (avg 26 -> GDD = 16) + Day 6 (avg 24 -> GDD = 14) = 30
  expect_equal(indices$GDD_Late, 30.00)
  expect_equal(indices$TotalRainfall_Late, 1.00) # 1 + 0
  expect_equal(indices$MeanTemp_Late, 25.00) # (26 + 24) / 2
})

test_that("read_met_file correctly handles CSV files", {
  tmp_csv <- tempfile(fileext = ".csv")
  write.csv(data.frame(A = 1:3, B = 4:6), tmp_csv, row.names = FALSE)
  
  df_csv <- read_met_file(tmp_csv)
  expect_equal(nrow(df_csv), 3)
  expect_equal(names(df_csv), c("A", "B"))
  unlink(tmp_csv)
})

test_that("read_met_file on Excel files behaves gracefully", {
  # When readxl is available, a nonexistent file should throw a file-not-found error,
  # not a missing package error.
  expect_error(read_met_file("nonexistent.xlsx"), "does not exist")
})

test_that("validate_environmental_data handles missing HarvestDate values gracefully", {
  df <- data.frame(
    Environment = c("E1", "E2"),
    Latitude = c(52.1, 52.2),
    Longitude = c(-106.6, -106.7),
    PlantingDate = c("2025-05-10", "2025-05-12"),
    HarvestDate = c("2025-09-10", NA),
    stringsAsFactors = FALSE
  )
  
  res <- validate_environmental_data(df)
  expect_true(res$valid)
  expect_equal(length(res$errors), 0)
  expect_true(any(grepl("missing HarvestDate", res$warnings)))
})

test_that("validate_environmental_data matches Site and Loc columns robustly", {
  df_site <- data.frame(
    Site = c("S1", "S2"),
    Latitude = c(52.1, 52.2),
    Longitude = c(-106.6, -106.7),
    PlantingDate = c("2025-05-10", "2025-05-12"),
    HarvestDate = c("2025-09-10", "2025-09-12"),
    stringsAsFactors = FALSE
  )
  res_site <- validate_environmental_data(df_site)
  expect_true(res_site$valid)
  expect_equal(length(res_site$errors), 0)

  df_loc <- data.frame(
    Loc = c("L1", "L2"),
    Latitude = c(52.1, 52.2),
    Longitude = c(-106.6, -106.7),
    PlantingDate = c("2025-05-10", "2025-05-12"),
    HarvestDate = c("2025-09-10", "2025-09-12"),
    stringsAsFactors = FALSE
  )
  res_loc <- validate_environmental_data(df_loc)
  expect_true(res_loc$valid)
  expect_equal(length(res_loc$errors), 0)
})

test_that("process_environmental_covariates supports robust Site/Loc columns", {
  bad_data_site <- data.frame(
    Site = "E1"
  )
  expect_equal(process_environmental_covariates(bad_data_site), bad_data_site)

  bad_data_loc <- data.frame(
    Loc = "E1"
  )
  expect_equal(process_environmental_covariates(bad_data_loc), bad_data_loc)
})

