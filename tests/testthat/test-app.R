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
