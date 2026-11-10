test_that("environment-block ANOVA matches the full model on incomplete blocks", {
  set.seed(909)
  d <- expand.grid(gen = factor(paste0("G", 1:20)), env = factor(paste0("E", 1:4)), rep = factor(1:15))
  d$block <- ave(seq_len(nrow(d)), d$env, d$rep, FUN = function(x) sample(rep(1:5, each = 4)))
  d$y <- 25 + rnorm(20)[d$gen] + rnorm(4, 0, 5)[d$env] + rnorm(nrow(d))
  d$y[seq(1, nrow(d), by = 53)] <- NA
  fast <- run_anova_met(d, "gen", "env", "rep", "block", "y")
  expect_identical(fast$engine, "Independent environment QR blocks")
  testthat::local_mocked_bindings(run_anova_by_environment = function(d) NULL)
  reference <- run_anova_met(d, "gen", "env", "rep", "block", "y")
  expect_equal(fast$anova_table, reference$anova_table, tolerance = 1e-6)
  ge <- build_ge_matrices(d, "gen", "env", "y", "rep", "block")
  residual <- reference$anova_table[reference$anova_table$Source == "Residuals", ]
  expect_equal(ge$residual_ms, residual$MeanSq, tolerance = 1e-8)
  expect_equal(ge$residual_df, residual$Df)
})

test_that("recent identical analyses are reused but changed data remain invalid", {
  server <- function(input, output, session) {
    counter <- new.env(parent = emptyenv()); counter$n <- 0L
    result <- analysis_event(input$run, input$key, { counter$n <- counter$n + 1L; counter$n })
  }
  shiny::testServer(server, {
    session$setInputs(key = "a", run = 1)
    expect_equal(as.integer(result()), 1L)
    session$setInputs(run = 2)
    expect_equal(as.integer(result()), 1L)
    session$setInputs(key = "b")
    expect_null(result())
    session$setInputs(run = 3)
    expect_equal(as.integer(result()), 2L)
    session$setInputs(key = "a", run = 4)
    expect_equal(as.integer(result()), 1L)
  })
})
