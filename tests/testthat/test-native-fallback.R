test_that("native and pure-R stability paths agree", {
  d <- balanced_fixture()
  native <- build_ge_matrices(d, "gen", "env", "y", "rep")
  native_er <- compute_er_table_fast(native)
  native_ammi <- compute_ammi_metrics_fast(native)
  testthat::local_mocked_bindings(native_kernel_available = function(name) FALSE)
  fallback <- build_ge_matrices(d, "gen", "env", "y", "rep")
  expect_equal(fallback$ge_means, native$ge_means, ignore_attr = TRUE, tolerance = 1e-10)
  expect_equal(compute_er_table_fast(fallback), native_er, tolerance = 1e-8)
  expect_equal(compute_ammi_metrics_fast(fallback), native_ammi, tolerance = 1e-8)
})

test_that("AMMI remains descriptive when plot error is not estimable", {
  d <- balanced_fixture(); d <- d[d$rep == 1, ]
  expect_warning(a <- run_ammi(d, "gen", "env", "rep", "y"), "descriptive")
  expect_null(a$anova_table)
  expect_equal(nrow(a$gen_scores), 8L)
})
