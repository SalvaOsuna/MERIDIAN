test_that("additive imputation recovers additive missing cells and rejects disconnection", {
  z <- expand.grid(gen = c("A", "B", "C"), env = c("E1", "E2", "E3"))
  z$y <- c(10, 20, 30)[z$gen] + c(0, 100, 200)[z$env]
  z <- subset(z, !(gen == "A" & env == "E3"))
  p <- prepare_gge_data(z, "gen", "env", "y")
  expect_equal(p$n_imputed, 1)
  expect_equal(p$imputed_cells$y, 210, tolerance = 1e-8)
  expect_error(prepare_gge_data(data.frame(gen = c("A", "B"), env = c("E1", "E2"), y = 1:2), "gen", "env", "y"), "Disconnected")
  a <- run_ammi(z, "gen", "env", trait = "y")
  expect_true(all(a$stability$WAAS == 0))
  expect_true(all(a$variance_explained$Percent == 0))
})

test_that("AMMI adjusts design and reports missing cells without an SVD failure", {
  d <- balanced_fixture()
  a <- run_ammi(d, "gen", "env", "rep", "y")
  b <- run_anova_met(d, "gen", "env", "rep", trait = "y")
  expect_equal(a$anova_table, b$anova_table)
  d <- subset(d, !(gen == "G01" & env == "E1"))
  a <- run_ammi(d, "gen", "env", "rep", "y")
  expect_equal(a$n_imputed, 1)
  expect_true(all(is.finite(a$interaction_matrix)))
})

test_that("stability filtering removes a deficient environment instead of complete genotypes", {
  d <- balanced_fixture()
  d <- subset(d, !(gen %in% c("G01", "G02", "G03", "G04") & env == "E1"))
  expect_warning(p <- prepare_metan_data(d, "gen", "env", "rep", "y"), "retain")
  expect_equal(nlevels(p$gen), 8)
  expect_equal(nlevels(p$env), 3)
  expect_equal(nrow(p), 72)
  expect_equal(attr(p, "preprocessing")$excluded$Level, "E1")
})

test_that("Eberhart-Russell agrees with metan including experimental error correction", {
  d <- balanced_fixture()
  ge <- build_ge_matrices(d, "gen", "env", "y", "rep")
  ours <- compute_er_table_fast(ge)
  reference <- suppressWarnings(metan::ge_reg(d, env, gen, rep, y, verbose = FALSE))$y$regression
  idx <- match(ours$GEN, reference$GEN)
  expect_equal(ours$S2di, reference$s2di[idx], tolerance = 1e-7)
  expect_equal(ours$bi, reference$b1[idx], tolerance = 1e-7)
  expect_equal(ours$Error_correction, rep(ge$residual_ms/3, nrow(ours)), tolerance = 1e-8)
})

test_that("native aggregation and regression match direct R calculations", {
  d <- balanced_fixture()
  expect_equal(unname(cpp_ge_means(as.integer(d$gen), as.integer(d$env), d$y, 8, 4)),
               unname(as.matrix(tapply(d$y, list(d$gen, d$env), mean))), ignore_attr = TRUE)
  ge <- build_ge_matrices(d, "gen", "env", "y", "rep")
  er <- cpp_er_statistics(ge$ge_means, ge$env_means - ge$grand_mean)
  ref <- apply(ge$ge_means, 1, function(y) stats::deviance(stats::lm(y ~ I(ge$env_means - ge$grand_mean)))/2)
  expect_equal(as.numeric(er$S2di), unname(ref), tolerance = 1e-7)
})
