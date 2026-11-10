test_that("BLUPs, prediction errors and generalized H2 are coding-invariant", {
  d <- balanced_fixture()
  a <- run_mixed_model(d, "gen", "env", "rep", "y")
  b <- compute_blups(d, "gen", "env", "rep", "y", mixed_result = a)
  d$env <- stats::relevel(d$env, "E4")
  d$rep <- interaction(d$env, d$rep)
  a2 <- run_mixed_model(d, "gen", "env", "rep", "y")
  b2 <- compute_blups(d, "gen", "env", "rep", "y", mixed_result = a2)
  expect_equal(b$BLUP, b2$BLUP, tolerance = 1e-4)
  expect_equal(b$SE, b2$SE, tolerance = 1e-4)
  expect_equal(a$H2, a2$H2, tolerance = 1e-4)
  expect_equal(a$n_rep, a2$n_rep)
  expect_equal(mean(b$BLUP), mean(d$y), tolerance = 1e-5)
  expect_true(all(b$CI_lower < b$BLUP & b$CI_upper > b$BLUP))
})

test_that("block numbering does not change adjusted inference", {
  d <- balanced_fixture()
  set.seed(911)
  d$block <- ave(seq_len(nrow(d)), d$env, d$rep, FUN = function(x) sample(rep(1:2, each = 4)))
  d$y <- d$y + rnorm(24, 0, 8)[as.integer(interaction(d$env, d$rep, d$block))]
  a <- run_anova_met(d, "gen", "env", "rep", "block", "y", "Alpha-Lattice")
  m <- suppressWarnings(run_mixed_model(d, "gen", "env", "rep", "y", "block"))
  d$block <- interaction(d$rep, d$block)
  b <- run_anova_met(d, "gen", "env", "rep", "block", "y", "Alpha-Lattice")
  m2 <- suppressWarnings(run_mixed_model(d, "gen", "env", "rep", "y", "block"))
  expect_equal(a$anova_table, b$anova_table, tolerance = 1e-6)
  expect_equal(m$H2, m2$H2, tolerance = 1e-4)
  expect_true("Block(Rep, Env)" %in% m$var_table$Component)
})

test_that("marginal ANOVA does not attribute environmental imbalance to genotype", {
  set.seed(315)
  d <- expand.grid(gen = factor(paste0("G", 1:10)), env = factor(paste0("E", 1:5)), rep = factor(1:4))
  d$y <- 50 + c(-20, -10, 0, 10, 20)[d$env] + rnorm(nrow(d))
  keep <- (as.integer(d$gen) <= 5 & as.integer(d$env) <= 2) | (as.integer(d$gen) > 5 & as.integer(d$env) >= 4) | d$rep == "1"
  a <- run_anova_met(d[keep, ], "gen", "env", "rep", trait = "y")
  expect_gt(a$anova_table$P_value[a$anova_table$Source == "Genotype (G)"], .05)
  expect_false("Pct_SS" %in% names(a$anova_table))
})

test_that("BLUEs include GxE and support asymptotic intervals and selection direction", {
  d <- balanced_fixture()
  old <- emmeans::emm_options(lmer.df = "asymptotic")
  on.exit(emmeans::emm_options(old))
  b <- compute_blues(d, "gen", "env", "rep", "y", direction = "lower")
  expect_match(attr(b, "model_formula"), "GE")
  expect_true(all(is.finite(b$CI_lower)))
  expect_equal(b$BLUE, sort(b$BLUE))
})

test_that("unreplicated designs are diagnosed and augmented checks are validated", {
  d <- balanced_fixture()
  expect_error(run_mixed_model(d[d$rep == 1, ], "gen", "env", "rep", "y"), "cannot be separated")
  expect_error(run_anova_met(d, "gen", "env", "rep", trait = "y", design_type = "Augmented"), "checks")
  augmented <- d[d$gen %in% c("G01", "G02") | d$rep == 1, ]
  expect_s4_class(run_mixed_model(augmented, "gen", "env", "rep", "y", design_type = "Augmented", augmented_checks = c("G01", "G02"))$model, "merMod")
})
