# Run from repository root with Rscript --vanilla.
# Audit only: no production source files are changed.
options(width = 120, warn = 1)
out_dir <- file.path("docs", "reviews")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
log_con <- file(file.path(out_dir, "evidence_2026-09-07.txt"), "wt")
sink(log_con, split = TRUE)
cat("MERIDIAN audit | ", format(Sys.time()), "\n", R.version.string, "\n", sep = "")
cat("Working directory: ", getwd(), "\n")
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(tibble))
for (f in c("utils.R", "data_processing.R", "models.R", "stability.R", "ammi.R", "enviromics.R", "report_registry.R", "report.R")) {
  source(file.path("R", f))
}
audit_case <- function(label, expr) {
  cat("\n### ", label, "\n", sep = "")
  tryCatch(force(expr), error = function(e) cat("ERROR: ", conditionMessage(e), "\n", sep = ""))
}
set.seed(20260907)
d <- expand.grid(gen = sprintf("G%02d", 1:12), env = paste0("E", 1:5), rep = 1:3)
g_eff <- rnorm(12, 0, 2)
ge_eff <- matrix(rnorm(60, 0, 2), 12, 5)
rep_eff <- matrix(rnorm(15, 0, 1), 5, 3)
d$y <- 30 + g_eff[d$gen] + c(-15, -5, 0, 7, 20)[d$env] +
  ge_eff[cbind(as.integer(d$gen), as.integer(d$env))] +
  rep_eff[cbind(as.integer(d$env), d$rep)] + rnorm(nrow(d), 0, 2)

audit_case("01 BLUPs change when only reference environment changes", {
  b1 <- compute_blups(d, "gen", "env", "rep", "y")
  d2 <- d
  d2$env <- relevel(d2$env, "E5")
  b2 <- compute_blups(d2, "gen", "env", "rep", "y")
  cmp <- merge(b1, b2, by = "Genotype", suffixes = c("_E1", "_E5"))
  print(head(cmp[, c("Genotype", "BLUP_E1", "BLUP_E5", "Effect_E1", "Effect_E5")], 3))
  cat("Mean BLUP shift:", mean(cmp$BLUP_E5 - cmp$BLUP_E1), "\n")
  cat("Observed overall mean:", mean(d$y), "mean reported BLUP:", mean(b1$BLUP), "\n")
})
audit_case("02 H2 changes with equivalent replicate identifiers", {
  m1 <- run_mixed_model(d, "gen", "env", "rep", "y")
  d2 <- d
  d2$rep <- interaction(d2$env, d2$rep, drop = TRUE)
  m2 <- run_mixed_model(d2, "gen", "env", "rep", "y")
  print(data.frame(labels = c("1:3 reused within each env", "unique env-rep labels"),
                   n_rep = c(m1$n_rep, m2$n_rep), H2 = c(m1$H2, m2$H2)))
  cat("Maximum difference in fitted variance components:",
      max(abs(as.data.frame(lme4::VarCorr(m1$model))$vcov - as.data.frame(lme4::VarCorr(m2$model))$vcov)), "\n")
})
audit_case("03 Alpha lattice: equivalent block labels change fitted model", {
  set.seed(827)
  a <- d
  a$block <- ave(seq_len(nrow(a)), a$env, a$rep,
                 FUN = function(z) sample(rep(1:3, each = 4)))
  effect <- rnorm(45, 0, 8)
  a$y <- a$y + effect[as.integer(interaction(a$env, a$rep, a$block))]
  a_unique <- a
  a_unique$block <- interaction(a$rep, a$block)
  f1 <- run_anova_met(a, "gen", "env", "rep", "block", "y", "Alpha-Lattice")
  f2 <- run_anova_met(a_unique, "gen", "env", "rep", "block", "y", "Alpha-Lattice")
  print(data.frame(labels = c("blocks restart within replicate", "unique blocks within env"),
                   residual_df = c(df.residual(f1$model), df.residual(f2$model)),
                   residual_MS = c(deviance(f1$model)/df.residual(f1$model), deviance(f2$model)/df.residual(f2$model))))
  cat("Model 1:", deparse(formula(f1$model)), "\n")
})
audit_case("04 BLUEs crash when emmeans uses asymptotic intervals", {
  old_opts <- emmeans::emm_options(lmer.df = "asymptotic")
  tryCatch(print(head(compute_blues(d, "gen", "env", "rep", "y"))),
           finally = emmeans::emm_options(old_opts))
})
audit_case("05 Missing GE cell breaks native AMMI", {
  incomplete <- subset(d, !(gen == "G01" & env == "E1"))
  run_ammi(incomplete, "gen", "env", "rep", "y")
})
audit_case("06 Pruning discards complete usable environments", {
  q <- subset(d, !(gen %in% c("G01", "G02", "G03", "G04", "G05", "G06") &
                     env == "E1"))
  pruned <- prepare_metan_data(q, "gen", "env", "rep", "y")
  cat("Input:", nrow(q), "rows; retained:", nrow(pruned), "rows\n")
  cat("Discarding E1 alone preserves:", nrow(subset(q, env != "E1")), "rows\n")
  cat("Retained genotypes:", paste(unique(pruned$gen), collapse = ","), "\n")
})
audit_case("07 Eberhart-Russell misses experimental-error correction", {
  ge <- build_ge_matrices(d, "gen", "env", "y")
  er <- compute_er_table_fast(ge)
  reference <- metan::ge_reg(d, env, gen, rep, y, verbose = FALSE)
  cat("metan result fields:", names(reference$y), "\n")
  print(head(er[, c("GEN", "bi", "S2di")], 3))
  print(head(reference$y$regression, 3))
  cat("metan implementation relevant lines:\n")
  ref_src <- deparse(body(metan::ge_reg))
  cat(ref_src[grep("S2di|s2di|MSerror|MSRES|MSE|nrep|deviation", ref_src, ignore.case = TRUE)], sep = "\n")
})
audit_case("08 Additive imputation fails even for perfectly additive observations", {
  z <- expand.grid(gen = c("A", "B", "C"), env = c("E1", "E2", "E3"))
  z$y <- c(10, 20, 30)[z$gen] + c(0, 100, 200)[z$env]
  z$rep <- 1
  z <- subset(z, !(gen == "A" & env == "E3"))
  imp <- prepare_gge_data(z, "gen", "env", "y")
  print(subset(imp$data, gen == "A" & env == "E3"))
  cat("Known additive value: 210\n")
})
audit_case("09 Dataset signature collision", {
  db <- list(data = d, traits = "y", gen_col = "gen", env_col = "env", rep_col = "rep")
  db2 <- db
  db2$data$y <- db2$data$y + 1000
  cat("All trait values changed; signatures identical:", identical(make_dataset_signature(db), make_dataset_signature(db2)), "\n")
})
audit_case("10 Validation accepts missing identifiers and infinite traits", {
  bad <- d
  bad$gen[1] <- NA
  bad$env[2] <- NA
  bad$rep[3] <- NA
  bad$y[4] <- Inf
  print(validate_met_data(bad, "gen", "env", "rep", "y"))
})
audit_case("11 Methods declare analyses even when no analyses exist", {
  cat(build_methods_text(list(data = d, gen_col = "gen", env_col = "env", traits = "y")), "\n")
})
audit_case("12 Finlay-Wilkinson without native library", {
  run_finlay_wilkinson(d, "gen", "env", "y")
})
audit_case("13 Check build tools", {
  print(Sys.which(c("make", "g++", "gcc")))
  print(pkgbuild::has_build_tools(debug = TRUE))
})
audit_case("14 Duplicate native exported symbols", {
  cpp <- list.files("src", pattern = "^RcppExports.*[.]cpp$", full.names = TRUE)
  symbols <- lapply(cpp, function(f) grep("^RcppExport", readLines(f), value = TRUE))
  names(symbols) <- cpp
  print(symbols)
})
audit_case("15 All R source files parse", {
  files <- list.files("R", pattern = "[.]R$", full.names = TRUE)
  failures <- lapply(files, function(f) tryCatch({parse(f); NULL}, error = function(e) paste(f, conditionMessage(e))))
  cat(length(files), "R files; parse failures:", length(Filter(Negate(is.null), failures)), "\n")
  print(Filter(Negate(is.null), failures))
})
cat("\n### Session\n")
print(sessionInfo())
sink()
close(log_con)
