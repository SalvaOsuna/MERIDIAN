Sys.setenv(LC_ALL = "C", LANG = "C")
.libPaths(c(normalizePath(".local-library", mustWork = FALSE), .libPaths()))
pkgload::load_all(".", quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
label <- if (length(args)) args[1] else "baseline"
dir.create("validation-output/performance", recursive = TRUE, showWarnings = FALSE)
cat("Native kernels:", native_kernel_available("cpp_ge_means"), native_kernel_available("cpp_er_statistics"), "\n")
timings <- list()
designs <- if (length(args) >= 2 && args[2] == "user") list(c(100L,4L,20L)) else list(c(50L,10L,3L),c(100L,10L,3L))
for (design in designs) {
  set.seed(712)
  g <- design[1]; e <- design[2]; r <- design[3]
  d <- expand.grid(gen = sprintf("G%04d", seq_len(g)), env = paste0("E", seq_len(e)), rep = seq_len(r))
  d$y <- 50 + rnorm(g, 0, 4)[d$gen] + rnorm(e, 0, 8)[d$env] +
    rnorm(g*e, 0, 2)[as.integer(interaction(d$gen, d$env))] + rnorm(nrow(d), 0, 2)
  d$y[sample(seq_len(nrow(d)), floor(.01*nrow(d)))] <- NA
  for (j in 2:9) d[[paste0("trait",j)]] <- d$y + rnorm(nrow(d))
  run <- function(stage, expr) {
    cat("START", g, "genotypes", stage, "\n"); flush.console()
    elapsed <- system.time(result <- force(expr))["elapsed"]
    cat("DONE", stage, elapsed, "s\n"); flush.console()
    timings[[length(timings)+1L]] <<- data.frame(Version=label, Genotypes=g, Environments=e, Rows=nrow(d), Stage=stage, Seconds=unname(elapsed))
    write.csv(do.call(rbind,timings), paste0("validation-output/performance/",label,"-timings.csv"), row.names=FALSE)
    result
  }
  anova <- run("anova", run_anova_met(d, "gen", "env", "rep", trait = "y"))
  prepared <- prepare_model_data(d, "gen", "env", "rep", "y")
  fit <- run("REML_BLUP_fit", fit_met_mixed(prepared))
  covariance <- run("BLUP_prediction_uncertainty", genotype_prediction_covariance(fit))
  bluefit <- run("REML_BLUE_fit", fit_met_mixed(prepared, FALSE))
  blues <- run("BLUE_intervals", as.data.frame(emmeans::emmeans(bluefit, "GEN")))
  ge <- run("GE_aggregation_and_error", build_ge_matrices(d, "gen", "env", "y", "rep"))
  saveRDS(list(data=d, anova=anova$anova_table, blup=lme4::ranef(fit)$GEN,
    covariance=covariance, blues=blues), paste0("validation-output/performance/",label,"-",g,".rds"))
}
print(do.call(rbind,timings))
