Sys.setenv(LC_ALL = "C", LANG = "C")
.libPaths(c(normalizePath(".local-library", mustWork = FALSE), .libPaths()))
pkgload::load_all(".", quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) >= 1L)
d <- read_met_file(args[1])
traits <- tail(names(d), 9)
label <- if (length(args) > 1L) args[2] else "current"
out <- "validation-output/activate"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
cat("Rows:", nrow(d), "Genotypes:", length(unique(d$Lentil)), "\n")
print(table(d$ENV)); print(traits)
print(sapply(d[c("Rep_gen", "Block", "Row", "Col")], function(x) length(unique(x))))
coverage <- do.call(rbind, lapply(traits, function(trait) {
  do.call(rbind, lapply(split(d, d$ENV), function(x) data.frame(Trait = trait,
    Environment = x$ENV[1], Observed = sum(is.finite(x[[trait]])),
    Genotypes = length(unique(x$Lentil[is.finite(x[[trait]])])))))
}))
write.csv(coverage, file.path(out, "coverage.csv"), row.names = FALSE)
results <- list()
run <- function(trait, stage, expr) {
  error <- ""
  elapsed <- system.time(value <- tryCatch(force(expr), error = function(e) {
    error <<- conditionMessage(e); NULL
  }))[["elapsed"]]
  results[[length(results)+1L]] <<- data.frame(Trait = trait, Stage = stage, Seconds = elapsed, Error = error)
  write.csv(do.call(rbind, results), file.path(out, paste0(label, "-results.csv")), row.names = FALSE)
  cat(trait, stage, elapsed, if(nzchar(error)) error else "OK", "\n"); flush.console()
  if (!is.null(value) && stage %in% c("anova", "mixed", "blue", "blup", "observed_winners"))
    saveRDS(value, file.path(out, paste0(label, "-", trait, "-", stage, ".rds")))
  value
}
if (any(nzchar(do.call(rbind, results)$Error))) quit(status = 1L)
for (trait in traits) {
  run(trait, "means_heatmap", plotly::plotly_build(plot_ge_heatmap(d, "Lentil", "ENV", trait)))
  run(trait, "environment_correlations", plotly::plotly_build(plot_env_correlation(d, "Lentil", "ENV", trait)))
  run(trait, "observed_winners", run_mega_envs(d, "Lentil", "ENV", trait))
  run(trait, "reaction_norms", run_finlay_wilkinson(d, "Lentil", "ENV", trait))
  if (length(args) > 2L && args[3] == "models") {
    run(trait, "anova", run_anova_met(d, "Lentil", "ENV", "Rep_gen", trait = trait))
    m <- run(trait, "mixed", run_mixed_model(d, "Lentil", "ENV", "Rep_gen", trait))
    run(trait, "blue", compute_blues(d, "Lentil", "ENV", "Rep_gen", trait))
    run(trait, "blup", compute_blups(d, "Lentil", "ENV", "Rep_gen", trait, mixed_result = m))
    a <- run(trait, "ammi", run_ammi(d, "Lentil", "ENV", "Rep_gen", trait))
    if (!is.null(a)) run(trait, "interaction_heatmap", plotly::plotly_build(plot_ge_heatmap_ammi(a, interactive = TRUE)))
    run(trait, "gge", run_gge_fast(d, "Lentil", "ENV", trait))
  }
}
