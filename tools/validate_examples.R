Sys.setenv(LC_ALL = "C", LANG = "C")
.libPaths(c(normalizePath(".local-library", mustWork = FALSE), .libPaths()))
pkgload::load_all(".", quiet = TRUE)
d <- read.csv(app_sys("extdata/example_phenotypic.csv"))
traits <- c("GrainYield", "PlantHeight", "DaysToFlowering", "TKW")
summary <- do.call(rbind, lapply(traits, function(trait) {
  a <- run_anova_met(d, "Genotype", "Environment", "Rep", "Block", trait)
  m <- run_mixed_model(d, "Genotype", "Environment", "Rep", trait, "Block")
  b <- compute_blups(d, "Genotype", "Environment", "Rep", trait, "Block", mixed_result = m)
  blue <- compute_blues(d, "Genotype", "Environment", "Rep", trait, "Block")
  ammi <- run_ammi(d, "Genotype", "Environment", "Rep", trait, block_col = "Block")
  gge <- run_gge_fast(d, "Genotype", "Environment", trait)
  ge <- build_ge_matrices(prepare_metan_data(d, "Genotype", "Environment", "Rep", trait),
    "Genotype", "Environment", trait, "Rep", "Block")
  er <- compute_er_table_fast(ge)
  stopifnot(nrow(b) == 20L, nrow(blue) == 20L, nrow(ammi$stability) == 20L,
    all(is.finite(b$BLUP)), all(is.finite(blue$BLUE)), all(is.finite(er$S2di)), !is.null(gge$model))
  cat("Validated", trait, "\n")
  data.frame(Trait = trait, Observations = a$n_obs, Genotypes = nrow(b), Environments = m$n_env,
    H2_Cullis = m$H2, Mean_BLUP = mean(b$BLUP), Mean_BLUE = mean(blue$BLUE), Imputed_cells = ammi$n_imputed)
}))
dir.create("validation-output", showWarnings = FALSE)
write.csv(summary, "validation-output/example-analysis-summary.csv", row.names = FALSE)
print(summary)
