Sys.setenv(LC_ALL = "C", LANG = "C")
.libPaths(c(normalizePath(".local-library", mustWork = FALSE), .libPaths()))
pkgload::load_all(".", quiet = TRUE)
d <- readRDS("validation-output/performance/user-before-100.rds")$data
traits <- c("y", paste0("trait", 2:9))
rows <- lapply(traits, function(trait) {
  timing <- system.time({
    a <- run_anova_met(d, "gen", "env", "rep", trait = trait)
    m <- run_mixed_model(d, "gen", "env", "rep", trait)
    blue <- compute_blues(d, "gen", "env", "rep", trait)
    blup <- compute_blups(d, "gen", "env", "rep", trait, mixed_result = m)
  })[["elapsed"]]
  stopifnot(nrow(blue) == 100L, nrow(blup) == 100L,
    all(is.finite(blue$SE)), all(is.finite(blup$SE)), is.finite(m$H2))
  cat(trait, timing, "seconds\n"); flush.console()
  data.frame(Trait = trait, Observations = a$n_obs, Seconds = timing, H2 = m$H2)
})
write.csv(do.call(rbind, rows), "validation-output/performance/all-nine-traits.csv", row.names = FALSE)
