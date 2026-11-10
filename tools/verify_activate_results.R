Sys.setenv(LC_ALL = "C", LANG = "C")
.libPaths(c(normalizePath(".local-library", mustWork = FALSE), .libPaths()))
pkgload::load_all(".", quiet = TRUE)
args <- commandArgs(trailingOnly = TRUE)
d <- read_met_file(args[1])
traits <- tail(names(d), 9)
out <- "validation-output/activate"
rows <- lapply(traits, function(trait) {
  result <- function(stage) readRDS(file.path(out, paste0("final-", trait, "-", stage, ".rds")))
  blue <- result("blue"); blup <- result("blup"); mixed <- result("mixed"); anova <- result("anova")
  winners <- result("observed_winners")$env_strat
  stopifnot(nrow(blue) == 104L, nrow(blup) == 104L,
    all(is.finite(as.matrix(blue[c("BLUE", "SE", "CI_lower", "CI_upper")]))),
    all(is.finite(as.matrix(blup[c("BLUP", "SE", "CI_lower", "CI_upper")]))))
  means <- stats::aggregate(d[[trait]], d[c("Lentil", "ENV")], mean, na.rm = TRUE)
  best <- tapply(means$x, means$ENV, max, na.rm = TRUE)
  stopifnot(isTRUE(all.equal(as.numeric(best[winners$ENV]), winners$MEAN, tolerance = 1e-10)))
  data.frame(Trait = trait, Observed = anova$n_obs, Missing = sum(is.na(d[[trait]])),
    Genotypes = nrow(blue), H2 = mixed$H2, Singular = mixed$singular)
})
write.csv(do.call(rbind, rows), file.path(out, "model-summary.csv"), row.names = FALSE)
write.csv(attr(d, "import_issues"), file.path(out, "import-issues.csv"), row.names = FALSE)
print(do.call(rbind, rows))
