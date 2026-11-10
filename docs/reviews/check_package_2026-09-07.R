# Diagnostic package checks, confined to a temporary copy.
options(width = 120, warn = 1)
root <- normalizePath(getwd(), winslash = "/")
out_dir <- file.path(root, "docs", "reviews")
stage <- tempfile("MERIDIAN-review-")
dir.create(stage)
src_dir <- file.path(stage, "MERIDIAN")
dir.create(src_dir)
for (f in c("DESCRIPTION", "NAMESPACE", "LICENSE", "LICENSE.md", ".Rbuildignore")) {
  if (file.exists(f)) file.copy(f, src_dir)
}
for (folder in c("R", "src", "man", "inst", "tests")) {
  paths <- list.files(folder, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  paths <- paths[!dir.exists(paths) & !grepl("[.](o|dll|so)$", paths)]
  for (p in paths) {
    dst <- file.path(src_dir, p)
    dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
    file.copy(p, dst)
  }
}
lib_dir <- file.path(stage, "library")
dir.create(lib_dir)
cat("Attempt installation of source including local backup source files.\n")
status <- system2(file.path(R.home("bin"), "R.exe"),
                  c("CMD", "INSTALL", "--preclean", paste0("--library=", shQuote(lib_dir)), shQuote(src_dir)),
                  stdout = file.path(out_dir, "install_2026-09-07.txt"), stderr = file.path(out_dir, "install_2026-09-07.txt"))
cat("Install exit:", status, "\n")
cat(readLines(file.path(out_dir, "install_2026-09-07.txt"), warn = FALSE), sep = "\n")

# Remove only explicitly enumerated backup files in the temporary copy.
backup_paths <- list.files(src_dir, pattern = "safeBackup", recursive = TRUE, full.names = TRUE)
stopifnot(all(startsWith(normalizePath(backup_paths, winslash = "/"), paste0(normalizePath(stage, winslash = "/"), "/"))))
invisible(file.remove(backup_paths))
# A bundled existing DLL is used only for loading tests, not a verified fresh build.
if (file.exists(file.path(root, "src", "MERIDIAN.dll"))) {
  file.copy(file.path(root, "src", "MERIDIAN.dll"), file.path(src_dir, "src"))
}
log_con <- file(file.path(out_dir, "package_tests_2026-09-07.txt"), "wt")
sink(log_con, split = TRUE)
cat("Temporary load of canonical sources; no backups; existing DLL; compile=FALSE.\n")
tryCatch({
  pkgload::load_all(src_dir, compile = FALSE, quiet = TRUE)
  testthat::test_file(file.path(src_dir, "tests", "testthat", "test-app.R"), reporter = "summary")
  cat("\n### Shiny ANOVA: old results after replacing data\n")
  data <- read.csv(file.path(root, "inst", "extdata", "example_phenotypic.csv"))
  bundle <- list(data = data, traits = "GrainYield", gen_col = "Genotype", env_col = "Environment",
                 rep_col = "Rep", block_col = NULL, row_col = "Row", col_col = "Col",
                 design = list(design = "RCBD"), augmented_checks = character())
  current <- shiny::reactiveVal(bundle)
  shiny::testServer(MERIDIAN:::mod_anova_server,
    args = list(data_result = list(data_bundle = current)), {
      session$setInputs(trait = "GrainYield", run_anova = 1)
      old_mean <- anova_results()$anova$grand_mean
      next_bundle <- bundle
      next_bundle$data$GrainYield <- next_bundle$data$GrainYield + 100
      current(next_bundle)
      session$flushReact()
      cat("Initial result:", old_mean, "new data mean:", mean(db()$data$GrainYield, na.rm = TRUE),
          "still displayed result:", anova_results()$anova$grand_mean, "\n")
    })
  cat("\n### Shiny spatial: changing environment without refitting\n")
  current(bundle)
  shiny::testServer(MERIDIAN:::mod_spatial_server,
    args = list(data_result = list(data_bundle = current)), {
      session$setInputs(trait = "GrainYield", env = "ENV1", nseg_row = 5, nseg_col = 5,
                        gen_random = TRUE, fixed_terms = character(), random_terms = character(), run_spats = 1)
      first <- adjusted_means()
      session$setInputs(env = "ENV2")
      second <- adjusted_means()
      cat("Original label:", unique(first$Environment), "new label:", unique(second$Environment),
          "predictions identical:", identical(first$Adjusted_Mean, second$Adjusted_Mean), "\n")
    })
}, error = function(e) cat("LOAD/TEST ERROR:", conditionMessage(e), "\n"))
cat("\nFresh-build check was attempted separately; see install log.\n")
sink()
close(log_con)
