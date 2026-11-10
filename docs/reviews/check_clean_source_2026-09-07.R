# Check a temporary copy excluding safeBackup files. Does not alter the working tree.
options(width = 120)
# Windows does not accept the inherited C.UTF-8 locale; scope to check children.
Sys.setenv(LC_ALL = "C", LANG = "C", LANGUAGE = "en")
root <- normalizePath(getwd(), winslash = "/")
out_dir <- file.path(root, "docs", "reviews")
stage <- tempfile("MERIDIAN-clean-review-")
dir.create(stage)
src_dir <- file.path(stage, "MERIDIAN")
dir.create(src_dir)
for (f in c("DESCRIPTION", "NAMESPACE", "LICENSE", "LICENSE.md", ".Rbuildignore")) {
  if (file.exists(f)) file.copy(f, src_dir)
}
for (folder in c("R", "src", "man", "inst", "tests")) {
  paths <- list.files(folder, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  paths <- paths[!dir.exists(paths) & !grepl("safeBackup|[.](o|dll|so)$", paths)]
  for (p in paths) {
    dst <- file.path(src_dir, p)
    dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
    file.copy(p, dst)
  }
}
setwd(stage)
build_log <- file.path(out_dir, "clean_build_2026-09-07.txt")
status <- system2(file.path(R.home("bin"), "R.exe"),
                  c("CMD", "build", "--no-build-vignettes", shQuote(src_dir)), stdout = build_log, stderr = build_log)
cat("Clean source build exit:", status, "\n")
cat(readLines(build_log, warn = FALSE), sep = "\n")
if (status == 0) {
  tarball <- list.files(stage, pattern = "[.]tar[.]gz$", full.names = TRUE)[1]
  check_log <- file.path(out_dir, "clean_check_console_2026-09-07.txt")
  Sys.setenv("_R_CHECK_FORCE_SUGGESTS_" = "false", "_R_CHECK_CRAN_INCOMING_REMOTE_" = "false")
  status <- system2(file.path(R.home("bin"), "R.exe"),
                    c("CMD", "check", "--no-manual", "--no-vignettes", shQuote(tarball)),
                    stdout = check_log, stderr = check_log)
  cat("Clean source check exit:", status, "\n")
  result_dir <- file.path(stage, "MERIDIAN.Rcheck")
  for (f in c("00check.log", "00install.out")) {
    if (file.exists(file.path(result_dir, f))) {
      file.copy(file.path(result_dir, f), file.path(out_dir, paste0("clean_", f)), overwrite = TRUE)
    }
  }
  cat(tail(readLines(check_log, warn = FALSE), 45), sep = "\n")
}
