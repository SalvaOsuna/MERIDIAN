# Rebuild the package documentation in the existing GitHub Pages source folder.
# Run from the repository root with pkgdown and package dependencies installed.
if (!requireNamespace("pkgdown", quietly = TRUE)) {
  stop("Install pkgdown before building the documentation: install.packages('pkgdown')")
}
if (!rmarkdown::pandoc_available()) {
  # Use the same RStudio-bundled Pandoc discovery as the app's report exporter.
  pkgload::load_all(".", quiet = TRUE)
  if (!MERIDIAN:::report_pandoc_available()) stop("Pandoc is required to build the website.")
}
# Build in an empty staging directory so pkgdown never cleans historical reviews
# or local evidence kept alongside the website under docs/.
staging <- tempfile("meridian-pkgdown-")
dir.create(staging)
withr::with_envvar(c(RSTUDIO_PANDOC = rmarkdown::find_pandoc()$dir), {
  pkgdown::build_site(preview = FALSE, examples = TRUE, devel = FALSE,
                     override = list(destination = staging))
})
paths <- list.files(staging, recursive = TRUE, all.files = TRUE, no.. = TRUE)
for (path in paths) {
  target <- file.path("docs", path)
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(file.path(staging, path), target, overwrite = TRUE)) {
    stop("Could not copy generated page: ", path)
  }
}
file.create("docs/.nojekyll")
message("Documentation built in docs/; historical reviews and local evidence preserved.")
