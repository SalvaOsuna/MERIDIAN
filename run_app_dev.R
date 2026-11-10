# Run from the project root. Package dependencies must already be installed.
if (dir.exists(".local-library")) .libPaths(c(normalizePath(".local-library"), .libPaths()))
pkgload::load_all(".")
shiny::runApp(MERIDIAN::run_app())
