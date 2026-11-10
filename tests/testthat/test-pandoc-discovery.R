test_that("report export discovers direct and architecture-specific Pandoc bundles", {
  local_mocked_bindings(
    pandoc_available = function(...) FALSE,
    find_pandoc = function(dir, ...) {
      if (file.exists(file.path(dir, "pandoc-marker"))) list(dir = dir) else list(dir = NULL)
    },
    .package = "rmarkdown"
  )
  root <- tempfile("rstudio-tools-")
  dir.create(root)
  expect_false(report_pandoc_available(root))
  for (architecture in c("aarch64", "x86_64", ".")) {
    path <- file.path(root, architecture)
    dir.create(path, showWarnings = FALSE)
    file.create(file.path(path, "pandoc-marker"))
    expect_true(report_pandoc_available(root), info = architecture)
    unlink(file.path(path, "pandoc-marker"))
  }
  expect_false(report_pandoc_available(root))
})
