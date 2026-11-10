test_that("Excel supports zero, one and many rows and uploaded filenames", {
  for (n in c(0L, 1L, 5L)) {
    d <- data.frame(Genotype = paste0("G", seq_len(5))[seq_len(n)], Value = seq_len(n))
    path <- tempfile(fileext = ".xlsx")
    write_table_excel_with_metadata(d, path, "Test", list(Trait = "Yield"))
    expect_true(file.info(path)$size > 0)
    back <- openxlsx::read.xlsx(path, sheet = "Data")
    expect_equal(nrow(back), n)
    expect_equal(names(back), names(d))
  }
  path <- tempfile(); openxlsx::write.xlsx(data.frame(x = 1:3), path)
  expect_equal(read_met_file(path, filename = "uploaded.xlsx")$x, 1:3)
})

test_that("all graph formats and compositions write valid files", {
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 4:6), ggplot2::aes(x, y)) + ggplot2::geom_point()
  composed <- compose_patchwork_figure(list(a = p, b = p), c("a", "b"), list(auto_labels = TRUE))
  for (fmt in c("PNG", "PDF", "SVG", "TIFF")) {
    path <- tempfile(fileext = paste0(".", tolower(fmt)))
    save_ggplot_by_format(composed, path, fmt, 5, 3, 100)
    expect_gt(file.info(path)$size, 100)
    header <- readBin(path, "raw", n = 4)
    expected <- switch(fmt, PNG = as.raw(c(137, 80, 78, 71)), PDF = charToRaw("%PDF"),
      SVG = charToRaw("<?xm"), TIFF = as.raw(c(73, 73, 42, 0)))
    expect_equal(header, expected)
  }
})

test_that("reports contain all selected items with escaped text and truthful methods", {
  skip_if_not(report_pandoc_available(), "Pandoc is not installed")
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = 2:4), ggplot2::aes(x, y)) + ggplot2::geom_point()
  params <- list(report_title = "Export validation", author = "Test", institution = "Test", notes = "<script>bad</script>",
    include_date = FALSE, sections = c("Data Description", "Methods Description"),
    tables = list("SpATS selected table" = data.frame(Genotype = "G1", Mean = 5)),
    plots = list("Composed figure" = compose_patchwork_figure(list(a = p, b = p), c("a", "b"), list())),
    methods_text = build_methods_text(example_bundle(), items = list()), meta = list(n_gen = 20))
  path <- tempfile(fileext = ".html")
  render_meridian_report(path, "HTML", params)
  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(html, "SpATS selected table", fixed = TRUE)
  expect_match(html, "Composed figure", fixed = TRUE)
  expect_match(html, "data:image/png;base64", fixed = TRUE)
  expect_match(html, "&lt;script&gt;bad&lt;/script&gt;", fixed = TRUE)
  expect_match(html, "No fitted analyses", fixed = TRUE)
})
