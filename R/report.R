# Build methods from actual results or frozen registry items, never from selected traits alone.
build_methods_text <- function(db, anova_res = NULL, stab_res = NULL, adapt_res = NULL, items = NULL) {
  parts <- character()
  if (!is.null(items)) {
    parts <- vapply(items, function(x) {
      details <- c(x$metadata$model_formula, x$metadata$imputation, x$metadata$analysis_method)
      paste0(x$label, " (", x$module, "; registered ", format(x$created_at, "%Y-%m-%d %H:%M:%S"), ")",
             if (length(details)) paste0(": ", paste(details, collapse = "; ")) else "")
    }, character(1))
  } else {
    if (!is.null(anova_res$anova)) parts <- c(parts, paste(anova_res$trait, anova_res$anova$method, anova_res$anova$formula))
    if (!is.null(anova_res$mixed)) parts <- c(parts, paste(anova_res$trait, "REML", anova_res$mixed$formula, anova_res$mixed$H2_method))
    if (!is.null(anova_res$blues)) parts <- c(parts, paste(anova_res$trait, "design-adjusted BLUEs including G x E"))
    if (!is.null(anova_res$blups)) parts <- c(parts, paste(anova_res$trait, "equal-environment BLUPs with approximate prediction intervals"))
    if (!is.null(stab_res$ammi)) parts <- c(parts, paste(stab_res$trait, "AMMI", stab_res$ammi$method, "imputed cells:", stab_res$ammi$n_imputed))
    if (!is.null(stab_res$gge)) parts <- c(parts, paste(stab_res$trait, "GGE", stab_res$gge$method, "imputed cells:", stab_res$gge$n_imputed))
    for (nm in c("er", "wricke", "shukla")) if (!is.null(stab_res[[nm]])) parts <- c(parts, paste(stab_res$trait, nm))
    if (!is.null(adapt_res$fw)) parts <- c(parts, paste(adapt_res$trait, "Finlay-Wilkinson", adapt_res$fw$method))
    if (!is.null(adapt_res$mega_env)) parts <- c(parts, adapt_res$mega_env$method)
  }
  if (!length(parts)) return("No fitted analyses or registered outputs were supplied for this report.")
  paste0("MERIDIAN ", as.character(utils::packageVersion("MERIDIAN")),
    "; R ", getRversion(), ". Dataset signature: ", make_dataset_signature(db),
    ". Design: ", db$design$design %||% "unspecified", ". Included outputs: ",
    paste(unique(parts), collapse = ". "), ". Results describe the registered executions only.")
}

report_pandoc_available <- function() {
  if (rmarkdown::pandoc_available()) return(TRUE)
  # RStudio bundles Pandoc even when the app is launched from a terminal.
  candidates <- c(
    file.path(Sys.getenv("ProgramFiles"), "RStudio/resources/app/bin/quarto/bin/tools"),
    file.path(Sys.getenv("ProgramFiles"), "RStudio/bin/pandoc"),
    "/Applications/RStudio.app/Contents/Resources/app/bin/quarto/bin/tools",
    "/usr/lib/rstudio/resources/app/bin/quarto/bin/tools",
    "/usr/lib/rstudio/resources/app/bin/quarto/bin/tools/x86_64",
    "/usr/lib/rstudio/resources/app/resources/pandoc"
  )
  for (path in candidates[dir.exists(candidates)]) {
    found <- rmarkdown::find_pandoc(dir = path, cache = FALSE)
    if (length(found$dir) == 1L && nzchar(found$dir)) return(TRUE)
  }
  FALSE
}

render_meridian_report <- function(output_file, output_format, params, session = NULL) {
  if (!report_pandoc_available()) stop("HTML/PDF reports need Pandoc. Install RStudio or add Pandoc to PATH.")
  template <- app_sys("report_template.Rmd")
  work <- tempfile("meridian_report_")
  dir.create(work)
  if (!file.copy(template, file.path(work, "report.Rmd"))) stop("Could not copy report template.")
  css <- app_sys("report_styles.css")
  if (file.exists(css)) file.copy(css, file.path(work, "report_styles.css"))
  output_file <- normalizePath(output_file, winslash = "/", mustWork = FALSE)
  is_pdf <- identical(toupper(output_format), "PDF")
  html <- if (is_pdf) file.path(work, "report.html") else output_file
  rmarkdown::render(file.path(work, "report.Rmd"), output_file = html,
    output_format = rmarkdown::html_document(self_contained = TRUE,
      css = if (file.exists(file.path(work, "report_styles.css"))) file.path(work, "report_styles.css") else NULL),
    params = params, envir = new.env(parent = baseenv()), quiet = TRUE)
  if (is_pdf) {
    if (!requireNamespace("pagedown", quietly = TRUE)) stop("PDF reports require the optional package 'pagedown'.")
    pagedown::chrome_print(input = html, output = output_file)
  }
  if (!file.exists(output_file) || file.info(output_file)$size == 0) stop("The report output was not created.")
  invisible(output_file)
}
