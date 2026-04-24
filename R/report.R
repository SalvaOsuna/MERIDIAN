# =============================================================================
# MERIDIAN — Automated Report Helpers
# =============================================================================

build_methods_text <- function(db, anova_res = NULL, stab_res = NULL, adapt_res = NULL) {
  traits <- db$traits %||% character(0)
  trait_txt <- if (length(traits) > 0) paste(traits, collapse = ", ") else "no traits selected"
  n_gen <- if (!is.null(db$gen_col) && db$gen_col %in% names(db$data)) dplyr::n_distinct(db$data[[db$gen_col]]) else NA_integer_
  n_env <- if (!is.null(db$env_col) && db$env_col %in% names(db$data)) dplyr::n_distinct(db$data[[db$env_col]]) else NA_integer_
  design <- db$design$design %||% "Unknown"

  model_parts <- c("fixed-effects ANOVA", "mixed model (REML)", "BLUEs", "BLUPs")
  stab_parts <- c("AMMI decomposition", "GGE biplot", "Wricke ecovalence", "Shukla variance", "Finlay-Wilkinson regression")
  env_covars <- if (!is.null(db$env_data)) {
    names(db$env_data)[sapply(db$env_data, is.numeric)]
  } else character(0)

  paste0(
    "Phenotypic records were analyzed in MERIDIAN using the ", design, " design setup. ",
    "A total of ", ifelse(is.na(n_gen), "NA", n_gen), " genotypes across ",
    ifelse(is.na(n_env), "NA", n_env), " environments were considered. ",
    "Traits analyzed: ", trait_txt, ". ",
    "Statistical analyses included ", paste(model_parts, collapse = ", "), ". ",
    "Stability analyses included ", paste(stab_parts, collapse = ", "), ". ",
    if (length(env_covars) > 0) {
      paste0("Enviromics covariables included: ", paste(env_covars, collapse = ", "), ". ")
    } else {
      "No enviromics covariables were available. "
    },
    "All figures were rendered directly from ggplot2 objects at publication resolution."
  )
}

render_meridian_report <- function(output_file, output_format, params, session = NULL) {
  template_path <- file.path("inst", "report_template.Rmd")
  if (!file.exists(template_path)) stop("Report template not found: inst/report_template.Rmd")

  quiet <- FALSE
  if (identical(output_format, "PDF")) {
    if (!requireNamespace("pagedown", quietly = TRUE)) {
      stop("PDF export requires package 'pagedown'. Please install it.")
    }
    html_tmp <- tempfile(fileext = ".html")
    rmarkdown::render(
      input = template_path,
      output_file = html_tmp,
      output_format = "html_document",
      params = params,
      envir = new.env(parent = globalenv()),
      quiet = quiet
    )
    tryCatch(
      pagedown::chrome_print(input = html_tmp, output = output_file),
      error = function(e) {
        stop("PDF generation via pagedown failed. Ensure Chrome/Chromium is available. Details: ", conditionMessage(e))
      }
    )
  } else {
    rmarkdown::render(
      input = template_path,
      output_file = output_file,
      output_format = "html_document",
      params = params,
      envir = new.env(parent = globalenv()),
      quiet = quiet
    )
  }
}
