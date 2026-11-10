# =============================================================================
# MERIDIAN - Utility Functions
# Shared helpers used across all modules
# =============================================================================

# ---------------------------------------------------------------------------
# Null-or operator (for R < 4.4.0 compatibility)
# ---------------------------------------------------------------------------
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || identical(x, "")) y else x
}

# ---------------------------------------------------------------------------
# UI Labels (i18n-ready: all user-facing strings centralized here)
# ---------------------------------------------------------------------------
LABELS <- list(
  # App-level
  app_title        = "MERIDIAN",
  app_subtitle     = "Multi-Environment Research Integration: Data Intelligence & Agronomic Networks",


  # Module 1 - Data Upload
  m1_title         = "Data Upload & Validation",
  m1_upload        = "Upload Data File",
  m1_upload_hint   = "Accepted formats: .csv, .xlsx, .xls",
  m1_separator     = "CSV Separator",
  m1_header        = "First row is header",
  m1_select_gen    = "Genotype Column",
  m1_select_env    = "Environment Column",
  m1_select_rep    = "Replicate Column",
  m1_select_block  = "Block Column (optional)",
  m1_select_row    = "Row Column (optional)",
  m1_select_col    = "Column Column (optional)",
  m1_select_traits = "Response Variables (traits)",
  m1_download_ex   = "Download Example Dataset",
  m1_design        = "Experimental Design",
  m1_preview       = "Data Preview",
  m1_missing       = "Missing Values",
  m1_summary_env   = "Summary by Environment",
  m1_summary_gen   = "Summary by Genotype",
  m1_no_data       = "No data loaded. Upload a file or load the example dataset.",

  # Module 2 - EDA
  m2_title         = "Exploratory Data Analysis",
  m2_boxplots      = "Boxplots",
  m2_heatmap       = "G\u00D7E Heatmap",
  m2_correlation   = "Environment Correlations",
  m2_outliers      = "Outlier Detection",

  m2_summary       = "Summary Tables",
  m2_select_trait  = "Select Trait",
  m2_group_by      = "Group By",
  m2_show_points   = "Show data points",
  m2_cluster_rows  = "Cluster genotypes",
  m2_cluster_cols  = "Cluster environments",
  m2_corr_method   = "Correlation Method",
  m2_outlier_method = "Detection Method",
  m2_remove_outliers = "Remove Outliers",
  m2_color_palette = "Color Palette",

  # Module 3 - ANOVA (placeholder)
  m3_title         = "ANOVA & Variance Components",

  # Module 4 - Stability (placeholder)
  m4_title         = "Stability Analysis",

  # Module 5 - Adaptation (placeholder)
  m5_title         = "Adaptation Analysis",

  # Module 6 - Reports (placeholder)
  m6_title         = "Reports & Export",

  # Module 7 - Spatial
  m7_title         = "Spatial Trends",
  m7_select_trait  = "Select Trait",
  m7_select_env    = "Select Environment",
  m7_fixed_terms   = "Fixed Terms (optional)",
  m7_random_terms  = "Random Terms (optional)",
  m7_nseg_row      = "Number of Row Segments",
  m7_nseg_col      = "Number of Column Segments",

  # Common
  coming_soon      = "Coming soon - this module will be available in a future update.",
  no_data_warning  = "Please load data in the Data Upload tab first.",
  run_analysis     = "Run Analysis",
  download_csv     = "Download CSV",
  download_xlsx    = "Download Excel",
  download_plot    = "Download Plot"
)


# ---------------------------------------------------------------------------
# Auto-detect column roles from column names
# ---------------------------------------------------------------------------
#' Match column names to expected MET roles using fuzzy matching
#' @param col_names Character vector of column names from uploaded data
#' @return Named list: genotype, environment, rep, block (NULL if not found)
auto_detect_columns <- function(col_names) {
  col_lower <- tolower(col_names)

  patterns <- list(
    genotype    = c("^gen$", "^genotype$", "^geno$", "^cultivar$",
                    "^variety$", "^line$", "^entry$", "^accession$"),
    environment = c("^env$", "^environment$", "^loc$", "^location$",
                    "^site$", "^trial$"),
    rep         = c("^rep$", "^replicate$", "^replication$"),
    block       = c("^block$", "^blk$", "^iblock$", "^sub_block$",
                    "^subblock$", "^incomplete_block$"),
    row         = c("^row$", "^row_coord$", "^r$"),
    col         = c("^col$", "^column$", "^col_coord$", "^c$"),
    flowering   = c("^days_to_flowering$", "^days_to_flower$", "^flowering$", "^dtf$", "^dth$"),
    maturity    = c("^days_to_maturity$", "^maturity$", "^dtm$")
  )

  result <- list()
  for (role in names(patterns)) {
    match_idx <- NULL
    for (pat in patterns[[role]]) {
      match_idx <- grep(pat, col_lower)
      if (length(match_idx) > 0) break
    }
    result[[role]] <- if (length(match_idx) > 0) col_names[match_idx[1]] else NULL
  }

  # Auto-detect numeric columns as potential traits
  # (This will be done with actual data in the module)
  result
}


# ---------------------------------------------------------------------------
# Detect experimental design
# ---------------------------------------------------------------------------
#' Detect the experimental design from the data structure
#' @param df Data frame
#' @param gen_col Name of genotype column
#' @param env_col Name of environment column
#' @param rep_col Name of replicate column
#' @param block_col Name of block column (can be NULL)
#' @return List with design_type and description
detect_design <- function(df, gen_col, env_col, rep_col, block_col = NULL) {
  if (is.null(gen_col) || is.null(env_col) || is.null(rep_col)) {
    return(list(
      design = "Unknown",
      description = "Required columns (genotype, environment, replicate) not fully mapped.",
      icon = "question-circle"
    ))
  }

  tryCatch({
    # Check per-environment structure
    env_summaries <- df |>
      dplyr::group_by(.data[[env_col]], .data[[rep_col]]) |>
      dplyr::summarise(
        n_gen = dplyr::n_distinct(.data[[gen_col]]),
        .groups = "drop"
      )

    total_gen <- dplyr::n_distinct(df[[gen_col]])
    n_reps    <- dplyr::n_distinct(df[[rep_col]])

    # If block column exists, check for incomplete blocks
    if (!is.null(block_col) && block_col %in% names(df)) {
      block_summaries <- df |>
        dplyr::group_by(.data[[env_col]], .data[[rep_col]], .data[[block_col]]) |>
        dplyr::summarise(
          n_gen_block = dplyr::n_distinct(.data[[gen_col]]),
          .groups = "drop"
        )

      avg_gen_per_block <- mean(block_summaries$n_gen_block)

      # Check for augmented design: some genotypes replicated (checks), others not
      gen_rep_counts <- df |>
        dplyr::group_by(.data[[env_col]], .data[[gen_col]]) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop")

      if (length(unique(gen_rep_counts$n)) > 1) {
        max_rep <- max(gen_rep_counts$n)
        min_rep <- min(gen_rep_counts$n)
        n_checks <- sum(gen_rep_counts$n == max_rep) / dplyr::n_distinct(df[[env_col]])

        if (min_rep == 1 && max_rep > 1 && n_checks < total_gen * 0.5) {
          return(list(
            design = "Augmented",
            description = paste0(
              "Augmented design detected: ", round(n_checks), " check genotypes ",
              "replicated ", max_rep, " times, remaining entries unreplicated."
            ),
            icon = "layer-group"
          ))
        }
      }

      # Incomplete blocks -> Alpha-Lattice
      if (avg_gen_per_block < total_gen * 0.9) {
        return(list(
          design = "Alpha-Lattice",
          description = paste0(
            "Alpha-lattice / incomplete block design detected: ",
            round(avg_gen_per_block), " genotypes per block (", total_gen, " total). ",
            n_reps, " replicates."
          ),
          icon = "th"
        ))
      }
    }

    # Default: RCBD
    all_complete <- all(env_summaries$n_gen == total_gen)

    if (all_complete && n_reps >= 2) {
      return(list(
        design = "RCBD",
        description = paste0(
          "Randomized Complete Block Design: ", total_gen, " genotypes \u00D7 ",
          dplyr::n_distinct(df[[env_col]]), " environments \u00D7 ",
          n_reps, " replicates."
        ),
        icon = "th-large"
      ))
    }

    # Unbalanced
    list(
      design = "Unbalanced",
      description = paste0(
        "Unbalanced design: not all genotypes present in every rep/environment. ",
        total_gen, " genotypes, ", dplyr::n_distinct(df[[env_col]]), " environments."
      ),
      icon = "exclamation-triangle"
    )

  }, error = function(e) {
    list(
      design = "Unknown",
      description = paste("Could not determine design:", e$message),
      icon = "question-circle"
    )
  })
}


# ---------------------------------------------------------------------------
# Validate MET data
# ---------------------------------------------------------------------------
#' Validate MET dataset structure and content
#' @param df Data frame
#' @param gen_col Genotype column name
#' @param env_col Environment column name
#' @param rep_col Replicate column name
#' @param resp_vars Character vector of response variable names
#' @param block_col Optional block column identifying units within replicate
#' @return List of validation messages: errors (fatal), warnings (informational)
validate_met_data <- function(df, gen_col, env_col, rep_col, resp_vars, block_col = NULL) {
  errors <- warnings <- character()
  issues <- attr(df, "import_issues")
  if (!is.null(issues) && nrow(issues)) {
    counts <- table(issues$Column)
    warnings <- paste0("Spreadsheet error cells imported as missing: ",
      paste(paste0(names(counts), " (", as.integer(counts), ")"), collapse = ", "), ". Original file unchanged.")
  }
  required <- c(gen_col, env_col, rep_col, block_col)
  if (length(c(gen_col, env_col, rep_col)) != 3 || !all(required %in% names(df)))
    errors <- c(errors, "Map existing genotype, environment and replicate columns.")
  if (!length(resp_vars) || !all(resp_vars %in% names(df))) errors <- c(errors, "Select existing numeric response columns.")
  if (anyDuplicated(c(required, resp_vars))) errors <- c(errors, "Column roles and response variables must be distinct.")
  if (length(errors)) return(list(valid = FALSE, errors = errors, warnings = warnings))
  for (key in required) {
    if (any(is.na(df[[key]]) | !nzchar(trimws(as.character(df[[key]])))))
      errors <- c(errors, paste("Missing or empty identifiers in", key))
  }
  if (anyDuplicated(df[required])) errors <- c(errors, "Duplicate experimental units: resolve repeated genotype/environment/replicate/block records before analysis.")
  for (trait in resp_vars) {
    if (!is.numeric(df[[trait]])) { errors <- c(errors, paste("Non-numeric trait:", trait)); next }
    if (any(is.infinite(df[[trait]]))) errors <- c(errors, paste("Infinite trait values:", trait))
    good <- is.finite(df[[trait]])
    if (length(unique(df[[gen_col]][good & !is.na(df[[gen_col]])])) < 2 ||
        length(unique(df[[env_col]][good & !is.na(df[[env_col]])])) < 2)
      errors <- c(errors, paste("At least two observed genotypes and environments are needed for", trait))
    if (anyNA(df[[trait]])) warnings <- c(warnings, sprintf("%s: %d missing observations; treatment is reported separately for each analysis.", trait, sum(is.na(df[[trait]]))))
    counts <- table(df[[gen_col]][good], df[[env_col]][good])
    if (length(unique(as.vector(counts))) > 1) warnings <- c(warnings, paste("Unequal replication or missing genotype-environment cells for", trait))
  }
  list(valid = !length(errors), errors = unique(errors), warnings = unique(warnings))
}


# ---------------------------------------------------------------------------
# Safe analysis wrapper
# ---------------------------------------------------------------------------
#' Execute an expression with tryCatch, showing Shiny notifications on error/warning
#' @param expr Expression to evaluate
#' @param session Shiny session object
#' @return Result of expr, or NULL on error
safe_analysis <- function(expr, session) {
  withCallingHandlers(
    tryCatch(
      expr,
      error = function(e) {
        shiny::showNotification(
          paste("Analysis error:", e$message),
          type = "error",
          duration = 10,
          session = session
        )
        NULL
      }
    ),
    warning = function(w) {
      shiny::showNotification(
        paste("Warning:", w$message),
        type = "warning",
        duration = 8,
        session = session
      )
      invokeRestart("muffleWarning")
    }
  )
}


# ---------------------------------------------------------------------------
# Descriptive summary helper
# ---------------------------------------------------------------------------
#' Compute descriptive statistics grouped by a factor
#' @param df Data frame
#' @param group_col Column name to group by
#' @param traits Character vector of trait column names
#' @return Tibble with Mean, SD, Min, Max, CV%, N per group x trait
descriptive_summary <- function(df, group_col, traits) {
  if (native_kernel_available("cpp_group_numeric_summary")) {
    group_f <- as.factor(df[[group_col]])
    group_levels <- levels(group_f)
    results <- lapply(traits, function(trait) {
      summary_df <- cpp_group_numeric_summary(
        as.integer(group_f),
        as.numeric(df[[trait]]),
        length(group_levels)
      )
      data.frame(
        Group = group_levels[summary_df$GroupId],
        Trait = trait,
        N = summary_df$N,
        Mean = round(summary_df$Mean, 2),
        SD = round(summary_df$SD, 2),
        Min = round(summary_df$Min, 2),
        Max = round(summary_df$Max, 2),
        CV_pct = round(summary_df$CV_pct, 1),
        stringsAsFactors = FALSE
      ) |>
        stats::setNames(c(group_col, "Trait", "N", "Mean", "SD", "Min", "Max", "CV_pct"))
    })
    return(dplyr::bind_rows(results))
  }

  results <- list()
  for (trait in traits) {
    summary_df <- df |>
      dplyr::group_by(.data[[group_col]]) |>
      dplyr::summarise(
        Trait  = trait,
        N      = sum(!is.na(.data[[trait]])),
        Mean   = round(mean(.data[[trait]], na.rm = TRUE), 2),
        SD     = round(sd(.data[[trait]], na.rm = TRUE), 2),
        Min    = round(min(.data[[trait]], na.rm = TRUE), 2),
        Max    = round(max(.data[[trait]], na.rm = TRUE), 2),
        CV_pct = round(100 * sd(.data[[trait]], na.rm = TRUE) /
                         mean(.data[[trait]], na.rm = TRUE), 1),
        .groups = "drop"
      )
    results[[trait]] <- summary_df
  }
  dplyr::bind_rows(results)
}


# ---------------------------------------------------------------------------
# Detect outliers
# ---------------------------------------------------------------------------
#' Detect outliers in a trait by group
#' @param df Data frame
#' @param trait Trait column name
#' @param group_col Column to group by (e.g., environment)
#' @param method "iqr" or "zscore"
#' @param threshold Multiplier (1.5 for IQR, 3 for z-score)
#' @return Data frame with an 'is_outlier' logical column added
detect_outliers <- function(df, trait, group_col, method = "iqr", threshold = NULL) {
  if (is.null(threshold)) {
    threshold <- if (method == "iqr") 1.5 else 3
  }

  if (native_kernel_available("cpp_detect_outliers")) {
    group_f <- as.factor(df[[group_col]])
    df$is_outlier <- as.logical(cpp_detect_outliers(
      as.integer(group_f),
      as.numeric(df[[trait]]),
      nlevels(group_f),
      method,
      threshold
    ))
    return(df)
  }

  df |>
    dplyr::group_by(.data[[group_col]]) |>
    dplyr::mutate(
      is_outlier = {
        x <- .data[[trait]]
        if (method == "iqr") {
          q1 <- quantile(x, 0.25, na.rm = TRUE)
          q3 <- quantile(x, 0.75, na.rm = TRUE)
          iqr_val <- q3 - q1
          x < (q1 - threshold * iqr_val) | x > (q3 + threshold * iqr_val)
        } else {
          z <- abs(scale(x))
          z > threshold
        }
      }
    ) |>
    dplyr::ungroup()
}
