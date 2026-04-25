# =============================================================================
# MERIDIAN — Utility Functions
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


  # Module 1 — Data Upload
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

  # Module 2 — EDA
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

  # Module 3 — ANOVA (placeholder)
  m3_title         = "ANOVA & Variance Components",

  # Module 4 — Stability (placeholder)
  m4_title         = "Stability Analysis",

  # Module 5 — Adaptation (placeholder)
  m5_title         = "Adaptation Analysis",

  # Module 6 — Reports (placeholder)
  m6_title         = "Reports & Export",

  # Module 7 — Spatial
  m7_title         = "Spatial Trends",
  m7_select_trait  = "Select Trait",
  m7_select_env    = "Select Environment",
  m7_fixed_terms   = "Fixed Terms (optional)",
  m7_random_terms  = "Random Terms (optional)",
  m7_nseg_row      = "Number of Row Segments",
  m7_nseg_col      = "Number of Column Segments",

  # Common
  coming_soon      = "Coming soon — this module will be available in a future update.",
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
    col         = c("^col$", "^column$", "^col_coord$", "^c$")
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

      # Incomplete blocks → Alpha-Lattice
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
#' @return List of validation messages: errors (fatal), warnings (informational)
validate_met_data <- function(df, gen_col, env_col, rep_col, resp_vars) {
  errors   <- character(0)
  warnings <- character(0)

  # Required columns
  if (is.null(gen_col) || !gen_col %in% names(df))
    errors <- c(errors, "Genotype column is not mapped or not found in data.")
  if (is.null(env_col) || !env_col %in% names(df))
    errors <- c(errors, "Environment column is not mapped or not found in data.")
  if (is.null(rep_col) || !rep_col %in% names(df))
    errors <- c(errors, "Replicate column is not mapped or not found in data.")
  if (is.null(resp_vars) || length(resp_vars) == 0)
    errors <- c(errors, "No response variables (traits) selected.")

  # If there are fatal errors, return early
  if (length(errors) > 0) {
    return(list(valid = FALSE, errors = errors, warnings = warnings))
  }

  # Check trait columns are numeric
  for (trait in resp_vars) {
    if (!is.numeric(df[[trait]])) {
      errors <- c(errors, paste0("Trait '", trait, "' is not numeric."))
    }
  }

  # Minimum dimensions
  n_gen <- dplyr::n_distinct(df[[gen_col]])
  n_env <- dplyr::n_distinct(df[[env_col]])
  if (n_gen < 2)
    errors <- c(errors, "At least 2 genotypes are required for MET analysis.")
  if (n_env < 2)
    errors <- c(errors, "At least 2 environments are required for MET analysis.")

  # Missing values
  for (trait in resp_vars) {
    n_miss <- sum(is.na(df[[trait]]))
    if (n_miss > 0) {
      pct <- round(100 * n_miss / nrow(df), 1)
      warnings <- c(warnings, paste0(
        "Trait '", trait, "': ", n_miss, " missing values (", pct, "%)."
      ))
    }
  }

  # Duplicates
  dup_check <- df |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(gen_col, env_col, rep_col)))
    ) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::filter(n > 1)

  if (nrow(dup_check) > 0) {
    warnings <- c(warnings, paste0(
      nrow(dup_check), " duplicate entries found (same genotype \u00D7 environment \u00D7 rep)."
    ))
  }

  # Imbalance check
  balance <- df |>
    dplyr::group_by(.data[[env_col]]) |>
    dplyr::summarise(n_gen = dplyr::n_distinct(.data[[gen_col]]), .groups = "drop")
  if (length(unique(balance$n_gen)) > 1) {
    warnings <- c(warnings, paste0(
      "Unbalanced data: number of genotypes varies across environments (",
      paste(range(balance$n_gen), collapse = "\u2013"), ")."
    ))
  }

  list(
    valid    = length(errors) == 0,
    errors   = errors,
    warnings = warnings
  )
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
#' @return Tibble with Mean, SD, Min, Max, CV%, N per group × trait
descriptive_summary <- function(df, group_col, traits) {
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

# ---------------------------------------------------------------------------
# Results Store Key Generator
# ---------------------------------------------------------------------------
#' Generate a standard key for the global results_store
#' @param analysis_type Base analysis name (e.g., "AMMI", "GGE")
#' @param trait Trait name (defaults to "Global")
#' @return String key
make_results_key <- function(analysis_type, trait = "Global") {
  paste0(analysis_type, "__", trait)
}
