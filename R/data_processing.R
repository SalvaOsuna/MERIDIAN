# =============================================================================
# MERIDIAN — Data Processing Functions
# Pure functions for data I/O, validation, and transformation
# =============================================================================

#' Read a MET data file (CSV or Excel)
#' @param path File path
#' @param sep CSV separator (ignored for Excel)
#' @param header Whether first row is header
#' @return Data frame
read_met_file <- function(path, sep = ",", header = TRUE) {
  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Reading Excel files requires the optional package 'readxl'.", call. = FALSE)
    }
    df <- readxl::read_excel(path, col_names = header)
  } else {
    df <- readr::read_delim(
      path,
      delim       = sep,
      col_names   = header,
      show_col_types = FALSE,
      locale      = readr::locale(encoding = "UTF-8")
    )
  }

  # Ensure standard data.frame (not tibble-only behaviors)
  as.data.frame(df)
}


#' Compute missing value summary per column
#' @param df Data frame
#' @return Tibble with Column, N_Missing, Pct_Missing
compute_missing_summary <- function(df) {
  tibble::tibble(
    Column     = names(df),
    N_Missing  = sapply(df, function(x) sum(is.na(x))),
    Pct_Missing = round(100 * sapply(df, function(x) sum(is.na(x))) / nrow(df), 1),
    Type       = sapply(df, function(x) class(x)[1])
  ) |>
    dplyr::arrange(dplyr::desc(N_Missing))
}


#' Check for duplicate entries in the data
#' @param df Data frame
#' @param gen_col Genotype column name
#' @param env_col Environment column name
#' @param rep_col Replicate column name
#' @return Data frame of duplicated rows (empty if none)
compute_duplicate_check <- function(df, gen_col, env_col, rep_col) {
  dup <- df |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c(gen_col, env_col, rep_col)))
    ) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup()

  dup
}


#' Compute grand descriptive statistics for the entire dataset
#' @param df Data frame
#' @param traits Character vector of trait column names
#' @return Tibble with Trait, N, Mean, SD, Min, Max, CV%
compute_grand_summary <- function(df, traits) {
  results <- lapply(traits, function(trait) {
    x <- df[[trait]]
    tibble::tibble(
      Trait  = trait,
      N      = sum(!is.na(x)),
      Mean   = round(mean(x, na.rm = TRUE), 2),
      SD     = round(sd(x, na.rm = TRUE), 2),
      Min    = round(min(x, na.rm = TRUE), 2),
      Max    = round(max(x, na.rm = TRUE), 2),
      CV_pct = round(100 * sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE), 1)
    )
  })
  dplyr::bind_rows(results)
}


#' Pivot data to genotype × environment means for a given trait
#' @param df Data frame
#' @param gen_col Genotype column
#' @param env_col Environment column
#' @param trait Trait column
#' @return Wide data frame (rows = genotypes, cols = environments)
pivot_ge_means <- function(df, gen_col, env_col, trait) {
  df |>
    dplyr::group_by(.data[[gen_col]], .data[[env_col]]) |>
    dplyr::summarise(
      value = mean(.data[[trait]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from  = dplyr::all_of(env_col),
      values_from = value
    ) |>
    tibble::column_to_rownames(gen_col) |>
    as.data.frame()
}
