# =============================================================================
# MERIDIAN — Enviromics & Adaptation Functions
# Environmental covariate analysis, typology, reaction norms
# =============================================================================

# ---------------------------------------------------------------------------
# Phenotypic Adaptation (GxE Based)
# ---------------------------------------------------------------------------

#' Run Mega-Environment Analysis (Which-won-where)
#' Natively computes winning genotypes per environment and groups them.
#' @param df Data frame
#' @param gen_col, env_col Column names
#' @param trait Trait column name
#' @return List with env_strat dataframe
run_mega_envs <- function(df, gen_col, env_col, trait) {
  df_std <- df[!is.na(df[[trait]]), ]
  
  # Calculate genotype means per environment
  env_means <- df_std |>
    dplyr::group_by(!!rlang::sym(env_col), !!rlang::sym(gen_col)) |>
    dplyr::summarise(mean_val = mean(!!rlang::sym(trait), na.rm = TRUE), .groups = "drop")
  
  # Find winner per environment
  winners <- env_means |>
    dplyr::group_by(!!rlang::sym(env_col)) |>
    dplyr::slice_max(mean_val, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::rename(Winning_Genotype = !!rlang::sym(gen_col), Max_Mean = mean_val)
  
  # Group environments by winner to form mega-environments
  env_strat <- winners |>
    dplyr::mutate(MEGA_ENV = paste0("ME: ", Winning_Genotype)) |>
    dplyr::select(ENV = !!rlang::sym(env_col), MEGA_ENV, MEAN = Max_Mean, Winning_Genotype)
  
  list(
    env_strat = env_strat
  )
}

#' Run Finlay-Wilkinson Reaction Norms
#' Computes the environmental index and fits regressions for each genotype.
#' @param df Data frame
#' @param gen_col, env_col Column names
#' @param trait Trait column name
#' @return Data frame with fitted lines and scatter data
run_finlay_wilkinson <- function(df, gen_col, env_col, trait) {
  df_std <- df[!is.na(df[[trait]]), ]

  # 1. Compute means
  cell_means <- df_std |>
    dplyr::group_by(!!rlang::sym(gen_col), !!rlang::sym(env_col)) |>
    dplyr::summarise(mean_val = mean(!!rlang::sym(trait), na.rm = TRUE), .groups = "drop")

  grand_mean <- mean(cell_means$mean_val, na.rm = TRUE)

  # 2. Compute Environmental Index (EI)
  env_means <- cell_means |>
    dplyr::group_by(!!rlang::sym(env_col)) |>
    dplyr::summarise(env_mean = mean(mean_val, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(EI = env_mean - grand_mean)

  # 3. Merge EI back to cell means
  fw_data <- cell_means |>
    dplyr::left_join(env_means |> dplyr::select(!!rlang::sym(env_col), EI), by = env_col)

  # 4. Fit regressions using fast C++ kernel
  gen_fct <- as.factor(fw_data[[gen_col]])
  gen_ids <- as.integer(gen_fct)
  n_gen   <- nlevels(gen_fct)
  
  cpp_res <- cpp_fw_regression(fw_data$mean_val, fw_data$EI, gen_ids, n_gen)
  
  gen_slopes <- data.frame(
    slope     = cpp_res$slope,
    intercept = cpp_res$intercept,
    stringsAsFactors = FALSE
  )
  gen_slopes[[gen_col]] <- levels(gen_fct)

  list(
    fw_data = fw_data,
    gen_slopes = gen_slopes,
    env_means = env_means
  )
}


# ---------------------------------------------------------------------------
# Enviromics Strategies (Covariate Based)
# ---------------------------------------------------------------------------

#' Compute Environmental Typology (PCA)
#' @param env_data Data frame with environmental covariates
#' @param env_col Column name representing environments
#' @return prcomp object and formatted data
compute_env_pca <- function(env_data, env_col) {
  # Select only numeric columns
  num_data <- env_data |> dplyr::select(dplyr::where(is.numeric))

  # Drop rows with NA in numeric columns (or impute)
  valid_rows <- complete.cases(num_data)
  num_data <- num_data[valid_rows, ]
  env_labels <- env_data[[env_col]][valid_rows]

  if (ncol(num_data) < 2 || nrow(num_data) < 2) return(NULL)

  # PCA
  pca_res <- prcomp(num_data, scale. = TRUE, center = TRUE)

  list(
    pca_res = pca_res,
    env_labels = env_labels,
    num_data = num_data
  )
}

#' Compute Covariate-Phenotype Correlations
#' @param env_data Environmental covariates
#' @param env_means Data frame from run_finlay_wilkinson (contains env_mean)
#' @param env_col Column name for environments
#' @return Correlation matrix
compute_covariate_correlations <- function(env_data, env_means, env_col) {
  # Merge
  merged <- env_data |>
    dplyr::inner_join(env_means, by = env_col)

  num_data <- merged |> dplyr::select(dplyr::where(is.numeric))

  if (ncol(num_data) < 2 || nrow(num_data) < 3) return(NULL)

  cor_mat <- cor(num_data, use = "pairwise.complete.obs")
  list(
    cor_mat = cor_mat,
    merged_data = merged
  )
}
