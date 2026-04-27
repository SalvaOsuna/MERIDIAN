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
#' @param gen_col Genotype column name
#' @param env_col Environment column name
#' @param trait Trait column name
#' @return List with env_strat dataframe
run_mega_envs <- function(df, gen_col, env_col, trait) {
  df_std <- df[!is.na(df[[trait]]), ]

  gen_f <- as.factor(df_std[[gen_col]])
  env_f <- as.factor(df_std[[env_col]])
  y <- as.numeric(df_std[[trait]])

  n_gen <- nlevels(gen_f)
  n_env <- nlevels(env_f)

  ge_means <- if (exists("cpp_ge_means", mode = "function")) {
    cpp_ge_means(as.integer(gen_f), as.integer(env_f), y, n_gen, n_env)
  } else {
    tapply(y, list(gen_f, env_f), mean, na.rm = TRUE)
  }

  ge_means <- as.matrix(ge_means)
  dimnames(ge_means) <- list(levels(gen_f), levels(env_f))

  if (anyNA(ge_means)) {
    valid_env <- colSums(!is.na(ge_means)) == n_gen
    ge_means <- ge_means[, valid_env, drop = FALSE]
  }
  if (ncol(ge_means) == 0) return(list(env_strat = NULL))

  winner_idx <- apply(ge_means, 2, which.max)
  winning_gen <- rownames(ge_means)[winner_idx]
  max_mean <- ge_means[cbind(winner_idx, seq_len(ncol(ge_means)))]

  env_strat <- data.frame(
    ENV = colnames(ge_means),
    MEGA_ENV = paste0("ME: ", winning_gen),
    MEAN = as.numeric(max_mean),
    Winning_Genotype = winning_gen,
    stringsAsFactors = FALSE
  )

  list(
    env_strat = env_strat
  )
}

#' Run Finlay-Wilkinson Reaction Norms
#' Computes the environmental index and fits regressions for each genotype.
#' @param df Data frame
#' @param gen_col Genotype column name
#' @param env_col Environment column name
#' @param trait Trait column name
#' @return Data frame with fitted lines and scatter data
run_finlay_wilkinson <- function(df, gen_col, env_col, trait) {
  df_std <- df[!is.na(df[[trait]]), ]
  gen_f <- as.factor(df_std[[gen_col]])
  env_f <- as.factor(df_std[[env_col]])
  y <- as.numeric(df_std[[trait]])

  n_gen <- nlevels(gen_f)
  n_env <- nlevels(env_f)

  ge_means <- if (exists("cpp_ge_means", mode = "function")) {
    cpp_ge_means(as.integer(gen_f), as.integer(env_f), y, n_gen, n_env)
  } else {
    tapply(y, list(gen_f, env_f), mean, na.rm = TRUE)
  }

  ge_means <- as.matrix(ge_means)
  dimnames(ge_means) <- list(levels(gen_f), levels(env_f))

  if (anyNA(ge_means)) {
    keep_gen <- rowSums(!is.na(ge_means)) == ncol(ge_means)
    keep_env <- colSums(!is.na(ge_means)) == nrow(ge_means)
    ge_means <- ge_means[keep_gen, keep_env, drop = FALSE]
  }
  if (nrow(ge_means) < 2 || ncol(ge_means) < 2) {
    return(list(fw_data = NULL, gen_slopes = NULL, env_means = NULL))
  }

  grand_mean <- mean(ge_means)
  env_mean_vec <- colMeans(ge_means)
  ei <- env_mean_vec - grand_mean

  fw_data <- expand.grid(
    GEN_TMP = rownames(ge_means),
    ENV_TMP = colnames(ge_means),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  fw_data$mean_val <- as.numeric(ge_means)
  fw_data$EI <- rep(ei, each = nrow(ge_means))
  names(fw_data)[1] <- gen_col
  names(fw_data)[2] <- env_col

  env_means <- data.frame(
    ENV_TMP = colnames(ge_means),
    env_mean = as.numeric(env_mean_vec),
    EI = as.numeric(ei),
    stringsAsFactors = FALSE
  )
  names(env_means)[1] <- env_col

  gen_ids <- rep(seq_len(nrow(ge_means)), times = ncol(ge_means))
  cpp_res <- cpp_fw_regression(fw_data$mean_val, fw_data$EI, gen_ids, nrow(ge_means))

  gen_slopes <- data.frame(
    slope = cpp_res$slope,
    intercept = cpp_res$intercept,
    stringsAsFactors = FALSE
  )
  gen_slopes[[gen_col]] <- rownames(ge_means)

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
