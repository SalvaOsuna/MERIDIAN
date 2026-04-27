# =============================================================================
# MERIDIAN — Stability Analysis Functions
# Eberhart-Russell, AMMI, GGE, Wricke, Shukla, combined rankings
# =============================================================================


#' Prepare data for stability functions
#' @param df Data frame
#' @param gen_col Genotype column name
#' @param env_col Environment column name
#' @param rep_col Replicate column name
#' @param trait Trait column name
#' @return Data frame with cleaned factor columns
prepare_metan_data <- function(df, gen_col, env_col, rep_col, trait = NULL) {
  df_std <- df

  # Drop rows with NA trait values
  if (!is.null(trait) && trait %in% names(df_std)) {
    df_std <- df_std[!is.na(df_std[[trait]]), , drop = FALSE]

    # Step 1: keep cells with the modal number of reps
    cell_counts <- table(df_std[[gen_col]], df_std[[env_col]])
    expected_reps <- as.integer(names(which.max(table(cell_counts[cell_counts > 0]))))

    bad_cells <- which(cell_counts > 0 & cell_counts != expected_reps, arr.ind = TRUE)
    if (nrow(bad_cells) > 0) {
      bad_gen <- rownames(cell_counts)[bad_cells[, 1]]
      bad_env <- colnames(cell_counts)[bad_cells[, 2]]
      bad_pairs <- paste(bad_gen, bad_env)
      cur_pairs <- paste(df_std[[gen_col]], df_std[[env_col]])
      df_std <- df_std[!cur_pairs %in% bad_pairs, , drop = FALSE]
    }

    # Step 2: iteratively prune to fully crossed design using C++ routine
    gen_fct <- as.factor(df_std[[gen_col]])
    env_fct <- as.factor(df_std[[env_col]])
    gen_int <- as.integer(gen_fct)
    env_int <- as.integer(env_fct)

    n_gen_levels <- nlevels(gen_fct)
    n_env_levels <- nlevels(env_fct)

    keep_rows <- if (exists("cpp_find_balanced_subset", mode = "function")) {
      cpp_find_balanced_subset(gen_int, env_int, n_gen_levels, n_env_levels)
    } else {
      keep_gen <- rep(TRUE, n_gen_levels)
      keep_env <- rep(TRUE, n_env_levels)
      keep <- rep(TRUE, nrow(df_std))
      changed <- TRUE
      while (changed) {
        changed <- FALSE
        active_envs <- which(keep_env)
        active_gens <- which(keep_gen)
        present <- table(
          factor(gen_int[keep], levels = seq_len(n_gen_levels)),
          factor(env_int[keep], levels = seq_len(n_env_levels))
        ) > 0
        gen_env_counts <- rowSums(present)
        env_gen_counts <- colSums(present)

        drop_gen <- active_gens[gen_env_counts[active_gens] < length(active_envs)]
        drop_env <- active_envs[env_gen_counts[active_envs] < length(active_gens)]
        if (length(drop_gen) > 0 || length(drop_env) > 0) {
          keep_gen[drop_gen] <- FALSE
          keep_env[drop_env] <- FALSE
          keep <- !is.na(gen_int) & !is.na(env_int) & keep_gen[gen_int] & keep_env[env_int]
          changed <- TRUE
        }
      }
      keep
    }
    df_std <- df_std[keep_rows, , drop = FALSE]

    df_std[[gen_col]] <- droplevels(as.factor(df_std[[gen_col]]))
    df_std[[env_col]] <- droplevels(as.factor(df_std[[env_col]]))
  }

  df_std[[gen_col]] <- as.factor(df_std[[gen_col]])
  df_std[[env_col]] <- as.factor(df_std[[env_col]])
  df_std[[rep_col]] <- as.factor(df_std[[rep_col]])
  df_std
}


#' Prepare data for GGE while preserving all observed genotypes/environments
#' Missing GxE cells are imputed with additive expectations to avoid dropping levels.
#' @param df Data frame
#' @param gen_col Genotype column name
#' @param env_col Environment column name
#' @param trait Trait column name
#' @return List with imputed GGE input data and imputed-cell count
prepare_gge_data <- function(df, gen_col, env_col, trait) {
  df_std <- df[!is.na(df[[trait]]), , drop = FALSE]
  df_std[[gen_col]] <- as.factor(df_std[[gen_col]])
  df_std[[env_col]] <- as.factor(df_std[[env_col]])

  gen_levels <- levels(df_std[[gen_col]])
  env_levels <- levels(df_std[[env_col]])
  n_gen <- length(gen_levels)
  n_env <- length(env_levels)

  if (n_gen < 2 || n_env < 2) {
    stop("GGE requires at least 2 genotypes and 2 environments.")
  }

  gen_int <- as.integer(df_std[[gen_col]])
  env_int <- as.integer(df_std[[env_col]])
  y <- as.numeric(df_std[[trait]])

  ge_means <- if (exists("cpp_ge_means", mode = "function")) {
    cpp_ge_means(gen_int, env_int, y, n_gen, n_env)
  } else {
    tapply(y, list(df_std[[gen_col]], df_std[[env_col]]), mean, na.rm = TRUE)
  }
  ge_means <- as.matrix(ge_means)
  dimnames(ge_means) <- list(gen_levels, env_levels)

  # Impute missing cells with additive expectation: mu + g + e
  miss_idx <- which(is.na(ge_means), arr.ind = TRUE)
  if (nrow(miss_idx) > 0) {
    if (exists("cpp_impute_additive_gxe", mode = "function")) {
      imputed <- cpp_impute_additive_gxe(ge_means)
      ge_means <- as.matrix(imputed$matrix)
    } else {
      grand_mean <- mean(ge_means, na.rm = TRUE)
      gen_means <- rowMeans(ge_means, na.rm = TRUE)
      env_means <- colMeans(ge_means, na.rm = TRUE)

      # Fallbacks when a full row/column is missing
      gen_means[is.na(gen_means)] <- grand_mean
      env_means[is.na(env_means)] <- grand_mean

      for (k in seq_len(nrow(miss_idx))) {
        i <- miss_idx[k, 1]
        j <- miss_idx[k, 2]
        ge_means[i, j] <- gen_means[i] + env_means[j] - grand_mean
      }
    }
    dimnames(ge_means) <- list(gen_levels, env_levels)
  }

  out <- expand.grid(
    GEN_TMP = gen_levels,
    ENV_TMP = env_levels,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  out$TRAIT_TMP <- as.numeric(ge_means)
  names(out)[1] <- gen_col
  names(out)[2] <- env_col
  names(out)[3] <- trait

  list(
    data = out,
    n_imputed = nrow(miss_idx)
  )
}


#' Build fast GxE mean matrix and additive interaction matrix
#' @param df_std Cleaned data frame
#' @param gen_col Genotype column name
#' @param env_col Environment column name
#' @param trait Trait column name
#' @return List with ge_means, interaction, means, dims and labels
build_ge_matrices <- function(df_std, gen_col, env_col, trait) {
  gen_f <- as.factor(df_std[[gen_col]])
  env_f <- as.factor(df_std[[env_col]])
  y <- as.numeric(df_std[[trait]])

  n_gen <- nlevels(gen_f)
  n_env <- nlevels(env_f)

  gen_int <- as.integer(gen_f)
  env_int <- as.integer(env_f)

  ge_means <- if (exists("cpp_ge_means", mode = "function")) {
    cpp_ge_means(gen_int, env_int, y, n_gen, n_env)
  } else {
    tapply(y, list(gen_f, env_f), mean, na.rm = TRUE)
  }

  ge_means <- as.matrix(ge_means)
  dimnames(ge_means) <- list(levels(gen_f), levels(env_f))

  if (anyNA(ge_means)) {
    stop("Balanced GxE matrix required for fast stability computations.")
  }

  grand_mean <- mean(ge_means)
  gen_means <- rowMeans(ge_means)
  env_means <- colMeans(ge_means)

  interaction <- sweep(sweep(ge_means, 1, gen_means, "-"), 2, env_means, "-") + grand_mean

  list(
    ge_means = ge_means,
    interaction = interaction,
    grand_mean = grand_mean,
    gen_means = gen_means,
    env_means = env_means,
    gen_levels = rownames(ge_means),
    env_levels = colnames(ge_means),
    n_gen = n_gen,
    n_env = n_env
  )
}


#' AMMI analysis using metan::performs_ammi()
#' @param df Data frame
#' @param gen_col Genotype column name
#' @param env_col Environment column name
#' @param rep_col Replicate column name
#' @param trait Trait column name
#' @return List with AMMI model, ANOVA table, and IPCA scores
run_ammi_metan <- function(df, gen_col, env_col, rep_col, trait) {
  df_std <- prepare_metan_data(df, gen_col, env_col, rep_col, trait)

  model <- metan::performs_ammi(
    .data = df_std,
    env   = !!rlang::sym(env_col),
    gen   = !!rlang::sym(gen_col),
    rep   = !!rlang::sym(rep_col),
    resp  = !!rlang::sym(trait)
  )

  trait_data <- model[[trait]]

  ammi_anova <- tryCatch(as.data.frame(trait_data$ANOVA), error = function(e) NULL)
  ipca_scores <- tryCatch(as.data.frame(trait_data$PCA), error = function(e) NULL)

  list(model = model, anova = ammi_anova, scores = ipca_scores)
}


#' GGE Biplot analysis using metan::gge()
#' @param df Data frame
#' @param gen_col Genotype column name
#' @param env_col Environment column name
#' @param rep_col Replicate column name
#' @param trait Trait column name
#' @return List with GGE model and imputed-cell count
run_gge <- function(df, gen_col, env_col, rep_col, trait) {
  gge_input <- prepare_gge_data(df, gen_col, env_col, trait)
  df_std <- gge_input$data

  model <- metan::gge(
    .data = df_std,
    env   = !!rlang::sym(env_col),
    gen   = !!rlang::sym(gen_col),
    resp  = !!rlang::sym(trait)
  )

  list(model = model, n_imputed = gge_input$n_imputed)
}


#' Fast Eberhart & Russell parameters from GxE means
#' @param ge GxE matrix object from build_ge_matrices()
#' @return Data frame of Eberhart-Russell parameters
compute_er_table_fast <- function(ge) {
  ei <- ge$env_means - ge$grand_mean
  ss_ei <- sum(ei^2)

  if (exists("cpp_er_statistics", mode = "function")) {
    er <- cpp_er_statistics(ge$ge_means, ei)
    bi <- as.numeric(er$slope)
    intercept <- as.numeric(er$intercept)
    s2di <- as.numeric(er$S2di)
    r2 <- as.numeric(er$R2)
  } else if (exists("cpp_fw_regression", mode = "function")) {
      mean_vals <- as.numeric(ge$ge_means)
      ei_vals <- rep(ei, each = ge$n_gen)
      gen_ids <- rep(seq_len(ge$n_gen), times = ge$n_env)
      fw <- cpp_fw_regression(mean_vals, ei_vals, gen_ids, ge$n_gen)
      bi <- as.numeric(fw$slope)
      intercept <- as.numeric(fw$intercept)
  } else if (ss_ei <= .Machine$double.eps) {
    bi <- rep(NA_real_, ge$n_gen)
    intercept <- ge$gen_means
  } else {
    bi <- as.vector(ge$ge_means %*% ei / ss_ei)
    intercept <- ge$gen_means
  }

  if (!exists("s2di", inherits = FALSE) || !exists("r2", inherits = FALSE)) {
    fitted <- matrix(intercept, nrow = ge$n_gen, ncol = ge$n_env) +
      tcrossprod(bi, ei)

    resid <- ge$ge_means - fitted
    sse <- rowSums(resid^2)
    sst <- rowSums((ge$ge_means - matrix(intercept, nrow = ge$n_gen, ncol = ge$n_env))^2)

    s2di <- if (ge$n_env > 2) sse / (ge$n_env - 2) else rep(NA_real_, ge$n_gen)
    r2 <- ifelse(sst > .Machine$double.eps, pmax(0, 1 - sse / sst), NA_real_)
  }

  data.frame(
    GEN = ge$gen_levels,
    Y = as.numeric(intercept),
    bi = as.numeric(bi),
    b1 = as.numeric(bi),
    S2di = as.numeric(s2di),
    R2 = as.numeric(r2),
    stringsAsFactors = FALSE
  )
}


#' Fast Wricke ecovalence from additive interaction matrix
#' @param ge GxE matrix object from build_ge_matrices()
#' @return Data frame of Wricke ecovalence values
compute_wricke_table_fast <- function(ge) {
  wi <- rowSums(ge$interaction^2)
  data.frame(
    GEN = ge$gen_levels,
    Y = as.numeric(ge$gen_means),
    Wi = as.numeric(wi),
    stringsAsFactors = FALSE
  )
}


#' Fast Shukla variance from Wricke and interaction SS
#' @param ge GxE matrix object from build_ge_matrices()
#' @param wi Optional precomputed Wricke ecovalence vector
#' @return Data frame of Shukla variance values
compute_shukla_table_fast <- function(ge, wi = NULL) {
  if (is.null(wi)) wi <- rowSums(ge$interaction^2)

  p <- ge$n_gen
  q <- ge$n_env
  ssge <- sum(ge$interaction^2)

  shukla <- rep(NA_real_, p)
  if (p > 2 && q > 1) {
    term1 <- (p / ((p - 2) * (q - 1))) * wi
    term2 <- ssge / ((p - 1) * (p - 2) * (q - 1))
    shukla <- pmax(0, term1 - term2)
  }

  data.frame(
    GEN = ge$gen_levels,
    Y = as.numeric(ge$gen_means),
    ShuklaVar = as.numeric(shukla),
    stringsAsFactors = FALSE
  )
}


#' Fast AMMI metrics (ASV, WAAS, SIPC) from interaction matrix
#' @param ge GxE matrix object from build_ge_matrices()
#' @return Data frame of AMMI stability metrics
compute_ammi_metrics_fast <- function(ge) {
  n_axis <- min(ge$n_gen, ge$n_env) - 1
  if (n_axis < 1) {
    return(data.frame(
      GEN = ge$gen_levels,
      ASV = NA_real_,
      WAAS = NA_real_,
      SIPC = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  sv <- svd(ge$interaction)
  axis_ss <- sv$d^2
  total_ss <- sum(axis_ss)
  axis_pct <- if (total_ss > 0) 100 * axis_ss / total_ss else rep(0, length(axis_ss))

  n_axis <- min(n_axis, length(sv$d))
  gen_ipca <- sv$u[, 1:n_axis, drop = FALSE] %*%
    diag(sqrt(sv$d[1:n_axis]), nrow = n_axis)

  metrics <- if (exists("cpp_ammi_stability", mode = "function")) {
    cpp_ammi_stability(
      gen_ipca_scores = gen_ipca,
      axis_ss = axis_ss[1:n_axis],
      axis_pct = axis_pct[1:n_axis],
      n_axis = n_axis
    )
  } else {
    ss_ratio <- if (n_axis >= 2 && axis_ss[2] > 0) axis_ss[1] / axis_ss[2] else NA_real_
    asv <- rep(NA_real_, ge$n_gen)
    if (n_axis >= 2 && !is.na(ss_ratio)) {
      asv <- sqrt((ss_ratio * gen_ipca[, 1])^2 + gen_ipca[, 2]^2)
    }

    w_sum <- sum(axis_pct[1:n_axis])
    waas <- if (w_sum > 0) {
      rowSums(abs(gen_ipca) * matrix(axis_pct[1:n_axis], nrow = ge$n_gen, ncol = n_axis, byrow = TRUE)) / w_sum
    } else {
      rep(NA_real_, ge$n_gen)
    }

    sipc <- rowSums(abs(gen_ipca))
    data.frame(ASV = asv, WAAS = waas, SIPC = sipc)
  }

  data.frame(
    GEN = ge$gen_levels,
    ASV = as.numeric(metrics$ASV),
    WAAS = as.numeric(metrics$WAAS),
    SIPC = as.numeric(metrics$SIPC),
    stringsAsFactors = FALSE
  )
}


#' Eberhart & Russell stability regression
#' @param df Data frame
#' @param gen_col Genotype column name
#' @param env_col Environment column name
#' @param rep_col Replicate column name
#' @param trait Trait column name
#' @return List with fitted model metadata and parameters
run_eberhart_russell <- function(df, gen_col, env_col, rep_col, trait) {
  df_std <- prepare_metan_data(df, gen_col, env_col, rep_col, trait)
  run_eberhart_russell_fast(df_std, gen_col, env_col, rep_col, trait)
}


#' Wricke ecovalence
#' @param df Data frame
#' @param gen_col Genotype column name
#' @param env_col Environment column name
#' @param rep_col Replicate column name
#' @param trait Trait column name
#' @return List with fitted model metadata and parameters
compute_wricke <- function(df, gen_col, env_col, rep_col, trait) {
  df_std <- prepare_metan_data(df, gen_col, env_col, rep_col, trait)
  compute_wricke_fast(df_std, gen_col, env_col, rep_col, trait)
}


#' Shukla stability variance
#' @param df Data frame
#' @param gen_col Genotype column name
#' @param env_col Environment column name
#' @param rep_col Replicate column name
#' @param trait Trait column name
#' @return List with fitted model metadata and parameters
compute_shukla <- function(df, gen_col, env_col, rep_col, trait) {
  df_std <- prepare_metan_data(df, gen_col, env_col, rep_col, trait)
  compute_shukla_fast(df_std, gen_col, env_col, rep_col, trait)
}


#' Run all stability indices
#' @param df Data frame
#' @param gen_col Genotype column name
#' @param env_col Environment column name
#' @param rep_col Replicate column name
#' @param trait Trait column name
#' @return List with fitted model metadata and combined stability table
run_all_stability <- function(df, gen_col, env_col, rep_col, trait) {
  df_std <- prepare_metan_data(df, gen_col, env_col, rep_col, trait)
  run_all_stability_fast(df_std, gen_col, env_col, rep_col, trait)
}


#' Combine stability rankings into one table
#' @param stats_table Stability statistics data frame
#' @return Rounded stability ranking data frame
stability_ranking <- function(stats_table) {
  if (is.null(stats_table)) return(NULL)

  key_cols <- intersect(
    c("GEN", "Y", "Wi", "ShuklaVar", "bi", "S2di", "AMMI_SS", "ASV", "HMGV", "RPGV", "HMRPGV"),
    names(stats_table)
  )

  if (length(key_cols) == 0) return(stats_table)

  ranking <- stats_table[, key_cols, drop = FALSE]
  num_cols <- sapply(ranking, is.numeric)
  ranking[num_cols] <- lapply(ranking[num_cols], round, 3)
  ranking
}


# =============================================================================
# Fast variants — accept pre-prepared data (skip redundant pruning)
# =============================================================================

run_gge_fast <- function(df_std, gen_col, env_col, trait) {
  # Accept either raw or preprocessed data; always preserve all env levels for GGE.
  gge_input <- prepare_gge_data(df_std, gen_col, env_col, trait)
  gge_df <- gge_input$data

  model <- metan::gge(
    .data = gge_df,
    env   = !!rlang::sym(env_col),
    gen   = !!rlang::sym(gen_col),
    resp  = !!rlang::sym(trait)
  )
  list(model = model, n_imputed = gge_input$n_imputed)
}


run_eberhart_russell_fast <- function(df_std, gen_col, env_col, rep_col, trait) {
  ge <- build_ge_matrices(df_std, gen_col, env_col, trait)
  params <- compute_er_table_fast(ge)
  list(model = NULL, params = params)
}


compute_wricke_fast <- function(df_std, gen_col, env_col, rep_col, trait) {
  ge <- build_ge_matrices(df_std, gen_col, env_col, trait)
  params <- compute_wricke_table_fast(ge)
  list(model = NULL, params = params)
}


compute_shukla_fast <- function(df_std, gen_col, env_col, rep_col, trait) {
  ge <- build_ge_matrices(df_std, gen_col, env_col, trait)
  params <- compute_shukla_table_fast(ge)
  list(model = NULL, params = params)
}


run_all_stability_fast <- function(df_std, gen_col, env_col, rep_col, trait) {
  ge <- build_ge_matrices(df_std, gen_col, env_col, trait)

  er <- compute_er_table_fast(ge)
  wr <- compute_wricke_table_fast(ge)
  sh <- compute_shukla_table_fast(ge, wi = wr$Wi)
  am <- compute_ammi_metrics_fast(ge)

  stats_table <- merge(er, wr[, c("GEN", "Wi"), drop = FALSE], by = "GEN", sort = FALSE)
  stats_table <- merge(stats_table, sh[, c("GEN", "ShuklaVar"), drop = FALSE], by = "GEN", sort = FALSE)
  stats_table <- merge(stats_table, am[, c("GEN", "ASV", "WAAS", "SIPC"), drop = FALSE], by = "GEN", sort = FALSE)

  # Preserve original genotype order from matrix rows
  stats_table <- stats_table[match(ge$gen_levels, stats_table$GEN), , drop = FALSE]

  list(model = NULL, stats_table = stats_table)
}
