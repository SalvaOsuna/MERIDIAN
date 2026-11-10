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
  if (is.null(trait)) stop("Select a trait before preparing stability data.")
  if (!is.numeric(df[[trait]]) || any(is.infinite(df[[trait]]))) stop("Trait values must be numeric and finite.")
  keys <- c(gen_col, env_col, rep_col)
  if (any(vapply(df[keys], function(x) any(is.na(x) | !nzchar(trimws(as.character(x)))), logical(1))))
    stop("Experimental identifiers cannot be missing or empty.")
  out <- droplevels(df[!is.na(df[[trait]]), , drop = FALSE])
  original <- out
  # Remove one level at a time, retaining all replicate counts and checks.
  repeat {
    counts <- table(out[[gen_col]], out[[env_col]])
    if (any(dim(counts) < 2)) stop("No complete stability subset with at least two genotypes and environments remains.")
    if (all(counts > 0)) break
    gm <- rowMeans(counts == 0); em <- colMeans(counts == 0)
    if (nrow(counts) <= 2) gm[] <- -Inf
    if (ncol(counts) <= 2) em[] <- -Inf
    if (all(!is.finite(c(gm, em)))) stop("Insufficient connected observations for complete-cell stability indices.")
    if (max(em) >= max(gm)) out <- out[as.character(out[[env_col]]) != names(which.max(em)), , drop = FALSE]
    else out <- out[as.character(out[[gen_col]]) != names(which.max(gm)), , drop = FALSE]
    out <- droplevels(out)
  }
  for (nm in keys) out[[nm]] <- droplevels(factor(out[[nm]]))
  excluded <- data.frame(Type = c(rep("Genotype", length(setdiff(unique(original[[gen_col]]), unique(out[[gen_col]])))),
                                  rep("Environment", length(setdiff(unique(original[[env_col]]), unique(out[[env_col]]))))),
                         Level = c(as.character(setdiff(unique(original[[gen_col]]), unique(out[[gen_col]]))),
                                   as.character(setdiff(unique(original[[env_col]]), unique(out[[env_col]])))))
  attr(out, "preprocessing") <- list(input_rows = nrow(df), observed_rows = nrow(original), retained_rows = nrow(out),
                                    missing_trait_rows = sum(is.na(df[[trait]])), excluded = excluded,
                                    method = "Greedy complete-cell subset; unequal replication retained")
  if (nrow(out) < nrow(original)) warning(sprintf("Classical stability indices retain %d of %d observed records. Review the exclusions table; AMMI/GGE use all observed levels.", nrow(out), nrow(original)), call. = FALSE)
  out
}


#' Prepare data for GGE while preserving all observed genotypes/environments
#' Missing GxE cells are imputed with additive expectations to avoid dropping levels.
#' @param df Data frame
#' @param gen_col Genotype column name
#' @param env_col Environment column name
#' @param trait Trait column name
#' @return List with imputed GGE input data and imputed-cell count
prepare_gge_data <- function(df, gen_col, env_col, trait) {
  if (!is.numeric(df[[trait]]) || any(is.infinite(df[[trait]]))) stop("Trait values must be numeric and finite.")
  if (anyNA(df[c(gen_col, env_col)])) stop("Genotype and environment identifiers cannot be missing.")
  dat <- data.frame(GEN = factor(df[[gen_col]]), ENV = factor(df[[env_col]]), Y = df[[trait]])
  dat <- droplevels(dat[!is.na(dat$Y), , drop = FALSE])
  if (nlevels(dat$GEN) < 2 || nlevels(dat$ENV) < 2) stop("At least two observed genotypes and environments are required.")
  means <- stats::aggregate(Y ~ GEN + ENV, dat, mean)
  grid <- expand.grid(GEN = levels(dat$GEN), ENV = levels(dat$ENV), KEEP.OUT.ATTRS = FALSE)
  grid <- merge(grid, means, by = c("GEN", "ENV"), all.x = TRUE, sort = FALSE)
  grid$GEN <- factor(grid$GEN, levels = levels(dat$GEN)); grid$ENV <- factor(grid$ENV, levels = levels(dat$ENV))
  grid <- grid[order(grid$ENV, grid$GEN), ]
  missing <- is.na(grid$Y)
  if (any(missing)) {
    additive <- stats::lm(Y ~ GEN + ENV, means)
    if (additive$rank < ncol(stats::model.matrix(additive))) stop("Disconnected genotype-environment network: additive missing-cell predictions are not estimable.")
    grid$Y[missing] <- stats::predict(additive, grid[missing, , drop = FALSE])
  }
  imputed <- grid[missing, , drop = FALSE]
  names(grid) <- c(gen_col, env_col, trait)
  names(imputed) <- c(gen_col, env_col, trait)
  list(data = grid, n_imputed = sum(missing), imputed_cells = imputed,
       matrix = matrix(grid[[trait]], nrow = nlevels(dat$GEN), dimnames = list(levels(dat$GEN), levels(dat$ENV))),
       method = if (any(missing)) "Least-squares additive imputation of missing cell means" else "Observed cell means")
}


#' Build fast GxE mean matrix and additive interaction matrix
#' @param df_std Cleaned data frame
#' @param gen_col Genotype column name
#' @param env_col Environment column name
#' @param trait Trait column name
#' @param rep_col Optional replicate column for estimating plot error
#' @param block_col Optional incomplete block column
#' @return List with ge_means, interaction, means, dims and labels
build_ge_matrices <- function(df_std, gen_col, env_col, trait, rep_col = NULL, block_col = NULL) {
  if (!is.numeric(df_std[[trait]]) || any(is.infinite(df_std[[trait]]))) stop("Trait values must be numeric and finite.")
  df_std <- droplevels(df_std[!is.na(df_std[[trait]]), , drop = FALSE])
  gen_f <- droplevels(as.factor(df_std[[gen_col]]))
  env_f <- droplevels(as.factor(df_std[[env_col]]))
  y <- as.numeric(df_std[[trait]])

  n_gen <- nlevels(gen_f)
  n_env <- nlevels(env_f)

  gen_int <- as.integer(gen_f)
  env_int <- as.integer(env_f)

  ge_means <- if (native_kernel_available("cpp_ge_means")) {
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

  counts <- table(gen_f, env_f)
  error_models <- if (!is.null(rep_col)) {
    d <- prepare_model_data(df_std, gen_col, env_col, rep_col, trait, block_col)
    fit_met_environment_models(d)
  } else list(stats::lm(y ~ gen_f * env_f))
  error_df <- sum(vapply(error_models, stats::df.residual, numeric(1)))
  mse <- if (error_df > 0) sum(vapply(error_models, stats::deviance, numeric(1))) / error_df else NA_real_
  list(
    cell_counts = unclass(counts), residual_ms = mse, residual_df = error_df,
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

  list(model = model, n_imputed = gge_input$n_imputed, imputed_cells = gge_input$imputed_cells, method = gge_input$method)
}


#' Fast Eberhart & Russell parameters from GxE means
#' @param ge GxE matrix object from build_ge_matrices()
#' @return Data frame of Eberhart-Russell parameters
compute_er_table_fast <- function(ge) {
  ei <- ge$env_means - ge$grand_mean
  ss_ei <- sum(ei^2)

  if (native_kernel_available("cpp_er_statistics")) {
    er <- cpp_er_statistics(ge$ge_means, ei)
    bi <- as.numeric(er$slope)
    intercept <- as.numeric(er$intercept)
    s2di <- as.numeric(er$S2di)
    r2 <- as.numeric(er$R2)
  } else if (native_kernel_available("cpp_fw_regression")) {
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

  deviation_ms <- s2di
  correction <- rep(NA_real_, ge$n_gen)
  if (ge$n_env > 2 && ss_ei > .Machine$double.eps && is.finite(ge$residual_ms)) {
    leverage <- 1 / ge$n_env + ei^2 / ss_ei
    correction <- rowSums(sweep(ge$residual_ms / ge$cell_counts, 2, 1 - leverage, "*")) / (ge$n_env - 2)
  }
  s2di <- deviation_ms - correction
  data.frame(
    Deviation_MS = deviation_ms, Error_correction = correction,
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

  metrics <- if (native_kernel_available("cpp_ammi_stability")) {
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
  list(model = model, n_imputed = gge_input$n_imputed, imputed_cells = gge_input$imputed_cells, method = gge_input$method)
}


run_eberhart_russell_fast <- function(df_std, gen_col, env_col, rep_col, trait) {
  ge <- build_ge_matrices(df_std, gen_col, env_col, trait, rep_col)
  params <- compute_er_table_fast(ge)
  list(model = NULL, params = params)
}


compute_wricke_fast <- function(df_std, gen_col, env_col, rep_col, trait) {
  ge <- build_ge_matrices(df_std, gen_col, env_col, trait, rep_col)
  params <- compute_wricke_table_fast(ge)
  list(model = NULL, params = params)
}


compute_shukla_fast <- function(df_std, gen_col, env_col, rep_col, trait) {
  ge <- build_ge_matrices(df_std, gen_col, env_col, trait, rep_col)
  params <- compute_shukla_table_fast(ge)
  list(model = NULL, params = params)
}


run_all_stability_fast <- function(df_std, gen_col, env_col, rep_col, trait) {
  ge <- build_ge_matrices(df_std, gen_col, env_col, trait, rep_col)

  combine_stability_from_ge(ge)
}

combine_stability_from_ge <- function(ge) {
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
