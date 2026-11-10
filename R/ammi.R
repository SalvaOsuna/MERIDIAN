#' Run AMMI Analysis natively (no metan dependency)
#'
#' @param data     Data frame in long format
#' @param gen      Genotype column name
#' @param env      Environment column name
#' @param rep      Replicate column name, or NULL when unavailable
#' @param trait    Phenotypic trait column name
#' @param n_axis   Number of IPCA axes to retain (default = 2)
#'
#' @param block_col Optional block column nested in replicate and environment
#' @param design_type Experimental design label
#' @param augmented_checks Replicated check genotypes for augmented designs
#' @return List of results for plotting and reporting

run_ammi <- function(data, gen, env, rep = NULL, trait, n_axis = 2, block_col = NULL,
                     design_type = "RCBD", augmented_checks = NULL) {
  input <- prepare_gge_data(data, gen, env, trait)
  M <- input$matrix
  n_gen <- nrow(M); n_env <- ncol(M)
  if (length(n_axis) != 1 || !is.finite(n_axis) || n_axis < 1) stop("Select at least one AMMI axis.")
  n_axis <- min(as.integer(n_axis), min(n_gen, n_env) - 1L)
  grand_mean <- mean(M)
  gen_means <- data.frame(GEN = rownames(M), gen_mean = rowMeans(M), gen_effect = rowMeans(M) - grand_mean)
  env_means <- data.frame(ENV = colnames(M), env_mean = colMeans(M), env_effect = colMeans(M) - grand_mean)
  interaction_matrix <- sweep(sweep(M, 1, rowMeans(M)), 2, colMeans(M)) + grand_mean
  if (max(abs(interaction_matrix)) < 1e-10 * max(1, max(abs(M)))) interaction_matrix[] <- 0
  sv <- svd(interaction_matrix)
  rownames(sv$u) <- rownames(M); rownames(sv$v) <- colnames(M)
  axes <- seq_len(n_axis)
  gen_scores <- as.data.frame(sweep(sv$u[, axes, drop = FALSE], 2, sqrt(sv$d[axes]), "*"))
  env_scores <- as.data.frame(sweep(sv$v[, axes, drop = FALSE], 2, sqrt(sv$d[axes]), "*"))
  names(gen_scores) <- names(env_scores) <- paste0("IPCA", axes)
  ipca_matrix <- as.matrix(gen_scores)
  gen_scores <- cbind(gen_scores, gen_means); env_scores <- cbind(env_scores, env_means)
  ss <- sv$d^2
  pct <- if (sum(ss) > 0) 100 * ss / sum(ss) else rep(0, length(ss))
  ratio <- if (n_axis >= 2 && ss[2] > 1e-12 * max(ss)) ss[1]/ss[2] else NA_real_
  stability <- gen_scores
  stability$ASV <- if (is.finite(ratio)) sqrt((ratio * ipca_matrix[, 1])^2 + ipca_matrix[, 2]^2) else if (sum(ss) == 0) 0 else NA_real_
  stability$WAAS <- if (sum(pct[axes]) > 0) as.vector(abs(ipca_matrix) %*% pct[axes] / sum(pct[axes])) else 0
  stability$SIPC <- rowSums(abs(ipca_matrix))
  stability <- stability[order(stability$WAAS), ]
  cell_means <- input$data; names(cell_means) <- c("GEN", "ENV", "mean_val")
  interaction_long <- cell_means
  interaction_long$fitted_additive <- rep(rowMeans(M), times = n_env) + rep(colMeans(M), each = n_gen) - grand_mean
  interaction_long$interaction <- as.vector(interaction_matrix)
  av <- if (!is.null(rep)) tryCatch(
    run_anova_met(data, gen, env, rep, block_col, trait, design_type, augmented_checks),
    error = function(e) {
      if (!grepl("No residual degrees of freedom", conditionMessage(e), fixed = TRUE)) stop(e)
      warning("AMMI decomposition is descriptive: no residual degrees of freedom are available for an ANOVA.", call. = FALSE)
      NULL
    }) else NULL
  list(grand_mean = grand_mean, gen_means = gen_means, env_means = env_means,
       cell_means = cell_means, interaction_long = interaction_long, interaction_matrix = interaction_matrix,
       singular_values = sv$d, U = sv$u, V = sv$v, gen_scores = gen_scores, env_scores = env_scores,
       variance_explained = data.frame(Axis = paste0("IPCA", seq_along(ss)), SS = ss, Percent = pct, Cumulative = cumsum(pct)),
       stability = stability, anova_table = av$anova_table, n_imputed = input$n_imputed,
       imputed_cells = input$imputed_cells, method = input$method, anova_method = av$method,
       params = list(gen = gen, env = env, rep = rep, trait = trait, n_axis = n_axis, n_gen = n_gen, n_env = n_env))
}
