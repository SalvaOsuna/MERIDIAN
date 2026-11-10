# Shared design preparation, adjusted inference and prediction uncertainty.
prepare_model_data <- function(df, gen_col, env_col, rep_col, trait, block_col = NULL,
                               design_type = "RCBD", augmented_checks = NULL) {
  keys <- c(gen_col, env_col, rep_col, block_col)
  if (anyDuplicated(c(keys, trait)) || !all(c(keys, trait) %in% names(df)))
    stop("Map distinct, existing columns for experimental identifiers and trait.")
  if (!is.numeric(df[[trait]]) || any(is.infinite(df[[trait]])))
    stop("The selected trait must be numeric and contain no infinite values.")
  if (any(vapply(df[keys], function(x) any(is.na(x) | !nzchar(trimws(as.character(x)))), logical(1))))
    stop("Experimental identifiers cannot be missing or empty.")
  if (anyDuplicated(df[keys])) stop("Duplicate experimental units: resolve repeated genotype/environment/replicate/block records first.")
  d <- data.frame(Y = df[[trait]], GEN = factor(df[[gen_col]]), ENV = factor(df[[env_col]]), REP = factor(df[[rep_col]]))
  if (!is.null(block_col)) d$BLOCK <- factor(df[[block_col]])
  d <- droplevels(d[!is.na(d$Y), , drop = FALSE])
  if (nlevels(d$GEN) < 2 || nlevels(d$ENV) < 2) stop("At least two observed genotypes and environments are required for this trait.")
  d$GE <- interaction(d$GEN, d$ENV, drop = TRUE)
  d$REP_UNIT <- interaction(d$ENV, d$REP, drop = TRUE)
  if (!is.null(block_col)) d$BLOCK_UNIT <- interaction(d$ENV, d$REP, d$BLOCK, drop = TRUE)
  if (identical(design_type, "Augmented")) {
    checks <- intersect(as.character(augmented_checks), levels(d$GEN))
    if (!length(checks)) stop("Select the replicated checks for an augmented design.")
    if (!any(table(d$GE[d$GEN %in% checks]) > 1)) stop("Augmented-design checks must be replicated within environments.")
  }
  attr(d, "design_type") <- design_type
  attr(d, "n_missing") <- sum(is.na(df[[trait]]))
  d
}

design_terms <- function(d) {
  out <- if (nlevels(d$REP_UNIT) > nlevels(d$ENV)) "REP_UNIT" else character()
  if ("BLOCK_UNIT" %in% names(d) && nlevels(d$BLOCK_UNIT) > nlevels(d$REP_UNIT)) out <- c(out, "BLOCK_UNIT")
  out
}

met_reference_matrix <- function(model, d) {
  units <- unique(d[setdiff(names(d), c("Y", "GEN", "GE"))])
  grid <- do.call(rbind, lapply(levels(d$GEN), function(g) {
    x <- units; x$GEN <- factor(g, levels = levels(d$GEN)); x
  }))
  X <- stats::model.matrix(stats::delete.response(stats::terms(model)), grid,
                           contrasts.arg = model$contrasts, xlev = model$xlevels)
  weights <- 1 / as.numeric(table(grid$ENV)[grid$ENV])
  average <- function(ids) do.call(rbind, lapply(split(seq_len(nrow(grid)), ids), function(i)
    colSums(X[i, , drop = FALSE] * weights[i]) / sum(weights[i])))
  list(gen = average(grid$GEN), env = average(grid$ENV))
}

#' Run design-adjusted fixed-effects MET ANOVA
#' @param df Data frame
#' @param gen_col,env_col,rep_col Genotype, environment and replicate columns
#' @param block_col Optional block column nested within replicate and environment
#' @param trait Numeric trait column
#' @param design_type Experimental design label
#' @param augmented_checks Check genotypes for augmented designs
#' @return Adjusted tests and observation counts. Model is an lm object on the
#' general path, or a list of equivalent environment-specific lm fits on the
#' optimized path for more than 1000 observations with estimable cell means.
run_anova_met <- function(df, gen_col, env_col, rep_col, block_col = NULL, trait,
                          design_type = "RCBD", augmented_checks = NULL) {
  d <- prepare_model_data(df, gen_col, env_col, rep_col, trait, block_col, design_type, augmented_checks)
  if (nrow(d) > 1000L) {
    blocked <- run_anova_by_environment(d)
    if (!is.null(blocked)) return(blocked)
  }
  nuisance <- design_terms(d)
  model <- stats::lm(stats::reformulate(c("GEN * ENV", nuisance), "Y"), data = d)
  rdf <- stats::df.residual(model)
  if (rdf <= 0) stop("No residual degrees of freedom remain for a design-adjusted ANOVA.")
  mse <- stats::deviance(model) / rdf
  # Reuse lm's pivoted QR factorization. A second SVD of the full plot-by-
  # coefficient matrix is much more expensive and supplies the same information.
  qr_r <- qr.R(model$qr)
  independent <- seq_len(model$rank)
  dependent <- setdiff(seq_len(ncol(qr_r)), independent)
  basis <- model$qr$pivot[independent]
  omitted <- model$qr$pivot[dependent]
  R <- qr_r[independent, independent, drop = FALSE]
  inverse <- chol2inv(R)
  beta <- stats::coef(model)[basis]
  aliases <- if (length(dependent)) backsolve(R, qr_r[independent, dependent, drop = FALSE]) else NULL
  test <- function(L, label) {
    L <- sweep(L, 2, colMeans(L))
    if (length(omitted) && max(abs(L[, omitted, drop = FALSE] - L[, basis, drop = FALSE] %*% aliases)) >= 1e-7)
      return(data.frame(Source = label, Df = NA_real_, SumSq = NA_real_, MeanSq = NA_real_,
                        F_value = NA_real_, P_value = NA_real_, Note = "Equal-environment marginal contrasts are not estimable"))
    L <- L[, basis, drop = FALSE]
    ev <- eigen(L %*% inverse %*% t(L), symmetric = TRUE)
    ok <- ev$values > max(c(ev$values, 1e-12)) * 1e-8
    k <- sum(ok)
    ss <- if (k) sum(as.vector(crossprod(ev$vectors[, ok, drop = FALSE], L %*% beta))^2 / ev$values[ok]) else NA_real_
    f <- ss / k / mse
    data.frame(Source = label, Df = k, SumSq = ss, MeanSq = ss/k, F_value = f,
               P_value = stats::pf(f, k, rdf, lower.tail = FALSE), Note = "Adjusted marginal F test")
  }
  nested_test <- function(reduced, label) {
    k <- stats::df.residual(reduced) - rdf
    ss <- max(0, stats::deviance(reduced) - stats::deviance(model))
    f <- if (k > 0) ss / k / mse else NA_real_
    data.frame(Source = label, Df = k, SumSq = ss, MeanSq = if (k > 0) ss/k else NA_real_, F_value = f,
               P_value = if (k > 0) stats::pf(f, k, rdf, lower.tail = FALSE) else NA_real_, Note = "Partial F test conditional on remaining terms")
  }
  ref <- met_reference_matrix(model, d)
  reduced <- stats::lm(stats::reformulate(c("GEN", "ENV", nuisance), "Y"), data = d)
  rows <- list(test(ref$gen, "Genotype (G)"), test(ref$env, "Environment (E)"), nested_test(reduced, "G x E"))
  if (length(nuisance)) rows <- c(rows, list(nested_test(stats::lm(Y ~ GEN * ENV, d), "Design within environment")))
  rows <- c(rows, list(data.frame(Source = "Residuals", Df = rdf, SumSq = stats::deviance(model),
                                   MeanSq = mse, F_value = NA_real_, P_value = NA_real_, Note = "Plot error")))
  tbl <- do.call(rbind, rows)
  tbl$Signif <- vapply(tbl$P_value, function(p) if (is.na(p)) "" else if (p < .001) "***" else if (p < .01) "**" else if (p < .05) "*" else if (p < .1) "." else "ns", character(1))
  list(anova_table = tbl, model = model, n_obs = nrow(d), grand_mean = mean(d$Y),
       method = "Design-adjusted marginal F tests; partial sums of squares are not additive",
       formula = paste(deparse(stats::formula(model)), collapse = " "), n_missing = attr(d, "n_missing"))
}

fit_met_environment_models <- function(d) {
  lapply(split(d, d$ENV, drop = TRUE), function(local) {
    local <- droplevels(local)
    terms <- c(if (nlevels(local$GEN) > 1L) "GEN", design_terms(local))
    stats::lm(stats::reformulate(if (length(terms)) terms else "1", "Y"), local)
  })
}

lm_reference_statistics <- function(model, L) {
  R <- qr.R(model$qr)
  independent <- seq_len(model$rank)
  dependent <- setdiff(seq_len(ncol(R)), independent)
  basis <- model$qr$pivot[independent]
  omitted <- model$qr$pivot[dependent]
  main <- R[independent, independent, drop = FALSE]
  if (length(omitted)) {
    alias <- backsolve(main, R[independent, dependent, drop = FALSE])
    if (max(abs(L[, omitted, drop = FALSE] - L[, basis, drop = FALSE] %*% alias)) > 1e-7) return(NULL)
  }
  L <- L[, basis, drop = FALSE]
  list(estimate = as.vector(L %*% stats::coef(model)[basis]),
       covariance = L %*% chol2inv(main) %*% t(L))
}

# GEN x ENV and every design term are nested in environment. Factorizing these
# independent blocks is algebraically identical to the global fixed model.
# Return to the general QR path when cell means are not individually estimable.
run_anova_by_environment <- function(d) {
  if (any(table(d$GEN, d$ENV) == 0)) return(NULL)
  fits <- fit_met_environment_models(d)
  pieces <- lapply(names(fits), function(environment) {
    local <- droplevels(d[d$ENV == environment, , drop = FALSE])
    lm_reference_statistics(fits[[environment]], met_reference_matrix(fits[[environment]], local)$gen)
  })
  if (any(vapply(pieces, is.null, logical(1)))) return(NULL)
  rdf <- sum(vapply(fits, stats::df.residual, numeric(1)))
  if (rdf <= 0) stop("No residual degrees of freedom remain for a design-adjusted ANOVA.")
  sse <- sum(vapply(fits, stats::deviance, numeric(1)))
  mse <- sse / rdf
  test <- function(estimates, covariance, label) {
    n <- length(estimates)
    H <- diag(n) - matrix(1/n, n, n)
    ev <- eigen(H %*% covariance %*% H, symmetric = TRUE)
    ok <- ev$values > max(c(ev$values, 1e-12)) * 1e-8
    k <- sum(ok)
    ss <- sum(as.vector(crossprod(ev$vectors[, ok, drop = FALSE], H %*% estimates))^2 / ev$values[ok])
    f <- ss/k/mse
    data.frame(Source = label, Df = k, SumSq = ss, MeanSq = ss/k, F_value = f,
      P_value = stats::pf(f, k, rdf, lower.tail = FALSE), Note = "Adjusted marginal F test")
  }
  n_env <- length(pieces)
  genotype <- test(Reduce(`+`, lapply(pieces, `[[`, "estimate"))/n_env,
    Reduce(`+`, lapply(pieces, `[[`, "covariance"))/n_env^2, "Genotype (G)")
  environment <- test(vapply(pieces, function(x) mean(x$estimate), numeric(1)),
    diag(vapply(pieces, function(x) mean(x$covariance), numeric(1))), "Environment (E)")
  nuisance <- design_terms(d)
  additive <- stats::lm(stats::reformulate(c("GEN", "ENV", nuisance), "Y"), d)
  partial <- function(reduced_sse, reduced_df, label) {
    k <- reduced_df - rdf
    ss <- max(0, reduced_sse - sse)
    f <- if (k > 0) ss/k/mse else NA_real_
    data.frame(Source = label, Df = k, SumSq = ss, MeanSq = if (k > 0) ss/k else NA_real_, F_value = f,
      P_value = if (k > 0) stats::pf(f, k, rdf, lower.tail = FALSE) else NA_real_, Note = "Partial F test conditional on remaining terms")
  }
  rows <- list(genotype, environment, partial(stats::deviance(additive), stats::df.residual(additive), "G x E"))
  if (length(nuisance)) {
    cell_residuals <- d$Y - stats::ave(d$Y, d$GE, FUN = mean)
    rows <- c(rows, list(partial(sum(cell_residuals^2), nrow(d)-nlevels(d$GE), "Design within environment")))
  }
  rows <- c(rows, list(data.frame(Source = "Residuals", Df = rdf, SumSq = sse, MeanSq = mse,
    F_value = NA_real_, P_value = NA_real_, Note = "Plot error")))
  tbl <- do.call(rbind, rows)
  tbl$Signif <- vapply(tbl$P_value, function(p) if (is.na(p)) "" else if (p < .001) "***" else if (p < .01) "**" else if (p < .05) "*" else if (p < .1) "." else "ns", character(1))
  list(anova_table = tbl, model = fits, n_obs = nrow(d), grand_mean = mean(d$Y),
    method = "Design-adjusted marginal F tests; partial sums of squares are not additive",
    formula = paste(deparse(stats::reformulate(c("GEN * ENV", nuisance), "Y")), collapse = " "),
    n_missing = attr(d, "n_missing"), engine = "Independent environment QR blocks")
}

fit_met_mixed <- function(d, genotype_random = TRUE) {
  if (nlevels(d$GE) >= nrow(d)) stop("G x E and plot error cannot be separated without within-cell replication.")
  random <- c(if (genotype_random) "GEN" else "ENV", "GE", design_terms(d))
  random <- random[vapply(random, function(x) nlevels(d[[x]]) > 1L && nlevels(d[[x]]) < nrow(d), logical(1))]
  form <- stats::reformulate(c(if (genotype_random) "ENV" else "GEN", paste0("(1|", random, ")")), "Y")
  lme4::lmer(form, data = d, REML = TRUE, control = lme4::lmerControl(optimizer = "nloptwrap"))
}

# Henderson equations on the spherical scale include fixed-effect estimation
# and remain well-defined when some REML variance estimates reach zero.
genotype_prediction_covariance <- function(model) {
  X <- lme4::getME(model, "X"); Z <- lme4::getME(model, "Z"); Lambda <- lme4::getME(model, "Lambda")
  A <- cbind(Matrix::Matrix(X, sparse = TRUE), Z %*% Lambda)
  Q <- Matrix::crossprod(A) + Matrix::Diagonal(ncol(A), c(rep(0, ncol(X)), rep(1, ncol(Z))))
  gp <- lme4::getME(model, "Gp")
  k <- match("GEN", names(lme4::getME(model, "cnms")))
  idx <- seq.int(gp[k] + 1L, gp[k + 1L])
  mf <- stats::model.frame(model)
  xbar <- colMeans(X[!duplicated(mf$ENV), , drop = FALSE])
  target <- cbind(Matrix::Matrix(matrix(xbar, nrow = length(idx), ncol = ncol(X), byrow = TRUE), sparse = TRUE), Lambda[idx, , drop = FALSE])
  covariance <- as.matrix(stats::sigma(model)^2 * target %*% Matrix::solve(Q, Matrix::t(target)))
  list(covariance = covariance, mean = sum(xbar * lme4::fixef(model)))
}

#' Fit a design-aware mixed MET model and generalized heritability
#' @param df Data frame
#' @param gen_col,env_col,rep_col Genotype, environment and replicate columns
#' @param trait Numeric trait column
#' @param block_col Optional incomplete block column
#' @param design_type Experimental design label
#' @param augmented_checks Replicated checks for augmented designs
#' @return Model, variance components, prediction covariance and Cullis H2
run_mixed_model <- function(df, gen_col, env_col, rep_col, trait, block_col = NULL,
                            design_type = "RCBD", augmented_checks = NULL) {
  d <- prepare_model_data(df, gen_col, env_col, rep_col, trait, block_col, design_type, augmented_checks)
  model <- fit_met_mixed(d)
  vc <- as.data.frame(lme4::VarCorr(model))
  getvar <- function(nm) { x <- vc$vcov[vc$grp == nm]; if (length(x)) x[1] else 0 }
  pred <- genotype_prediction_covariance(model)
  C <- pred$covariance; n <- nrow(C)
  mean_pev_diff <- 2 * (sum(diag(C)) - sum(C)/n) / (n - 1)
  gvar <- getvar("GEN")
  h2 <- if (gvar > 1e-10) max(0, min(1, 1 - mean_pev_diff/(2*gvar))) else NA_real_
  labels <- c(GEN = "Genotype (G)", GE = "G x E", REP_UNIT = "Rep(Env)", BLOCK_UNIT = "Block(Rep, Env)", Residual = "Residual")
  vt <- data.frame(Component = unname(labels[vc$grp]), Variance = vc$vcov)
  vt <- rbind(vt, data.frame(Component = "Total", Variance = sum(vt$Variance)))
  vt$Pct <- 100 * vt$Variance / utils::tail(vt$Variance, 1)
  reps <- vapply(split(d$REP_UNIT, d$ENV), function(x) length(unique(x)), integer(1))
  list(model = model, var_table = vt, H2 = h2, accuracy = sqrt(h2),
       H2_method = "Cullis generalized H2 from mean pairwise prediction-error variance",
       sigma2_G = gvar, sigma2_GE = getvar("GE"), sigma2_E = getvar("Residual"),
       n_env = nlevels(d$ENV), n_rep = if (length(unique(reps)) == 1) unname(reps[1]) else NA_integer_,
       rep_range = range(reps), predictions = pred, n_missing = attr(d, "n_missing"),
       singular = lme4::isSingular(model), formula = paste(deparse(stats::formula(model)), collapse = " "))
}

#' Compute design-adjusted genotype BLUEs with G x E
#' @param df Data frame
#' @param gen_col,env_col,rep_col Genotype, environment and replicate columns
#' @param trait Numeric trait column
#' @param block_col Optional incomplete block column
#' @param design_type Experimental design label
#' @param augmented_checks Replicated checks for augmented designs
#' @param direction Selection direction: higher or lower
#' @return Genotype estimates and confidence intervals
compute_blues <- function(df, gen_col, env_col, rep_col, trait, block_col = NULL,
                          design_type = "RCBD", augmented_checks = NULL, direction = "higher") {
  d <- prepare_model_data(df, gen_col, env_col, rep_col, trait, block_col, design_type, augmented_checks)
  model <- fit_met_mixed(d, genotype_random = FALSE)
  emm <- as.data.frame(emmeans::emmeans(model, "GEN"))
  lower <- intersect(c("lower.CL", "asymp.LCL"), names(emm))[1]
  upper <- intersect(c("upper.CL", "asymp.UCL"), names(emm))[1]
  out <- data.frame(Genotype = emm$GEN, BLUE = emm$emmean, SE = emm$SE, CI_lower = emm[[lower]], CI_upper = emm[[upper]])
  out <- out[order(out$BLUE, decreasing = !identical(direction, "lower")), ]
  out$Rank <- rank(if (identical(direction, "lower")) out$BLUE else -out$BLUE, ties.method = "min")
  attr(out, "model_formula") <- paste(deparse(stats::formula(model)), collapse = " ")
  out
}

#' Compute equal-environment genotype BLUPs and prediction intervals
#' @param df Data frame
#' @param gen_col,env_col,rep_col Genotype, environment and replicate columns
#' @param trait Numeric trait column
#' @param block_col Optional incomplete block column
#' @param design_type Experimental design label
#' @param augmented_checks Replicated checks for augmented designs
#' @param direction Selection direction: higher or lower
#' @param mixed_result Optional existing result from run_mixed_model
#' @return Genotype predictions and approximate prediction intervals
compute_blups <- function(df, gen_col, env_col, rep_col, trait, block_col = NULL,
                          design_type = "RCBD", augmented_checks = NULL, direction = "higher", mixed_result = NULL) {
  m <- mixed_result %||% run_mixed_model(df, gen_col, env_col, rep_col, trait, block_col, design_type, augmented_checks)
  effects <- lme4::ranef(m$model)$GEN
  out <- data.frame(Genotype = rownames(effects), BLUP = m$predictions$mean + effects[, 1],
                     Effect = effects[, 1], SE = sqrt(pmax(0, diag(m$predictions$covariance))))
  out$CI_lower <- out$BLUP - stats::qnorm(.975) * out$SE
  out$CI_upper <- out$BLUP + stats::qnorm(.975) * out$SE
  out <- out[order(out$BLUP, decreasing = !identical(direction, "lower")), ]
  out$Rank <- rank(if (identical(direction, "lower")) out$BLUP else -out$BLUP, ties.method = "min")
  attr(out, "interval_method") <- "Approximate 95% prediction intervals including fixed-effect estimation, conditional on REML variance estimates"
  out
}
