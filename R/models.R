# =============================================================================
# MERIDIAN — Statistical Model Functions
# ANOVA, variance components, BLUEs, BLUPs
# =============================================================================


#' Run two-way ANOVA for MET data (fixed effects)
#' @param df Data frame
#' @param gen_col Genotype column name
#' @param env_col Environment column name
#' @param rep_col Replicate column name
#' @param block_col Optional block column name
#' @param trait Trait column name
#' @param design_type Experimental design label
#' @param augmented_checks Character vector of check genotypes (for augmented)
#' @return List with anova_table, model, type3_table
run_anova_met <- function(df, gen_col, env_col, rep_col, block_col = NULL, trait,
                          design_type = "RCBD", augmented_checks = NULL) {
  # Remove NAs for this trait
  df_clean <- df[!is.na(df[[trait]]), ]

  # Convert to factors
  df_clean[[gen_col]] <- as.factor(df_clean[[gen_col]])
  df_clean[[env_col]] <- as.factor(df_clean[[env_col]])
  df_clean[[rep_col]] <- as.factor(df_clean[[rep_col]])
  if (!is.null(block_col) && block_col %in% names(df_clean)) {
    df_clean[[block_col]] <- as.factor(df_clean[[block_col]])
  }

  qn <- function(x) paste0("`", x, "`")

  # Model terms by design
  if (identical(design_type, "Augmented")) {
    check_vals <- as.character(augmented_checks %||% character(0))
    df_clean$.is_check <- factor(
      ifelse(as.character(df_clean[[gen_col]]) %in% check_vals, "Check", "Test")
    )

    termlabels <- c(
      qn(env_col),
      ".is_check",
      paste0(qn(env_col), ":.is_check"),
      paste0(qn(gen_col), ":.is_check"),
      paste0(qn(env_col), ":", qn(rep_col))
    )
    if (!is.null(block_col) && block_col %in% names(df_clean)) {
      termlabels <- c(termlabels, paste0(qn(env_col), ":", qn(block_col)))
    }
  } else {
    termlabels <- c(
      qn(gen_col),
      qn(env_col),
      paste0(qn(gen_col), ":", qn(env_col)),
      paste0(qn(env_col), ":", qn(rep_col))
    )
    if (!is.null(block_col) && block_col %in% names(df_clean)) {
      termlabels <- c(termlabels, paste0(qn(env_col), ":", qn(block_col)))
    }
  }

  formula_obj <- stats::reformulate(
    termlabels = unique(termlabels),
    response = qn(trait)
  )

  # Fit model
 model <- stats::aov(formula_obj, data = df_clean)

  # Get ANOVA table
  anova_raw <- stats::anova(model)

  # Build clean table
  anova_table <- data.frame(
    Source   = rownames(anova_raw),
    Df      = anova_raw$Df,
    SumSq   = round(anova_raw$`Sum Sq`, 4),
    MeanSq  = round(anova_raw$`Mean Sq`, 4),
    F_value = round(anova_raw$`F value`, 3),
    P_value = anova_raw$`Pr(>F)`,
    stringsAsFactors = FALSE
  )

  # Format source names
  anova_table$Source <- gsub(paste0("`", gen_col, "`"), "Genotype (G)", anova_table$Source)
  anova_table$Source <- gsub(paste0("`", env_col, "`"), "Environment (E)", anova_table$Source)
  anova_table$Source <- gsub(paste0("`", rep_col, "`"), "Rep", anova_table$Source)
  if (!is.null(block_col)) {
    anova_table$Source <- gsub(paste0("`", block_col, "`"), "Block", anova_table$Source)
  }
  anova_table$Source <- gsub("\\.is_check", "CheckStatus", anova_table$Source)
  anova_table$Source <- gsub(":", " x ", anova_table$Source)
  anova_table$Source[nrow(anova_table)] <- "Residuals"

  # Significance stars
  anova_table$Signif <- sapply(anova_table$P_value, function(p) {
    if (is.na(p)) return("")
    if (p < 0.001) return("***")
    if (p < 0.01)  return("**")
    if (p < 0.05)  return("*")
    if (p < 0.1)   return(".")
    return("ns")
  })

  # Percent SS
  total_ss <- sum(anova_table$SumSq)
  anova_table$Pct_SS <- round(100 * anova_table$SumSq / total_ss, 1)

  list(
    anova_table = anova_table,
    model       = model,
    n_obs       = nrow(df_clean),
    grand_mean  = mean(df_clean[[trait]], na.rm = TRUE)
  )
}


#' Fit mixed model with lme4 and extract variance components
#' Model: trait ~ env + (1|gen) + (1|gen:env) + (1|env:rep)
#' @param df Data frame
#' @param gen_col Genotype column name
#' @param env_col Environment column name
#' @param rep_col Replicate column name
#' @param trait Trait column name
#' @return List with var_components, model, heritability
run_mixed_model <- function(df, gen_col, env_col, rep_col, trait) {
  df_clean <- df[!is.na(df[[trait]]), ]

  df_clean[[gen_col]] <- as.factor(df_clean[[gen_col]])
  df_clean[[env_col]] <- as.factor(df_clean[[env_col]])
  df_clean[[rep_col]] <- as.factor(df_clean[[rep_col]])

  qn <- function(x) paste0("`", x, "`")
  formula_obj <- stats::reformulate(
    termlabels = c(
      qn(env_col),
      paste0("(1|", qn(gen_col), ")"),
      paste0("(1|", qn(gen_col), ":", qn(env_col), ")"),
      paste0("(1|", qn(env_col), ":", qn(rep_col), ")")
    ),
    response = qn(trait)
  )

  model <- lme4::lmer(
    formula_obj, 
    data = df_clean,
    control = lme4::lmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)
  )

  # Extract variance components
  vc <- as.data.frame(lme4::VarCorr(model))
  resid_var <- stats::sigma(model)^2

  # Map component names
  sigma2_G   <- 0
  sigma2_GE  <- 0
  sigma2_rep <- 0

  for (i in seq_len(nrow(vc))) {
    grp <- vc$grp[i]
    if (grepl(paste0(gen_col, ":", env_col), grp) ||
        grepl(paste0(env_col, ":", gen_col), grp)) {
      sigma2_GE <- vc$vcov[i]
    } else if (grepl(gen_col, grp) && !grepl(":", grp)) {
      sigma2_G <- vc$vcov[i]
    } else if (grepl(rep_col, grp)) {
      sigma2_rep <- vc$vcov[i]
    }
  }

  n_env <- dplyr::n_distinct(df_clean[[env_col]])
  n_rep <- dplyr::n_distinct(df_clean[[rep_col]])

  # Heritability calculations
  # H2 (broad sense on entry-mean basis)
  H2 <- sigma2_G / (sigma2_G + sigma2_GE / n_env + resid_var / (n_env * n_rep))
  H2 <- max(0, min(1, H2))

  # Accuracy
  accuracy <- sqrt(H2)

  # Build variance components table
  var_table <- data.frame(
    Component = c("Genotype (G)", "G x E", "Rep(Env)", "Residual", "Total"),
    Variance  = round(c(sigma2_G, sigma2_GE, sigma2_rep, resid_var,
                        sigma2_G + sigma2_GE + sigma2_rep + resid_var), 4),
    stringsAsFactors = FALSE
  )
  var_table$Pct <- round(100 * var_table$Variance / var_table$Variance[5], 1)

  list(
    var_table  = var_table,
    model      = model,
    sigma2_G   = sigma2_G,
    sigma2_GE  = sigma2_GE,
    sigma2_E   = resid_var,
    H2         = round(H2, 3),
    accuracy   = round(accuracy, 3),
    n_env      = n_env,
    n_rep      = n_rep
  )
}


#' Compute BLUEs (genotype as fixed, environment as random)
#' @param df Data frame
#' @param gen_col Genotype column name
#' @param env_col Environment column name
#' @param rep_col Replicate column name
#' @param trait Trait column name
#' @return Data frame with Genotype, BLUE, SE, CI_lower, CI_upper
compute_blues <- function(df, gen_col, env_col, rep_col, trait) {
  df_clean <- df[!is.na(df[[trait]]), ]
  df_clean[[gen_col]] <- as.factor(df_clean[[gen_col]])
  df_clean[[env_col]] <- as.factor(df_clean[[env_col]])
  df_clean[[rep_col]] <- as.factor(df_clean[[rep_col]])

  qn <- function(x) paste0("`", x, "`")
  formula_obj <- stats::reformulate(
    termlabels = c(
      qn(gen_col),
      paste0("(1|", qn(env_col), ")"),
      paste0("(1|", qn(env_col), ":", qn(rep_col), ")")
    ),
    response = qn(trait)
  )

  model <- lme4::lmer(
    formula_obj, 
    data = df_clean,
    control = lme4::lmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)
  )

  # Extract estimated marginal means using emmeans
  emm <- emmeans::emmeans(model, specs = gen_col)
  emm_df <- as.data.frame(emm)

  # Rename columns for clarity
  result <- data.frame(
    Genotype  = emm_df[[1]],
    BLUE      = round(emm_df$emmean, 3),
    SE        = round(emm_df$SE, 3),
    CI_lower  = round(emm_df$lower.CL, 3),
    CI_upper  = round(emm_df$upper.CL, 3),
    stringsAsFactors = FALSE
  )

  # Sort by BLUE descending
  result <- result[order(-result$BLUE), ]
  result$Rank <- seq_len(nrow(result))
  rownames(result) <- NULL

  result
}


#' Compute BLUPs (genotype as random, environment as fixed)
#' @param df Data frame
#' @param gen_col Genotype column name
#' @param env_col Environment column name
#' @param rep_col Replicate column name
#' @param trait Trait column name
#' @return Data frame with Genotype, BLUP, SE, CI_lower, CI_upper
compute_blups <- function(df, gen_col, env_col, rep_col, trait) {
  df_clean <- df[!is.na(df[[trait]]), ]
  df_clean[[gen_col]] <- as.factor(df_clean[[gen_col]])
  df_clean[[env_col]] <- as.factor(df_clean[[env_col]])
  df_clean[[rep_col]] <- as.factor(df_clean[[rep_col]])

  qn <- function(x) paste0("`", x, "`")
  formula_obj <- stats::reformulate(
    termlabels = c(
      qn(env_col),
      paste0("(1|", qn(gen_col), ")"),
      paste0("(1|", qn(gen_col), ":", qn(env_col), ")"),
      paste0("(1|", qn(env_col), ":", qn(rep_col), ")")
    ),
    response = qn(trait)
  )

  model <- lme4::lmer(
    formula_obj, 
    data = df_clean,
    control = lme4::lmerControl(optimizer = "nloptwrap", calc.derivs = FALSE)
  )

  # Extract BLUPs (random effect predictions)
  ranef_gen <- lme4::ranef(model)[[gen_col]]
  grand_mean <- lme4::fixef(model)[1]  # intercept

  # Conditional modes + grand mean
  result <- data.frame(
    Genotype = rownames(ranef_gen),
    BLUP     = round(grand_mean + ranef_gen[, 1], 3),
    Effect   = round(ranef_gen[, 1], 3),
    stringsAsFactors = FALSE
  )

  # Get conditional standard deviations
  ranef_cond <- lme4::ranef(model, condVar = TRUE)
  cond_var <- attr(ranef_cond[[gen_col]], "postVar")[1, 1, ]
  result$SE <- round(sqrt(cond_var), 3)
  result$CI_lower <- round(result$BLUP - 1.96 * result$SE, 3)
  result$CI_upper <- round(result$BLUP + 1.96 * result$SE, 3)

  # Sort by BLUP descending
  result <- result[order(-result$BLUP), ]
  result$Rank <- seq_len(nrow(result))
  rownames(result) <- NULL

  result
}
