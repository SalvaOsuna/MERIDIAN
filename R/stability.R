# =============================================================================
# MERIDIAN — Stability Analysis Functions
# Eberhart-Russell, AMMI, GGE, Wricke, Shukla, MTSI
# Uses metan package as the primary analysis engine
# =============================================================================


#' Prepare data for metan functions
#' metan expects specific column names or uses .data pronoun
#' @param df Data frame
#' @param gen_col, env_col, rep_col Column names
#' @return Data frame with standardized column names
prepare_metan_data <- function(df, gen_col, env_col, rep_col, trait = NULL) {
  df_std <- df

  # Drop rows with NA in the trait column to prevent metan internal errors
  # (e.g. weighted.mean dimension mismatch in ge_stats/ammi_indexes)
  if (!is.null(trait) && trait %in% names(df_std)) {
    df_std <- df_std[!is.na(df_std[[trait]]), , drop = FALSE]

    # Ensure perfectly balanced design: equal reps per cell + fully crossed.
    # Step 1: find expected reps (mode) and drop cells with fewer reps
    cell_counts <- table(df_std[[gen_col]], df_std[[env_col]])
    expected_reps <- as.integer(names(which.max(table(cell_counts[cell_counts > 0]))))

    # Keep only rows belonging to cells with the expected number of reps
    bad_cells <- which(cell_counts > 0 & cell_counts != expected_reps, arr.ind = TRUE)
    if (nrow(bad_cells) > 0) {
      bad_gen <- rownames(cell_counts)[bad_cells[, 1]]
      bad_env <- colnames(cell_counts)[bad_cells[, 2]]
      bad_pairs <- paste(bad_gen, bad_env)
      cur_pairs <- paste(df_std[[gen_col]], df_std[[env_col]])
      df_std <- df_std[!cur_pairs %in% bad_pairs, , drop = FALSE]
    }

    # Step 2: iteratively prune to fully crossed design
    changed <- TRUE
    while (changed) {
      changed <- FALSE
      cell_counts <- table(df_std[[gen_col]], df_std[[env_col]])
      n_env <- ncol(cell_counts)
      n_gen <- nrow(cell_counts)

      gen_ok <- rowSums(cell_counts > 0) == n_env
      if (any(!gen_ok)) {
        df_std <- df_std[df_std[[gen_col]] %in% names(which(gen_ok)), , drop = FALSE]
        changed <- TRUE
      }

      env_ok <- colSums(cell_counts > 0) == n_gen
      if (any(!env_ok)) {
        df_std <- df_std[df_std[[env_col]] %in% names(which(env_ok)), , drop = FALSE]
        changed <- TRUE
      }
    }

    # Drop unused factor levels
    df_std[[gen_col]] <- droplevels(as.factor(df_std[[gen_col]]))
    df_std[[env_col]] <- droplevels(as.factor(df_std[[env_col]]))
  }

  # metan works best when we pass column names as strings
  # No renaming needed — metan accepts column names as arguments
  df_std[[gen_col]] <- as.factor(df_std[[gen_col]])
  df_std[[env_col]] <- as.factor(df_std[[env_col]])
  df_std[[rep_col]] <- as.factor(df_std[[rep_col]])
  df_std
}


#' AMMI analysis using metan::performs_ammi()
#' @param df Data frame
#' @param gen_col, env_col, rep_col Column names
#' @param trait Trait column name
#' @return List with model, anova, scores
run_ammi_metan <- function(df, gen_col, env_col, rep_col, trait) {
  df_std <- prepare_metan_data(df, gen_col, env_col, rep_col, trait)

  model <- metan::performs_ammi(
    .data = df_std,
    env   = !!rlang::sym(env_col),
    gen   = !!rlang::sym(gen_col),
    rep   = !!rlang::sym(rep_col),
    resp  = !!rlang::sym(trait)
  )

  # Access internal structure: model$trait_name$ANOVA, $PCA
  trait_data <- model[[trait]]

  ammi_anova <- tryCatch(
    as.data.frame(trait_data$ANOVA),
    error = function(e) NULL
  )

  ipca_scores <- tryCatch(
    as.data.frame(trait_data$PCA),
    error = function(e) NULL
  )

  list(
    model  = model,
    anova  = ammi_anova,
    scores = ipca_scores
  )
}


#' GGE Biplot analysis using metan::gge()
#' @param df Data frame
#' @param gen_col, env_col, rep_col Column names
#' @param trait Trait column name
#' @return List with model, gge object
run_gge <- function(df, gen_col, env_col, rep_col, trait) {
  df_std <- prepare_metan_data(df, gen_col, env_col, rep_col, trait)

  model <- metan::gge(
    .data = df_std,
    env   = !!rlang::sym(env_col),
    gen   = !!rlang::sym(gen_col),
    resp  = !!rlang::sym(trait)
  )

  list(model = model)
}


#' Eberhart & Russell stability regression
#' Uses metan::ge_reg()
#' @param df Data frame
#' @param gen_col, env_col, rep_col Column names
#' @param trait Trait column name
#' @return List with model, regression parameters
run_eberhart_russell <- function(df, gen_col, env_col, rep_col, trait) {
  df_std <- prepare_metan_data(df, gen_col, env_col, rep_col, trait)

  model <- metan::ge_reg(
    .data = df_std,
    env   = !!rlang::sym(env_col),
    gen   = !!rlang::sym(gen_col),
    rep   = !!rlang::sym(rep_col),
    resp  = !!rlang::sym(trait)
  )

  # Extract regression table directly from model internals
  # model$trait_name$regression has: GEN, Y, b1, t(b1=1), Pr>|t|, desvios, etc.
  params <- tryCatch({
    reg_tbl <- as.data.frame(model[[trait]]$regression)
    reg_tbl
  }, error = function(e) {
    # Fallback to get_model_data
    as.data.frame(metan::get_model_data(model))
  })

  list(
    model  = model,
    params = params
  )
}


#' Wricke ecovalence using metan::ecovalence()
#' @param df Data frame
#' @param gen_col, env_col, rep_col Column names
#' @param trait Trait column name
#' @return Data frame with ecovalence values
compute_wricke <- function(df, gen_col, env_col, rep_col, trait) {
  df_std <- prepare_metan_data(df, gen_col, env_col, rep_col, trait)

  model <- metan::ecovalence(
    .data = df_std,
    env   = !!rlang::sym(env_col),
    gen   = !!rlang::sym(gen_col),
    rep   = !!rlang::sym(rep_col),
    resp  = !!rlang::sym(trait)
  )

  params <- metan::get_model_data(model)
  list(model = model, params = params)
}


#' Shukla stability variance using metan::Shukla()
#' @param df Data frame
#' @param gen_col, env_col, rep_col Column names
#' @param trait Trait column name
#' @return Data frame with Shukla variance
compute_shukla <- function(df, gen_col, env_col, rep_col, trait) {
  df_std <- prepare_metan_data(df, gen_col, env_col, rep_col, trait)

  model <- metan::Shukla(
    .data = df_std,
    env   = !!rlang::sym(env_col),
    gen   = !!rlang::sym(gen_col),
    rep   = !!rlang::sym(rep_col),
    resp  = !!rlang::sym(trait)
  )

  params <- metan::get_model_data(model)
  list(model = model, params = params)
}


#' Run all stability indices via metan::ge_stats()
#' @param df Data frame
#' @param gen_col, env_col, rep_col Column names
#' @param trait Trait column name
#' @return List with combined stability stats
run_all_stability <- function(df, gen_col, env_col, rep_col, trait) {
  df_std <- prepare_metan_data(df, gen_col, env_col, rep_col, trait)

  model <- metan::ge_stats(
    .data = df_std,
    env   = !!rlang::sym(env_col),
    gen   = !!rlang::sym(gen_col),
    rep   = !!rlang::sym(rep_col),
    resp  = !!rlang::sym(trait)
  )

  stats_table <- metan::get_model_data(model)

  list(
    model       = model,
    stats_table = stats_table
  )
}


#' Combine stability rankings into one table
#' @param stats_table Output from run_all_stability()
#' @return Sorted data frame with multi-criteria ranking
stability_ranking <- function(stats_table) {
  if (is.null(stats_table)) return(NULL)

  # stats_table from ge_stats contains all indices
  # Select key columns and add a composite rank
  key_cols <- intersect(
    c("GEN", "Y", "Wi", "ShuklaVar", "bi", "S2di", "AMMI_SS", "ASV", "HMGV", "RPGV", "HMRPGV"),
    names(stats_table)
  )

  if (length(key_cols) == 0) return(stats_table)

  ranking <- stats_table[, key_cols, drop = FALSE]

  # Round numeric columns
  num_cols <- sapply(ranking, is.numeric)
  ranking[num_cols] <- lapply(ranking[num_cols], round, 3)

  ranking
}
