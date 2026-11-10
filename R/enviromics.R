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
#' @param direction Selection objective: higher or lower
run_mega_envs <- function(df, gen_col, env_col, trait, direction = "higher") {
  d <- data.frame(GEN = as.character(df[[gen_col]]), ENV = as.character(df[[env_col]]), Y = df[[trait]])
  d <- d[is.finite(d$Y), , drop = FALSE]
  means <- stats::aggregate(Y ~ GEN + ENV, d, mean)
  out <- do.call(rbind, lapply(split(means, means$ENV), function(x) {
    best <- if (identical(direction, "lower")) min(x$Y) else max(x$Y)
    winners <- paste(sort(x$GEN[abs(x$Y-best) < 1e-10]), collapse = ", ")
    data.frame(ENV = x$ENV[1], MEGA_ENV = paste0("Winner: ", winners), MEAN = best,
               Winning_Genotype = winners, N_Genotypes = nrow(x),
               Coverage = nrow(x)/length(unique(d$GEN)))
  }))
  list(env_strat = out, method = "Exploratory grouping by observed winner; unequal coverage is reported, not validated mega-environments", direction = direction)
}

#' Run Finlay-Wilkinson Reaction Norms
#' Computes the environmental index and fits regressions for each genotype.
#' @param df Data frame
#' @param gen_col Genotype column name
#' @param env_col Environment column name
#' @param trait Trait column name
#' @return Data frame with fitted lines and scatter data
run_finlay_wilkinson <- function(df, gen_col, env_col, trait) {
  input <- prepare_gge_data(df, gen_col, env_col, trait)
  M <- input$matrix
  env_mean <- colMeans(M)
  index <- env_mean - mean(env_mean)
  d <- data.frame(GEN = as.character(df[[gen_col]]), ENV = as.character(df[[env_col]]), mean_val = df[[trait]])
  d <- stats::aggregate(mean_val ~ GEN + ENV, d, mean)
  d$EI <- unname(index[as.character(d$ENV)])
  fits <- lapply(split(d, d$GEN), function(x) {
    fit <- if (nrow(x) >= 2 && stats::var(x$EI) > 1e-12) stats::lm(mean_val ~ EI, x) else NULL
    data.frame(GEN = x$GEN[1], slope = if (!is.null(fit)) unname(stats::coef(fit)[2]) else NA_real_,
               intercept = if (!is.null(fit)) unname(stats::coef(fit)[1]) else mean(x$mean_val), N_Envs = nrow(x))
  })
  slopes <- do.call(rbind, fits)
  names(d)[1:2] <- c(gen_col, env_col); names(slopes)[1] <- gen_col
  env_means <- data.frame(ENV = names(env_mean), env_mean = unname(env_mean), EI = unname(index))
  names(env_means)[1] <- env_col
  list(fw_data = d, gen_slopes = slopes, env_means = env_means, n_imputed = input$n_imputed,
       method = "Regression uses observed means only; environmental index uses the completed additive-imputation table")
}


# ---------------------------------------------------------------------------
# Enviromics Strategies (Covariate Based)
# ---------------------------------------------------------------------------

#' Compute Environmental Typology (PCA)
#' @param env_data Data frame with environmental covariates
#' @param env_col Column name representing environments
#' @return prcomp object and formatted data
compute_env_pca <- function(env_data, env_col) {
  validate_covariate_keys(env_data, env_col)
  cols <- setdiff(names(env_data)[vapply(env_data, is.numeric, logical(1))], env_col)
  cols <- cols[vapply(env_data[cols], function(x) sum(is.finite(x)) >= 2 && stats::sd(x[is.finite(x)]) > 0, logical(1))]
  num <- env_data[cols]
  keep <- if (length(cols)) apply(as.matrix(num), 1, function(x) all(is.finite(x))) else rep(FALSE, nrow(env_data))
  num <- num[keep, , drop = FALSE]
  if (ncol(num) < 2 || nrow(num) < 2) return(NULL)
  nonconstant <- vapply(num, function(x) stats::sd(x) > 0, logical(1))
  num <- num[nonconstant]
  if (ncol(num) < 2) return(NULL)
  list(pca_res = stats::prcomp(num, scale. = TRUE), env_labels = env_data[[env_col]][keep],
       num_data = num, excluded_environments = env_data[[env_col]][!keep], excluded_columns = setdiff(cols, names(num)))
}


#' Compute Covariate-Phenotype Correlations
#' @param env_data Environmental covariates
#' @param env_means Data frame from run_finlay_wilkinson (contains env_mean)
#' @param env_col Column name for environments
#' @return Correlation matrix
compute_covariate_correlations <- function(env_data, env_means, env_col) {
  validate_covariate_keys(env_data, env_col)
  validate_covariate_keys(env_means, env_col)
  merged <- dplyr::inner_join(env_data, env_means, by = env_col)
  cols <- setdiff(names(merged)[vapply(merged, is.numeric, logical(1))], env_col)
  num <- merged[cols]
  num[] <- lapply(num, function(x) { x[!is.finite(x)] <- NA_real_; x })
  keep <- vapply(num, function(x) sum(!is.na(x)) >= 2 && stats::sd(x, na.rm = TRUE) > 0, logical(1))
  num <- num[keep]
  if (ncol(num) < 2 || nrow(num) < 3) return(NULL)
  list(cor_mat = stats::cor(num, use = "pairwise.complete.obs"), merged_data = merged,
       pair_counts = crossprod(!is.na(as.matrix(num))))
}

validate_covariate_keys <- function(data, env_col) {
  if (!env_col %in% names(data)) stop("Environmental covariates must include the mapped environment column: ", env_col)
  x <- data[[env_col]]
  if (anyNA(x) || any(!nzchar(trimws(as.character(x)))) || anyDuplicated(x))
    stop("Environmental covariates require one uniquely identified row per environment.")
  invisible(TRUE)
}

# ---------------------------------------------------------------------------
# NASA POWER Weather Fetching & Indexing
# ---------------------------------------------------------------------------

#' Parse dates robustly from various formats
#' @param date_val Vector or single value of dates in char, factor, numeric, or date format
#' @return Date vector
#' @export
parse_date_robustly <- function(date_val) {
  if (is.null(date_val) || length(date_val) == 0) return(as.Date(character(0)))

  if (inherits(date_val, "Date")) {
    return(as.Date(date_val))
  }
  if (inherits(date_val, "POSIXt")) {
    tz <- attr(date_val, "tzone")
    if (!length(tz) || !nzchar(tz[1])) tz <- ""
    return(as.Date(date_val, tz = tz[1]))
  }

  date_char <- as.character(date_val)
  parsed_dates <- rep(as.Date(NA), length(date_char))

  formats_to_try <- c(
    "%Y-%m-%d", "%Y/%m/%d", "%d/%m/%Y", "%m/%d/%Y",
    "%Y%m%d", "%d-%m-%Y", "%m-%d-%Y"
  )

  for (i in seq_along(date_char)) {
    val <- date_char[i]
    if (is.na(val) || trimws(val) == "") next

    if (suppressWarnings(!is.na(as.numeric(val)))) {
      num_val <- as.numeric(val)
      if (num_val > 10000 && num_val < 60000) {
        parsed_dates[i] <- as.Date(num_val, origin = "1899-12-30")
        next
      }
    }

    for (fmt in formats_to_try) {
      d <- as.Date(val, format = fmt)
      if (!is.na(d)) {
        yr <- as.numeric(format(d, "%Y"))
        if (!is.na(yr) && yr >= 1900 && yr <= 2100) {
          parsed_dates[i] <- d
          break
        }
      }
    }

    if (is.na(parsed_dates[i])) {
      d <- tryCatch(as.Date(val), error = function(e) as.Date(NA))
      if (!is.na(d)) {
        yr <- as.numeric(format(d, "%Y"))
        if (!is.na(yr) && yr >= 1900 && yr <= 2100) {
          parsed_dates[i] <- d
        }
      }
    }
  }

  parsed_dates
}

#' Fetch daily weather parameters from NASA POWER API
#' @param lat Numeric latitude
#' @param lon Numeric longitude
#' @param start_date R Date object
#' @param end_date R Date object
#' @return A data frame with daily weather variables, or NULL if query fails
#' @export
fetch_nasa_weather <- function(lat, lon, start_date, end_date) {
  if (is.na(lat) || is.na(lon) || is.na(start_date) || is.na(end_date)) {
    return(NULL)
  }

  s_str <- format(start_date, "%Y-%m-%d")
  e_str <- format(end_date, "%Y-%m-%d")

  max_retries <- 2
  daily_weather <- NULL

  for (attempt in seq_len(max_retries + 1)) {
    daily_weather <- tryCatch({
      nasapower::get_power(
        community = "ag",
        lonlat = c(as.numeric(lon), as.numeric(lat)),
        pars = c("T2M_MAX", "T2M_MIN", "PRECTOTCORR", "RH2M", "ALLSKY_SFC_SW_DWN"),
        dates = c(s_str, e_str),
        temporal_api = "daily"
      )
    }, error = function(e) {
      if (attempt <= max_retries) {
        Sys.sleep(1)
      }
      NULL
    })

    if (!is.null(daily_weather)) break
  }

  if (is.null(daily_weather) || nrow(daily_weather) == 0) {
    stop("NASA POWER API call failed or is unavailable. Please check your internet connection.", call. = FALSE)
  }

  as.data.frame(daily_weather)
}

#' Calculate summary agroclimatic indices from daily weather series
#' @param daily_weather Data frame containing daily variables
#' @param dtf_mean Mean days to flowering (optional)
#' @param dtm_mean Mean days to maturity (optional)
#' @return A single-row data frame with computed indices
#' @export
calculate_enviromic_indices <- function(daily_weather, dtf_mean = NULL, dtm_mean = NULL) {
  if (is.null(daily_weather) || nrow(daily_weather) == 0) {
    return(data.frame(
      MeanTemp_C = as.numeric(NA),
      TotalRainfall_mm = as.numeric(NA),
      GDD = as.numeric(NA),
      MeanRH_pct = as.numeric(NA),
      TotalSolar_MJ = as.numeric(NA)
    ))
  }

  tmax <- as.numeric(daily_weather$T2M_MAX)
  tmin <- as.numeric(daily_weather$T2M_MIN)
  precip <- as.numeric(daily_weather$PRECTOTCORR)
  rh <- as.numeric(daily_weather$RH2M)
  solar <- as.numeric(daily_weather$ALLSKY_SFC_SW_DWN)

  avg_temp <- (tmax + tmin) / 2
  mean_temp <- mean(avg_temp, na.rm = TRUE)
  total_rainfall <- sum(precip, na.rm = TRUE)
  total_gdd <- sum(pmax(avg_temp - 10, 0), na.rm = TRUE)
  mean_rh <- mean(rh, na.rm = TRUE)
  total_solar <- sum(solar, na.rm = TRUE)

  res <- data.frame(
    MeanTemp_C = round(mean_temp, 2),
    TotalRainfall_mm = round(total_rainfall, 2),
    GDD = round(total_gdd, 2),
    MeanRH_pct = round(mean_rh, 2),
    TotalSolar_MJ = round(total_solar, 2),
    stringsAsFactors = FALSE
  )

  # Stage divisions
  n_days <- nrow(daily_weather)
  has_dtf <- !is.null(dtf_mean) && !is.na(dtf_mean) && dtf_mean > 0
  has_dtm <- !is.null(dtm_mean) && !is.na(dtm_mean) && dtm_mean > 0

  if (has_dtf) {
    dtf_idx <- min(n_days, round(dtf_mean))

    # Vegetative phase: 1 to dtf_idx
    veg_avg_temp <- avg_temp[1:dtf_idx]
    veg_precip <- precip[1:dtf_idx]

    res$GDD_Veg <- round(sum(pmax(veg_avg_temp - 10, 0), na.rm = TRUE), 2)
    res$TotalRainfall_Veg <- round(sum(veg_precip, na.rm = TRUE), 2)
    res$MeanTemp_Veg <- round(mean(veg_avg_temp, na.rm = TRUE), 2)

    rep_start <- min(n_days + 1, dtf_idx + 1)
  } else {
    rep_start <- 1
  }

  if (has_dtm) {
    dtm_idx <- min(n_days, round(dtm_mean))
  } else {
    dtm_idx <- n_days
  }

  # Reproductive phase runs from rep_start to dtm_idx
  if (has_dtf || has_dtm) {
    if (rep_start <= dtm_idx) {
      rep_indices <- rep_start:dtm_idx
      rep_avg_temp <- avg_temp[rep_indices]
      rep_precip <- precip[rep_indices]
      rep_tmax <- tmax[rep_indices]

      res$GDD_Rep <- round(sum(pmax(rep_avg_temp - 10, 0), na.rm = TRUE), 2)
      res$TotalRainfall_Rep <- round(sum(rep_precip, na.rm = TRUE), 2)
      res$MeanTemp_Rep <- round(mean(rep_avg_temp, na.rm = TRUE), 2)
      res$HeatStressDays_Rep <- sum(rep_tmax > 30, na.rm = TRUE)
    } else {
      res$GDD_Rep <- 0
      res$TotalRainfall_Rep <- 0
      res$MeanTemp_Rep <- as.numeric(NA)
      res$HeatStressDays_Rep <- 0
    }
  }

  # Late phase runs from dtm_idx + 1 to n_days if dtm is earlier than harvest
  if (has_dtm) {
    if (dtm_idx < n_days) {
      late_indices <- (dtm_idx + 1):n_days
      late_avg_temp <- avg_temp[late_indices]
      late_precip <- precip[late_indices]

      res$GDD_Late <- round(sum(pmax(late_avg_temp - 10, 0), na.rm = TRUE), 2)
      res$TotalRainfall_Late <- round(sum(late_precip, na.rm = TRUE), 2)
      res$MeanTemp_Late <- round(mean(late_avg_temp, na.rm = TRUE), 2)
    } else {
      res$GDD_Late <- 0
      res$TotalRainfall_Late <- 0
      res$MeanTemp_Late <- as.numeric(NA)
    }
  }

  res
}

#' Process environmental covariates table and calculate derived indices
#' @param env_data Input data frame with Environment, Latitude, Longitude, PlantingDate, HarvestDate
#' @param dtf_means Optional named numeric vector of mean days to flowering per environment
#' @param dtm_means Optional named numeric vector of mean days to maturity per environment
#' @param session Optional Shiny session object
#' @return Processed data frame with computed environmental indices
#' @export
process_environmental_covariates <- function(env_data, dtf_means = NULL, dtm_means = NULL, session = NULL) {
  if (is.null(env_data) || nrow(env_data) == 0) return(env_data)

  col_names <- names(env_data)

  env_col  <- col_names[grep("^(env|site|loc)", col_names, ignore.case = TRUE)][1]
  lat_col  <- col_names[grep("^lat", col_names, ignore.case = TRUE)][1]
  lon_col  <- col_names[grep("^lon", col_names, ignore.case = TRUE)][1]
  pdate_col <- col_names[grep("plant", col_names, ignore.case = TRUE)][1]
  hdate_col <- col_names[grep("harvest", col_names, ignore.case = TRUE)][1]

  if (is.na(env_col)) stop("An 'Environment' column is required in the environmental data.", call. = FALSE)
  if (is.na(lat_col) || is.na(lon_col) || is.na(pdate_col) || is.na(hdate_col)) {
    return(env_data)
  }

  p_dates <- parse_date_robustly(env_data[[pdate_col]])
  h_dates <- parse_date_robustly(env_data[[hdate_col]])

  if (any(is.na(p_dates))) {
    stop("Could not parse PlantingDate. Please ensure all environments have a valid PlantingDate (e.g. YYYY-MM-DD).", call. = FALSE)
  }

  # Dynamic imputation for missing HarvestDates
  if (any(is.na(h_dates))) {
    valid_idx <- !is.na(h_dates) & !is.na(p_dates)
    mean_dur <- 120
    if (sum(valid_idx) > 0) {
      durations <- as.numeric(h_dates[valid_idx] - p_dates[valid_idx])
      valid_durs <- durations[durations >= 30 & durations <= 200]
      if (length(valid_durs) > 0) {
        mean_dur <- round(mean(valid_durs))
      } else {
        mean_dur <- round(mean(durations))
        if (mean_dur < 30 || mean_dur > 200) mean_dur <- 120
      }
    }

    missing_idx <- which(is.na(h_dates))
    for (idx in missing_idx) {
      h_dates[idx] <- p_dates[idx] + mean_dur
    }
  }

  if (any(is.na(h_dates))) {
    stop("Could not parse HarvestDate. Please ensure they are valid dates (e.g. YYYY-MM-DD).", call. = FALSE)
  }

  indices_list <- list()
  n_envs <- nrow(env_data)

  for (i in seq_len(n_envs)) {
    env_name <- as.character(env_data[[env_col]][i])
    lat <- as.numeric(env_data[[lat_col]][i])
    lon <- as.numeric(env_data[[lon_col]][i])
    s_date <- p_dates[i]
    e_date <- h_dates[i]

    if (!is.null(session) && exists("incProgress", where = "package:shiny")) {
      shiny::incProgress(
        amount = 1 / n_envs,
        detail = paste("Fetching weather for", env_name)
      )
    }

    # Extract dtf and dtm for this specific environment
    dtf_val <- if (!is.null(dtf_means) && env_name %in% names(dtf_means)) dtf_means[[env_name]] else NULL
    dtm_val <- if (!is.null(dtm_means) && env_name %in% names(dtm_means)) dtm_means[[env_name]] else NULL

    # Clean/check values
    if (!is.null(dtf_val) && is.na(dtf_val)) dtf_val <- NULL
    if (!is.null(dtm_val) && is.na(dtm_val)) dtm_val <- NULL

    daily_w <- fetch_nasa_weather(lat, lon, s_date, e_date)
    indices_df <- calculate_enviromic_indices(daily_w, dtf_mean = dtf_val, dtm_mean = dtm_val)
    indices_df[[env_col]] <- env_name
    indices_list[[i]] <- indices_df
  }

  computed_indices <- dplyr::bind_rows(indices_list)

  # Dynamically determine the calculated names
  all_cols <- names(computed_indices)
  calculated_names <- setdiff(all_cols, env_col)

  original_extra <- env_data |>
    dplyr::select(-dplyr::any_of(c(lat_col, lon_col, pdate_col, hdate_col)))

  original_extra <- original_extra |>
    dplyr::select(-dplyr::any_of(calculated_names))

  geo_date_info <- data.frame(
    Environment = env_data[[env_col]],
    Latitude = env_data[[lat_col]],
    Longitude = env_data[[lon_col]],
    PlantingDate = p_dates,
    HarvestDate = h_dates,
    stringsAsFactors = FALSE
  )
  names(geo_date_info)[1] <- env_col

  res_df <- computed_indices |>
    dplyr::inner_join(geo_date_info, by = env_col) |>
    dplyr::inner_join(original_extra, by = env_col)

  std_names <- c(env_col, "Latitude", "Longitude", "PlantingDate", "HarvestDate", calculated_names)
  extra_names <- setdiff(names(res_df), std_names)

  res_df <- res_df |>
    dplyr::select(dplyr::all_of(c(std_names, extra_names)))

  as.data.frame(res_df)
}
