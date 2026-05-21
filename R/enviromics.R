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

# ---------------------------------------------------------------------------
# NASA POWER Weather Fetching & Indexing
# ---------------------------------------------------------------------------

#' Parse dates robustly from various formats
#' @param date_val Vector or single value of dates in char, factor, numeric, or date format
#' @return Date vector
#' @export
parse_date_robustly <- function(date_val) {
  if (is.null(date_val) || length(date_val) == 0) return(as.Date(character(0)))
  
  if (inherits(date_val, c("Date", "POSIXt"))) {
    return(as.Date(date_val))
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
  
  env_col  <- col_names[grep("^env", col_names, ignore.case = TRUE)][1]
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
  
  if (any(is.na(p_dates)) || any(is.na(h_dates))) {
    stop("Could not parse PlantingDate or HarvestDate. Please ensure they are valid dates (e.g. YYYY-MM-DD).", call. = FALSE)
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
