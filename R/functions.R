
# Data Preprocessing ------------------------------------------------------

#' Load and Clean Emergency Department Data
#'
#' Imports the Excel file, selects columns based on known prefixes,
#' and reshapes into a long format suitable for modeling.
#'
#' @param file_path String. Path to the .xlsx file.
#' @return A tibble with columns: Data_inizio_sett, area, sindrome, casi, week_num, etc.
#' @export
load_and_process_data <- function(file_path) {
  
  library(tidyverse)
  library(readxl)
  library(lubridate)
  
  
  df_raw <- readxl::read_excel(file_path)
  
  prefix_map <- list(
    "Lombardia"  = "N_accessi_RL_",
    "Milano"     = "N_accessi_ASST_Milanesi_",
    "Valtellina" = "N_accessi_ASST_Valtellina_"
  )
  
  all_areas <- list()
  
  for (area_name in names(prefix_map)) {
    prefix <- prefix_map[[area_name]]
    
    clean_area <- df_raw |>
      dplyr::select(Data_inizio_sett, dplyr::starts_with(prefix)) |>
      dplyr::rename_with(~ stringr::str_remove(.x, prefix), dplyr::starts_with(prefix)) |>
      tidyr::pivot_longer(cols = -Data_inizio_sett, names_to = "sindrome", values_to = "casi") |>
      dplyr::mutate(
        Data_inizio_sett = as.Date(Data_inizio_sett),
        area = area_name
      ) |>
      dplyr::arrange(sindrome, Data_inizio_sett) |>
      dplyr::group_by(sindrome) |>
      dplyr::mutate(
        week_num = dplyr::row_number(),
        week_of_year = lubridate::isoweek(Data_inizio_sett),
        dow = lubridate::wday(Data_inizio_sett, label = TRUE, week_start = 1) |> 
          as.factor() |> 
          forcats::fct_relevel("Mon")
      ) |>
      dplyr::ungroup()
    
    all_areas[[area_name]] <- clean_area
  }
  
  return(dplyr::bind_rows(all_areas))
}


#' Prepare All Data with Deltas (Grouped by Area)
#'
#' Adds delta calculations and returns a LIST named by area.
#'
#' @param clean_data Output from load_and_process_data
#' @return A named list of dataframes, one per area
#' @export
prepare_all_data_list <- function(clean_data) {
  clean_data |>
    dplyr::group_by(area, sindrome) |>
    dplyr::mutate(delta_casi = casi - dplyr::lag(casi)) |>
    dplyr::ungroup() |>
    split(~area)
}


# Exploratory Analysis ----------------------------------------------------

#' Calculate Exploratory Stats (Overdispersion & ACF)
#'
#' @param df_long Long-format dataframe with delta_casi column
#' @return A list containing overdispersion and acf dataframes
#' @export
get_exploratory_stats <- function(df_long) {
  
  overdisp <- df_long |>
    dplyr::group_by(sindrome) |>
    dplyr::summarise(
      n_weeks = dplyr::n(),
      mean_delta = mean(delta_casi, na.rm = TRUE),
      var_delta = stats::var(delta_casi, na.rm = TRUE),
      overdisp_index = var_delta / abs(mean_delta),
      prop_zeros = mean(delta_casi == 0, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(overdisp_index))
  
  acf_data <- df_long |>
    dplyr::group_by(sindrome) |>
    dplyr::summarise(
      acf_vals = list(stats::acf(delta_casi[!is.na(delta_casi)], lag.max = 10, plot = FALSE)$acf[-1]),
      lags = list(1:10),
      .groups = "drop"
    ) |>
    tidyr::unnest(c(acf_vals, lags))
  
  list(overdispersion = overdisp, acf = acf_data)
}


#' Calculate Historical Baselines
#'
#' Computes static benchmarks (Min/Max, Naive Poisson).
#'
#' @param df_long Long-format dataframe
#' @return A tibble with baseline statistics by syndrome
#' @export
get_baselines <- function(df_long) {
  df_long |>
    dplyr::group_by(sindrome) |>
    dplyr::summarise(
      roll_max_casi = max(utils::tail(casi, 52 * 2), na.rm = TRUE),
      roll_min_casi = min(utils::tail(casi, 52 * 2), na.rm = TRUE),
      poisson_mean  = mean(casi, na.rm = TRUE),
      p95_delta     = stats::quantile(utils::tail(delta_casi, 52), 0.95, na.rm = TRUE),
      p99_delta     = stats::quantile(utils::tail(delta_casi, 52), 0.99, na.rm = TRUE),
      .groups = "drop"
    )
}


#' Calculate Stats for All Areas
#' @export
calculate_all_stats <- function(data_list) {
  purrr::map(data_list, get_exploratory_stats)
}


#' Calculate Baselines for All Areas
#' @export
calculate_all_baselines <- function(data_list) {
  purrr::map(data_list, get_baselines)
}


# Utilities ---------------------------------------------------------------

#' Define Flat (Non-Seasonal) Syndromes
#'
#' Returns the vector of syndrome names that require stiff/conservative modeling.
#'
#' @return Character vector of flat syndrome names
#' @export
get_flat_syndromes <- function() {
  
  c("alt_cosc", "conv", "dec_gravi", "int_farm", "rash_cut")
}

#' Create Univariate STS Object for Single Syndrome
#'
#' Helper that creates sts inside the worker to avoid S4 serialization issues.
#'
#' @param syn_data Dataframe for a single syndrome
#' @return A surveillance::sts object
#' @keywords internal
create_univariate_sts <- function(syn_data) {
  library(surveillance)
  
  syn_data <- syn_data |> dplyr::arrange(Data_inizio_sett)
  
  obs_matrix <- as.matrix(as.integer(syn_data$casi))
  dimnames(obs_matrix) <- NULL
  
  surveillance::sts(
    observed = obs_matrix,
    epoch = as.numeric(syn_data$Data_inizio_sett),
    epochAsDate = TRUE,
    frequency = 365
  )
}


#' Generate Synthetic Outbreak Shape
#'
#' Creates a sigmoidal outbreak curve for sensitivity analysis.
#' Mimics realistic biological transmission (slow start, exponential growth, plateau).
#'
#' @param total_cases Total number of cases to distribute across the outbreak
#' @param duration Duration of outbreak in days
#' @param cumulative If TRUE, returns 7-day rolling cumulative; if FALSE, daily incident
#' @return Numeric vector of case counts per day
#' @export
generate_outbreak_shape <- function(total_cases, duration = 14, cumulative = TRUE) {
  t <- seq(-6, 6, length.out = duration)
  curve <- 1 / (1 + exp(-t))
  daily_incident <- (curve / sum(curve)) * total_cases
  
  if (cumulative && duration >= 7) {
    rolling_curve <- numeric(duration)
    window_size <- min(7, duration)
    for (i in 1:duration) {
      start_idx <- max(1, i - (window_size - 1))
      rolling_curve[i] <- sum(daily_incident[start_idx:i])
    }
    final_curve <- rolling_curve
  } else {
    final_curve <- daily_incident
  }
  
  return(round(final_curve))
}


#' Create Multivariate STS Object
#'
#' Builds a surveillance::sts object with all syndromes as columns.
#' This is the proper way to use the surveillance package - one matrix, not loops.
#'
#' @param area_data Dataframe filtered for a single area
#' @return A surveillance::sts object with multivariate observed matrix
#' @export
create_sts_object <- function(area_data) {
  library(surveillance)
  
  
  # Pivot to wide format: rows = dates, columns = syndromes
  wide_data <- area_data |>
    dplyr::select(Data_inizio_sett, sindrome, casi) |>
    tidyr::pivot_wider(names_from = sindrome, values_from = casi) |>
    dplyr::arrange(Data_inizio_sett)
  
  dates <- wide_data$Data_inizio_sett
  obs_matrix <- as.matrix(wide_data |> dplyr::select(-Data_inizio_sett))
  
  # Ensure integer counts
  
  obs_matrix <- apply(obs_matrix, 2, as.integer)
  
  # Create sts object
  sts_obj <- surveillance::sts(
    observed = obs_matrix,
    epoch = as.numeric(dates),
    epochAsDate = TRUE,
    frequency = 365
  )
  
  return(sts_obj)
}


#' Get Validation Range Indices
#'
#' Returns the row indices for the validation/test period.
#'
#' @param dates Vector of dates
#' @param validation_start Start date for validation period
#' @return Integer vector of indices
#' @export
get_validation_indices <- function(dates, validation_start) {
  which(dates > validation_start)
}

# Models ------------------------------------------------------------------

#' Fit GAM Model Based on Syndrome Type
#'
#' Automatically selects appropriate GAM flexibility based on
#' whether the syndrome is highly seasonal or flat/random.
#'
#' @param train_df Dataframe containing the historical training data
#' @param syndrome_name Character string of the current syndrome
#' @param flat_syndromes Character vector of syndromes requiring stiff model
#' @return A fitted mgcv::gam object
#' @export
fit_syndrome_gam <- function(train_df, syndrome_name, 
                             flat_syndromes = get_flat_syndromes()) {
  if (syndrome_name %in% flat_syndromes) {
    # STIFF MODEL (Low Seasonality)
    mgcv::gam(casi ~ s(week_num, bs = "ts", k = 3) + dow,
              data = train_df, family = mgcv::nb())
  } else {
    # FLEXIBLE MODEL (High Seasonality)
    mgcv::gam(casi ~ s(week_num, bs = "ts", k = 20) + 
                s(week_of_year, bs = "cc", k = 12) + dow,
              data = train_df, family = mgcv::nb())
  }
}


#' Calculate Adaptive Bias Factor
#'
#' Computes the 14-day trailing bias correction factor.
#'
#' @param train_df Training dataframe (must have 'casi' column)
#' @param model Fitted GAM model
#' @param syndrome_name Name of syndrome
#' @param window_size Number of trailing days for bias calculation
#' @return Numeric bias factor (defaults to 1 if calculation fails)
#' @export
calculate_adaptive_bias <- function(train_df, model, syndrome_name, window_size = 14) {
  if (syndrome_name %in% get_flat_syndromes()) {
    return(1)
  }
  
  recent <- utils::tail(train_df, window_size)
  if (nrow(recent) < window_size / 2) return(1)
  
  recent_pred <- stats::predict(model, newdata = recent, type = "response")
  bias <- mean(recent$casi, na.rm = TRUE) / mean(recent_pred, na.rm = TRUE)
  
  if (is.na(bias) || is.infinite(bias) || bias <= 0) {
    return(1)
  }
  
  return(bias)
}


#' Run Operational Model (Recent Horizon)
#'
#' Fits GAM models and generates adaptive forecasts for the most recent period.
#'
#' @param clean_data The dataframe from load_and_process_data
#' @param report_window_days Number of days for the report window (default: 60)
#' @return A list of dataframes, named by area
#' @export
run_operational_model <- function(clean_data, report_window_days = 60) {
  library(mgcv)
  
  areas <- unique(clean_data$area)
  flat_syndromes <- get_flat_syndromes()
  results_list <- list()
  
  for (a in areas) {
    area_data <- clean_data |> dplyr::filter(area == a)
    syndromes <- unique(area_data$sindrome)
    report_window_start <- max(area_data$Data_inizio_sett) - lubridate::days(report_window_days)
    
    area_preds <- purrr::map_dfr(syndromes, function(s) {
      syn_data <- area_data |> dplyr::filter(sindrome == s)
      train_df <- syn_data |> dplyr::filter(Data_inizio_sett < report_window_start)
      test_df  <- syn_data |> dplyr::filter(Data_inizio_sett >= report_window_start)
      
      if (nrow(train_df) < 30 || nrow(test_df) == 0) return(NULL)
      
      # Fit model
      m <- fit_syndrome_gam(train_df, s, flat_syndromes)
      theta <- m$family$getTheta(TRUE)
      
      # Predict and apply bias
      raw_pred <- stats::predict(m, newdata = test_df, type = "response")
      bias <- calculate_adaptive_bias(train_df, m, s)
      adaptive_pred <- raw_pred * bias
      
      dplyr::tibble(
        Data_inizio_sett = test_df$Data_inizio_sett,
        sindrome = s,
        casi = test_df$casi, 
        expected = as.numeric(adaptive_pred),
        limit_95 = stats::qnbinom(0.95, mu = as.numeric(adaptive_pred), size = theta),
        limit_99 = stats::qnbinom(0.99, mu = as.numeric(adaptive_pred), size = theta)
      )
    })
    
    results_list[[a]] <- area_preds
  }
  return(results_list)
}

#' Run Retrospective Rolling Validation (GAM) - PARALLEL
#'
#' @param clean_data The dataframe from load_and_process_data
#' @param validation_days Number of days for validation period (default: 365)
#' @param workers Number of parallel workers (default: 14)
#' @return A list of dataframes, named by area
#' @export
run_rolling_validation <- function(clean_data, validation_days = 365, workers = 14) {
  library(mgcv)
  library(furrr)
  
  future::plan(future::multisession, workers = workers)
  on.exit(future::plan(future::sequential), add = TRUE)
  
  areas <- unique(clean_data$area)
  flat_syndromes <- get_flat_syndromes()
  results_list <- list()
  
  for (a in areas) {
    cat("Processing area:", a, "\n")
    
    area_data <- clean_data |> dplyr::filter(area == a)
    syndromes <- unique(area_data$sindrome)
    validation_start <- max(area_data$Data_inizio_sett) - lubridate::days(validation_days)
    
    # PARALLEL: Process syndromes in parallel
    area_val <- furrr::future_map_dfr(syndromes, function(s) {
      # All processing happens inside the worker - no S4 objects passed
      syn_data <- area_data |> dplyr::filter(sindrome == s)
      val_indices <- which(syn_data$Data_inizio_sett >= validation_start)
      
      if (length(val_indices) == 0) return(NULL)
      
      preds_list <- list()
      m <- NULL
      theta <- NULL
      
      for (idx in val_indices) {
        current_date <- syn_data$Data_inizio_sett[idx]
        train_df <- syn_data |> dplyr::filter(Data_inizio_sett < current_date)
        
        if (nrow(train_df) < 30) next
        
        # Refit model monthly
        if (lubridate::day(current_date) == 1 || idx == val_indices[1]) {
          m <- fit_syndrome_gam(train_df, s, flat_syndromes)
          theta <- m$family$getTheta(TRUE)
        }
        
        test_row <- syn_data[idx, ]
        raw_pred <- stats::predict(m, newdata = test_row, type = "response")
        bias <- calculate_adaptive_bias(train_df, m, s)
        
        preds_list[[as.character(current_date)]] <- dplyr::tibble(
          Date = current_date,
          sindrome = s,
          Observed = test_row$casi,
          Raw_Pred = as.numeric(raw_pred),
          Adaptive_Pred = as.numeric(raw_pred * bias),
          Theta = theta
        )
      }
      dplyr::bind_rows(preds_list)
    }, .options = furrr::furrr_options(seed = TRUE))
    
    results_list[[a]] <- area_val
  }
  return(results_list)
}


#' Get Farrington Control Parameters
#'
#' Returns the standard control list for Farrington Flexible algorithm.
#'
#' @param range Validation indices
#' @param alpha Significance level (0.05 for 95%, 0.01 for 99%)
#' @return A control list for farringtonFlexible
#' @export
get_farrington_control <- function(range, alpha = 0.01) {
  list(
    range = range,
    noPeriods = 10,
    b = 2,
    w = 21,
    reweight = TRUE,
    weightsThreshold = 2.58,
    glmWarnings = FALSE,
    alpha = alpha,
    trend = TRUE,
    pastWeeksNotIncluded = 182,
    pThresholdTrend = 1,
    limit54 = c(5, 4),
    powertrans = "2/3",
    fitFun = "algo.farrington.fitGLM.flexible",
    populationOffset = FALSE,
    thresholdMethod = "nbPlugin"
  )
}

#' Run Farrington Flexible Validation - PARALLEL (FIXED)
#'
#' @param clean_data The dataframe from load_and_process_data
#' @param validation_days Number of days for validation period
#' @param workers Number of parallel workers (default: 14)
#' @return A list of dataframes, named by area
#' @export
run_farrington_validation <- function(clean_data, validation_days = 365, workers = 14) {
  library(surveillance)
  library(furrr)
  
  future::plan(future::multisession, workers = workers)
  on.exit(future::plan(future::sequential), add = TRUE)
  
  areas <- unique(clean_data$area)
  results_list <- list()
  
  for (a in areas) {
    cat("Farrington - Processing area:", a, "\n")
    
    area_data <- clean_data |> dplyr::filter(area == a)
    syndromes <- unique(area_data$sindrome)
    
    dates <- area_data |> 
      dplyr::distinct(Data_inizio_sett) |> 
      dplyr::arrange(Data_inizio_sett) |> 
      dplyr::pull(Data_inizio_sett)
    
    validation_start <- max(dates) - lubridate::days(validation_days)
    val_indices <- get_validation_indices(dates, validation_start)
    
    if (length(val_indices) == 0) {
      results_list[[a]] <- dplyr::tibble()
      next
    }
    
    # PARALLEL: Each worker creates its own sts object
    area_val <- furrr::future_map_dfr(syndromes, function(s) {
      cat("  -", s, "\n")
      
      # Filter data for this syndrome
      syn_data <- area_data |> 
        dplyr::filter(sindrome == s, !is.na(Data_inizio_sett), !is.na(casi)) |>
        dplyr::arrange(Data_inizio_sett)
      
      if (nrow(syn_data) == 0) return(NULL)
      
      # Create sts INSIDE the worker
      sts_uni <- tryCatch(
        create_univariate_sts(syn_data),
        error = function(e) NULL
      )
      
      if (is.null(sts_uni)) return(NULL)
      
      # Recalculate val_indices for this syndrome's data
      syn_dates <- syn_data$Data_inizio_sett
      syn_val_indices <- which(syn_dates > validation_start)
      
      if (length(syn_val_indices) == 0) return(NULL)
      
      # Run at both thresholds
      control_95 <- get_farrington_control(syn_val_indices, alpha = 0.05)
      control_99 <- get_farrington_control(syn_val_indices, alpha = 0.01)
      
      res_95 <- tryCatch(
        surveillance::farringtonFlexible(sts_uni, control = control_95),
        error = function(e) NULL
      )
      
      res_99 <- tryCatch(
        surveillance::farringtonFlexible(sts_uni, control = control_99),
        error = function(e) NULL
      )
      
      if (is.null(res_95) || is.null(res_99)) return(NULL)
      
      # Extract results
      ub_95 <- as.numeric(surveillance::upperbound(res_95))
      ub_99 <- as.numeric(surveillance::upperbound(res_99))
      observed <- syn_data$casi[syn_val_indices]
      
      # Get expected if available
      exp_raw <- res_99@control$expected
      exp_vec <- if (!is.null(exp_raw) && length(exp_raw) == length(syn_val_indices)) {
        as.numeric(exp_raw)
      } else {
        rep(NA_real_, length(syn_val_indices))
      }
      
      dplyr::tibble(
        Date = syn_dates[syn_val_indices],
        sindrome = s,
        Observed = observed,
        Expected = exp_vec,
        Limit_95 = ub_95,
        Limit_99 = ub_99,
        Status = dplyr::case_when(
          is.na(Limit_99) ~ "GREEN",
          Observed > Limit_99 ~ "RED",
          Observed > Limit_95 ~ "YELLOW",
          TRUE ~ "GREEN"
        )
      )
    }, .options = furrr::furrr_options(seed = TRUE))
    
    results_list[[a]] <- area_val
  }
  return(results_list)
}


#' Run GLR for Negative Binomial Validation - PARALLEL (FIXED)
#'
#' @param clean_data The dataframe from load_and_process_data
#' @param validation_days Number of days for validation period
#' @param workers Number of parallel workers (default: 14)
#' @return A list of dataframes, named by area
#' @export
run_glrnb_validation <- function(clean_data, validation_days = 365, workers = 14) {
  library(surveillance)
  library(furrr)
  
  future::plan(future::multisession, workers = workers)
  on.exit(future::plan(future::sequential), add = TRUE)
  
  areas <- unique(clean_data$area)
  results_list <- list()
  
  for (a in areas) {
    cat("GLRNB - Processing area:", a, "\n")
    
    area_data <- clean_data |> dplyr::filter(area == a)
    syndromes <- unique(area_data$sindrome)
    
    validation_start <- max(area_data$Data_inizio_sett) - lubridate::days(validation_days)
    
    # PARALLEL: Each worker creates its own sts object
    area_val <- furrr::future_map_dfr(syndromes, function(s) {
      cat("  -", s, "\n")
      
      syn_data <- area_data |> 
        dplyr::filter(sindrome == s, !is.na(Data_inizio_sett), !is.na(casi)) |>
        dplyr::arrange(Data_inizio_sett)
      
      if (nrow(syn_data) < 50) return(NULL)
      
      syn_dates <- syn_data$Data_inizio_sett
      syn_val_indices <- which(syn_dates > validation_start)
      train_indices <- which(syn_dates <= validation_start)
      
      if (length(syn_val_indices) == 0 || length(train_indices) < 30) return(NULL)
      
      # Create sts INSIDE the worker
      sts_uni <- tryCatch(
        create_univariate_sts(syn_data),
        error = function(e) NULL
      )
      
      if (is.null(sts_uni)) return(NULL)
      
      # Estimate in-control parameters
      train_obs <- syn_data$casi[train_indices]
      mu0 <- mean(train_obs, na.rm = TRUE)
      var_train <- var(train_obs, na.rm = TRUE)
      alpha0 <- if (var_train <= mu0) 100 else mu0^2 / (var_train - mu0)
      
      control <- list(
        range = syn_val_indices,
        mu0 = rep(mu0, length(syn_val_indices)),
        alpha = alpha0,
        c.ARL = 5,
        ret = "cases",
        theta = log(1.5)
      )
      
      res <- tryCatch(
        surveillance::glrnb(sts_uni, control = control),
        error = function(e) NULL
      )
      
      if (is.null(res)) return(NULL)
      
      alarms <- as.numeric(surveillance::alarms(res))
      upperbound <- as.numeric(surveillance::upperbound(res))
      observed <- syn_data$casi[syn_val_indices]
      
      dplyr::tibble(
        Date = syn_dates[syn_val_indices],
        sindrome = s,
        Observed = observed,
        Expected = mu0,
        Upperbound = upperbound,
        Alarm = alarms,
        Status = dplyr::if_else(alarms == 1, "ALERT", "GREEN")
      )
    }, .options = furrr::furrr_options(seed = TRUE))
    
    results_list[[a]] <- area_val
  }
  return(results_list)
}




#' Run Poisson Baseline Model
#'
#' Simple baseline using historical Poisson mean with fixed quantile thresholds.
#'
#' @param clean_data The dataframe from load_and_process_data
#' @param validation_days Number of days for validation period
#' @return A list of dataframes, named by area
#' @export
run_poisson_baseline <- function(clean_data, validation_days = 365) {
  
  areas <- unique(clean_data$area)
  results_list <- list()
  
  for (a in areas) {
    area_data <- clean_data |> dplyr::filter(area == a)
    syndromes <- unique(area_data$sindrome)
    validation_start <- max(area_data$Data_inizio_sett) - lubridate::days(validation_days)
    
    area_val <- purrr::map_dfr(syndromes, function(s) {
      syn_data <- area_data |> 
        dplyr::filter(sindrome == s) |> 
        dplyr::arrange(Data_inizio_sett)
      
      train_data <- syn_data |> dplyr::filter(Data_inizio_sett <= validation_start)
      val_data <- syn_data |> dplyr::filter(Data_inizio_sett > validation_start)
      
      if (nrow(train_data) < 30 || nrow(val_data) == 0) return(NULL)
      
      lambda <- mean(train_data$casi, na.rm = TRUE)
      
      dplyr::tibble(
        Date = val_data$Data_inizio_sett,
        sindrome = s,
        Observed = val_data$casi,
        Expected = lambda,
        Limit_95 = stats::qpois(0.95, lambda),
        Limit_99 = stats::qpois(0.99, lambda),
        Residual = (Observed - Expected) / sqrt(Expected),
        Status = dplyr::case_when(
          Observed > stats::qpois(0.99, lambda) ~ "RED",
          Observed > stats::qpois(0.95, lambda) ~ "YELLOW",
          TRUE ~ "GREEN"
        )
      )
    })
    
    results_list[[a]] <- area_val
  }
  return(results_list)
}


# Validation and Testing --------------------------------------------------

#' Run Sensitivity Analysis - PARALLEL (FIXED)
#'
#' @param validation_results_list Output from run_rolling_validation (GAM)
#' @param outbreak_durations Vector of outbreak durations to test (in days)
#' @param n_trials Number of Monte Carlo trials per magnitude level
#' @param workers Number of parallel workers (default: 14)
#' @return A list of dataframes with POD curves, named by area
#' @export
run_sensitivity_analysis <- function(validation_results_list, 
                                     outbreak_durations = c(4, 14, 49),
                                     n_trials = 50,
                                     workers = 14) {
  library(furrr)
  
  future::plan(future::multisession, workers = workers)
  on.exit(future::plan(future::sequential), add = TRUE)
  
  areas <- names(validation_results_list)
  final_results <- list()
  
  for (a in areas) {
    cat("Sensitivity GAM - Processing area:", a, "\n")
    
    val_df <- validation_results_list[[a]]
    if (is.null(val_df) || nrow(val_df) == 0) next
    
    syndromes <- unique(val_df$sindrome)
    
    # Create combinations
    combinations <- expand.grid(
      sindrome = syndromes, 
      duration = outbreak_durations, 
      stringsAsFactors = FALSE
    )
    
    # PARALLEL
    area_pod <- furrr::future_pmap_dfr(combinations, function(sindrome, duration) {
      s <- sindrome
      
      s_data <- val_df |> 
        dplyr::filter(sindrome == s) |>
        dplyr::mutate(
          Baseline_99 = stats::qnbinom(0.99, mu = Adaptive_Pred, size = Theta),
          Is_Green = Observed <= Baseline_99
        ) |>
        dplyr::arrange(Date)
      
      if (nrow(s_data) < duration + 20) return(NULL)
      
      valid_dates <- unique(s_data$Date)
      valid_dates <- valid_dates[valid_dates < (max(valid_dates) - (duration - 1))]
      
      # Find clean windows
      clean_start_dates_idx <- purrr::map_lgl(valid_dates, function(d) {
        window_slice <- s_data |> dplyr::filter(Date >= d, Date <= d + (duration - 1))
        nrow(window_slice) == duration && all(window_slice$Is_Green)
      })
      
      green_pool <- valid_dates[clean_start_dates_idx]
      sampling_pool <- if (length(green_pool) < 20) valid_dates else green_pool
      
      if (length(sampling_pool) == 0) return(NULL)
      
      magnitudes <- 0:13
      
      purrr::map_dfr(magnitudes, function(m) {
        total_fake <- 2^m
        detected <- 0
        
        for (t in 1:n_trials) {
          start_date <- sample(sampling_pool, 1)
          window <- seq(start_date, start_date + (duration - 1), by = "day")
          
          fake_cases <- generate_outbreak_shape(total_fake, duration, cumulative = TRUE)
          
          slice <- s_data |> dplyr::filter(Date %in% window) |> dplyr::arrange(Date)
          if (nrow(slice) < duration) next
          
          injected_obs <- slice$Observed + fake_cases
          new_bias <- mean(injected_obs) / mean(slice$Raw_Pred)
          if (is.na(new_bias) || is.infinite(new_bias)) new_bias <- 1
          
          new_thresh_99 <- stats::qnbinom(0.99, mu = slice$Raw_Pred * new_bias, size = slice$Theta)
          
          if (sum(injected_obs > new_thresh_99) > 0) detected <- detected + 1
        }
        
        dplyr::tibble(
          Total_Cases = total_fake,
          POD = detected / n_trials,
          sindrome = s,
          Duration_Days = duration
        )
      })
    }, .options = furrr::furrr_options(seed = TRUE))
    
    final_results[[a]] <- area_pod
  }
  return(final_results)
}


#' Run Sensitivity Analysis for Farrington - SINGLE AREA, PARALLEL
#'
#' Processes one area at a time with parallel workers.
#'
#' @param clean_data The dataframe from load_and_process_data
#' @param area_name Which area to process ("Lombardia", "Milano", "Valtellina")
#' @param outbreak_durations Vector of outbreak durations to test
#' @param n_trials Number of Monte Carlo trials per magnitude level
#' @param workers Number of parallel workers (default: 14)
#' @return A dataframe with POD curves for the specified area
#' @export
run_farrington_sensitivity_area <- function(clean_data, 
                                            area_name,
                                            outbreak_durations = c(4, 14, 49),
                                            n_trials = 20,
                                            workers = 14) {
  library(surveillance)
  library(furrr)
  
  # Set up parallel backend
  future::plan(future::multisession, workers = workers)
  on.exit(future::plan(future::sequential), add = TRUE)
  
  cat("\n========================================\n")
  cat("Sensitivity Farrington - Area:", area_name, "(parallel)\n")
  cat("========================================\n")
  
  area_data <- clean_data |> dplyr::filter(area == area_name)
  
  if (nrow(area_data) == 0) {
    warning("No data found for area: ", area_name)
    return(dplyr::tibble())
  }
  
  syndromes <- unique(area_data$sindrome)
  validation_start <- max(area_data$Data_inizio_sett, na.rm = TRUE) - lubridate::days(365)
  
  # Create combinations
  combinations <- expand.grid(
    sindrome = syndromes, 
    duration = outbreak_durations,
    stringsAsFactors = FALSE
  )
  
  n_total <- nrow(combinations)
  cat("Total combinations:", n_total, "\n")
  cat("Workers:", workers, "\n\n")
  
  # PARALLEL processing
  area_pod <- furrr::future_pmap_dfr(combinations, function(sindrome, duration) {
    
    s <- sindrome
    
    syn_data <- area_data |> 
      dplyr::filter(sindrome == s) |> 
      dplyr::arrange(Data_inizio_sett)
    
    if (nrow(syn_data) < 100) return(NULL)
    
    val_indices <- which(syn_data$Data_inizio_sett > validation_start)
    if (length(val_indices) < duration) return(NULL)
    
    # Create sts
    sts_obj <- tryCatch(
      create_univariate_sts(syn_data),
      error = function(e) NULL
    )
    
    if (is.null(sts_obj)) return(NULL)
    
    control_99 <- get_farrington_control(val_indices, alpha = 0.01)
    
    res_base <- tryCatch(
      surveillance::farringtonFlexible(sts_obj, control = control_99),
      error = function(e) NULL
    )
    
    if (is.null(res_base)) return(NULL)
    
    ub_99 <- as.numeric(surveillance::upperbound(res_base))
    
    # Handle potential NA in upperbound
    if (all(is.na(ub_99))) return(NULL)
    
    # Safe comparison handling NA
    is_green <- !is.na(ub_99) & !is.na(syn_data$casi[val_indices]) & 
      syn_data$casi[val_indices] <= ub_99
    
    valid_dates <- syn_data$Data_inizio_sett[val_indices]
    valid_dates <- valid_dates[valid_dates < (max(valid_dates, na.rm = TRUE) - (duration - 1))]
    
    if (length(valid_dates) == 0) return(NULL)
    
    # Find clean windows with NA handling
    clean_idx <- purrr::map_lgl(seq_along(valid_dates), function(idx) {
      if (idx + duration - 1 > length(is_green)) return(FALSE)
      window <- is_green[idx:(idx + duration - 1)]
      if (any(is.na(window))) return(FALSE)
      all(window)
    })
    
    # Ensure no NA in clean_idx
    clean_idx[is.na(clean_idx)] <- FALSE
    
    # Build sampling pool
    n_clean <- sum(clean_idx)
    if (n_clean >= 10) {
      sampling_pool <- valid_dates[clean_idx]
    } else {
      sampling_pool <- valid_dates
    }
    
    if (length(sampling_pool) == 0) return(NULL)
    
    magnitudes <- c(0, 2, 4, 6, 8, 10, 12)
    
    purrr::map_dfr(magnitudes, function(m) {
      total_fake <- 2^m
      detected <- 0
      valid_trials <- 0
      
      for (t in 1:n_trials) {
        start_date <- sample(sampling_pool, 1)
        start_idx <- which(syn_data$Data_inizio_sett == start_date)
        
        if (length(start_idx) == 0 || start_idx + duration - 1 > nrow(syn_data)) next
        
        end_idx <- start_idx + duration - 1
        
        fake_cases <- generate_outbreak_shape(total_fake, duration, cumulative = TRUE)
        modified_cases <- syn_data$casi
        modified_cases[start_idx:end_idx] <- modified_cases[start_idx:end_idx] + fake_cases
        
        # Create modified sts
        mod_syn_data <- syn_data
        mod_syn_data$casi <- modified_cases
        
        sts_mod <- tryCatch(
          create_univariate_sts(mod_syn_data),
          error = function(e) NULL
        )
        
        if (is.null(sts_mod)) next
        
        outbreak_range <- start_idx:end_idx
        control_mod <- get_farrington_control(outbreak_range, alpha = 0.01)
        
        res_mod <- tryCatch(
          surveillance::farringtonFlexible(sts_mod, control = control_mod),
          error = function(e) NULL
        )
        
        if (is.null(res_mod)) next
        
        valid_trials <- valid_trials + 1
        alarms_mod <- as.numeric(surveillance::alarms(res_mod))
        if (sum(alarms_mod, na.rm = TRUE) > 0) detected <- detected + 1
      }
      
      pod_value <- if (valid_trials > 0) detected / valid_trials else NA_real_
      
      dplyr::tibble(
        Total_Cases = total_fake,
        POD = pod_value,
        Valid_Trials = valid_trials,
        sindrome = s,
        Duration_Days = duration
      )
    })
  }, .options = furrr::furrr_options(seed = TRUE))
  
  cat("Area", area_name, "complete!\n")
  
  # Add area column
  if (nrow(area_pod) > 0) {
    area_pod$area <- area_name
  }
  
  return(area_pod)
}


#' Combine Farrington Sensitivity Results
#'
#' Helper to combine results from separate area runs into the expected list format.
#'
#' @param lombardia_results Output from run_farrington_sensitivity_area for Lombardia
#' @param milano_results Output from run_farrington_sensitivity_area for Milano
#' @param valtellina_results Output from run_farrington_sensitivity_area for Valtellina
#' @return A list of dataframes, named by area (matching original format)
#' @export
combine_farrington_sensitivity <- function(lombardia_results, 
                                           milano_results, 
                                           valtellina_results) {
  list(
    Lombardia = lombardia_results,
    Milano = milano_results,
    Valtellina = valtellina_results
  )
}
#' Calculate Residual Diagnostics for Any Model
#'
#' Computes overdispersion index and autocorrelation of residuals.
#'
#' @param results_df Dataframe with Observed and Expected columns
#' @param residual_col Name of the residual column (auto-calculated if NULL)
#' @return A list with overdispersion stats and ACF data
#' @export
calculate_residual_diagnostics <- function(results_df, residual_col = NULL) {
  
  if (is.null(residual_col) || !residual_col %in% names(results_df)) {
    results_df <- results_df |>
      dplyr::mutate(Residual = Observed - Expected)
    residual_col <- "Residual"
  }
  
  overdisp <- results_df |>
    dplyr::group_by(sindrome) |>
    dplyr::summarise(
      n_obs = dplyr::n(),
      mean_residual = mean(.data[[residual_col]], na.rm = TRUE),
      var_residual = stats::var(.data[[residual_col]], na.rm = TRUE),
      overdisp_index = var_residual / (abs(mean_residual) + 0.001),
      prop_positive = mean(.data[[residual_col]] > 0, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(overdisp_index))
  
  acf_data <- results_df |>
    dplyr::group_by(sindrome) |>
    dplyr::arrange(Date) |>
    dplyr::summarise(
      acf_vals = list({
        resid <- .data[[residual_col]][!is.na(.data[[residual_col]])]
        if (length(resid) > 15) {
          stats::acf(resid, lag.max = 10, plot = FALSE)$acf[-1]
        } else {
          rep(NA_real_, 10)
        }
      }),
      lags = list(1:10),
      .groups = "drop"
    ) |>
    tidyr::unnest(c(acf_vals, lags))
  
  list(overdispersion = overdisp, acf = acf_data)
}


#' Calculate Performance Metrics for Model Comparison
#'
#' Computes standardized metrics across all models.
#'
#' @param results_df Dataframe with Observed, Expected, and threshold columns
#' @param model_name Character string identifying the model
#' @return A tibble with performance metrics by syndrome
#' @export
calculate_model_metrics <- function(results_df, model_name) {
  
  # Normalize column names
  if ("Adaptive_Pred" %in% names(results_df) && !"Expected" %in% names(results_df)) {
    results_df <- results_df |> dplyr::mutate(Expected = Adaptive_Pred)
  }
  
  has_95 <- "Limit_95" %in% names(results_df)
  has_99 <- "Limit_99" %in% names(results_df)
  
  results_df |>
    dplyr::group_by(sindrome) |>
    dplyr::summarise(
      Model = model_name,
      N_Obs = dplyr::n(),
      RMSE = sqrt(mean((Observed - Expected)^2, na.rm = TRUE)),
      MAE = mean(abs(Observed - Expected), na.rm = TRUE),
      MAPE = mean(abs(Observed - Expected) / (Observed + 1), na.rm = TRUE) * 100,
      Mean_Bias = mean(Observed - Expected, na.rm = TRUE),
      R2 = 1 - sum((Observed - Expected)^2, na.rm = TRUE) / 
        sum((Observed - mean(Observed, na.rm = TRUE))^2, na.rm = TRUE),
      Coverage_95 = if (has_95) mean(Observed <= Limit_95, na.rm = TRUE) * 100 else NA_real_,
      Coverage_99 = if (has_99) mean(Observed <= Limit_99, na.rm = TRUE) * 100 else NA_real_,
      Alert_Rate_95 = if (has_95) mean(Observed > Limit_95, na.rm = TRUE) * 100 else NA_real_,
      Alert_Rate_99 = if (has_99) mean(Observed > Limit_99, na.rm = TRUE) * 100 else NA_real_,
      .groups = "drop"
    )
}


#' Standardize GAM Results for Comparison
#'
#' Formats GAM validation output to match the comparison framework.
#'
#' @param val_results Output from run_rolling_validation
#' @param area Character string for area name
#' @return A standardized tibble
#' @export
standardize_gam_results <- function(val_results, area) {
  val_df <- val_results[[area]]
  
  if (is.null(val_df) || nrow(val_df) == 0) return(dplyr::tibble())
  
  val_df |>
    dplyr::mutate(
      Expected = Adaptive_Pred,
      Limit_95 = stats::qnbinom(0.95, mu = Adaptive_Pred, size = Theta),
      Limit_99 = stats::qnbinom(0.99, mu = Adaptive_Pred, size = Theta),
      Residual = Observed - Expected,
      Std_Residual = Residual / sqrt(Expected + Expected^2 / Theta),
      Status = dplyr::case_when(
        Observed > Limit_99 ~ "RED",
        Observed > Limit_95 ~ "YELLOW",
        TRUE ~ "GREEN"
      )
    )
}


#' Standardize Farrington Results for Comparison
#'
#' Formats Farrington output to match the comparison framework.
#'
#' @param farrington_results Output from run_farrington_validation
#' @param area Character string for area name
#' @return A standardized tibble
#' @export
standardize_farrington_results <- function(farrington_results, area) {
  farr_df <- farrington_results[[area]]
  
  if (is.null(farr_df) || nrow(farr_df) == 0) return(dplyr::tibble())
  
  farr_df |>
    dplyr::mutate(
      Residual = Observed - Expected,
      Std_Residual = dplyr::if_else(
        !is.na(Expected) & Expected > 0,
        Residual / sqrt(Expected),
        NA_real_
      )
    )
}