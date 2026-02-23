#' Load and Clean Emergency Department Data
#'
#' Imports the Excel file, selects columns based on known prefixes,
#' and reshapes into a long format suitable for GAM modeling.
#'
#' @param file_path String. Path to the .xlsx file.
#' @return A tibble with columns: Data_inizio_sett, area, sindrome, casi, week_num, etc.
#' @export
load_and_process_data <- function(file_path) {
  # Note: Libraries are loaded for the session, but functions below use explicit namespaces
  library(tidyverse)
  library(readxl)
  library(lubridate)
  
  df_raw <- readxl::read_excel(file_path)
  
  # Define area mappings
  prefix_map <- list(
    "Lombardia"  = "N_accessi_RL_",
    "Milano"     = "N_accessi_ASST_Milanesi_",
    "Valtellina" = "N_accessi_ASST_Valtellina_"
  )
  
  all_areas <- list()
  
  for (area_name in names(prefix_map)) {
    prefix <- prefix_map[[area_name]]
    
    # Process specific area
    clean_area <- df_raw |>
      dplyr::select(Data_inizio_sett, dplyr::starts_with(prefix)) |>
      dplyr::rename_with(~ stringr::str_remove(.x, prefix), dplyr::starts_with(prefix)) |>
      tidyr::pivot_longer(cols = -Data_inizio_sett, names_to = "sindrome", values_to = "casi") |>
      dplyr::mutate(
        Data_inizio_sett = as.Date(Data_inizio_sett),
        area = area_name # Add area column for grouping later
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
  
  # Return one big dataframe (easier for targets to track)
  return(dplyr::bind_rows(all_areas))
}

#' Fit GAM Model based on Syndrome Type
#'
#' Automatically selects the appropriate GAM flexibility based on 
#' whether the syndrome is highly seasonal or flat/random.
#'
#' @param train_df Dataframe containing the historical training data.
#' @param syndrome_name Character string of the current syndrome.
#' @param flat_syndromes Character vector of syndromes that require a stiff model.
#' @return A fitted mgcv::gam object.
#' @export
fit_syndrome_gam <- function(train_df, syndrome_name, 
                             flat_syndromes = c("alt_cosc", "conv", "dec_gravi", "int_farm", "rash_cut")) {
  if (syndrome_name %in% flat_syndromes) {
    # --- 1A. STIFF MODEL (Low Seasonality/Autocorrelation) ---
    mgcv::gam(casi ~ s(week_num, bs = "ts", k = 3) + dow,
              data = train_df, family = mgcv::nb())
  } else {
    # --- 1B. FLEXIBLE MODEL (High Seasonality/Autocorrelation) ---
    mgcv::gam(casi ~ s(week_num, bs = "ts", k = 20) + 
                s(week_of_year, bs = "cc", k = 12) + dow,
              data = train_df, family = mgcv::nb())
  }
}


#' Run Operational Model (Recent Horizon)
#'
#' Fits the GAM model on historical data and generates the "Adaptive Forecast"
#' for the most recent 30 days to populate the daily report.
#'
#' @param clean_data The dataframe from load_and_process_data.
#' @return A list of dataframes, named by area ("Lombardia", "Milano", etc.)
#' @export
run_operational_model <- function(clean_data) {
  library(mgcv)
  
  areas <- unique(clean_data$area)
  results_list <- list()
  
  # --- UPDATED FLAT SYNDROMES LIST ---
  flat_syndromes <- c("alt_cosc", "conv", "dec_gravi", "int_farm", "rash_cut")
  
  for (a in areas) {
    area_data <- clean_data |> dplyr::filter(area == a)
    syndromes <- unique(area_data$sindrome)
    
    report_window_start <- max(area_data$Data_inizio_sett) - lubridate::days(60)
    
    area_preds <- purrr::map_dfr(syndromes, function(s) {
      syn_data <- area_data |> dplyr::filter(sindrome == s)
      
      train_df <- syn_data |> dplyr::filter(Data_inizio_sett < report_window_start)
      test_df  <- syn_data |> dplyr::filter(Data_inizio_sett >= report_window_start)
      
      # 1. Train Model using the centralized helper function
      m <- fit_syndrome_gam(train_df, s, flat_syndromes)
      theta <- m$family$getTheta(TRUE)
      
      # 2. Predict baseline
      raw_pred <- stats::predict(m, newdata = test_df, type = "response")
      
      # 3. Apply Adaptive Bias (Frozen for flat syndromes)
      if (s %in% flat_syndromes) {
        bias <- 1 
      } else {
        last_14 <- train_df |> utils::tail(14)
        last_14_pred <- stats::predict(m, newdata = last_14, type = "response")
        bias <- mean(last_14$casi) / mean(last_14_pred)
        if(is.na(bias) || is.infinite(bias)) bias <- 1
      }
      
      # 4. Final Thresholds
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


#' Run Retrospective Rolling Validation
#'
#' Performs the 365-day sliding window validation for the validation report.
#' WARNING: Computationally expensive.
#'
#' @param clean_data The dataframe from load_and_process_data.
#' @return A list of dataframes (validation results), named by area.
#' @export
run_rolling_validation <- function(clean_data) {
  library(mgcv)
  
  areas <- unique(clean_data$area)
  results_list <- list()
  
  # --- UPDATED FLAT SYNDROMES LIST ---
  flat_syndromes <- c("alt_cosc", "conv", "dec_gravi", "int_farm", "rash_cut")
  
  for (a in areas) {
    area_data <- clean_data |> dplyr::filter(area == a)
    syndromes <- unique(area_data$sindrome)
    validation_start <- max(area_data$Data_inizio_sett) - lubridate::days(365)
    
    area_val <- purrr::map_dfr(syndromes, function(s) {
      syn_data <- area_data |> dplyr::filter(sindrome == s)
      val_indices <- which(syn_data$Data_inizio_sett >= validation_start)
      
      preds_list <- list()
      
      # -- The Rolling Loop --
      for(idx in val_indices) {
        current_date <- syn_data$Data_inizio_sett[idx]
        train_df <- syn_data |> dplyr::filter(Data_inizio_sett < current_date)
        
        # Optimization: Refit model only on 1st of month
        if(lubridate::day(current_date) == 1 || idx == val_indices[1]) {
          # Train Model using the centralized helper function
          m <- fit_syndrome_gam(train_df, s, flat_syndromes)
          theta <- m$family$getTheta(TRUE)
        }
        
        test_row <- syn_data[idx, ]
        raw_pred <- stats::predict(m, newdata = test_row, type = "response")
        
        # Apply Adaptive Bias (Frozen for flat syndromes)
        if (s %in% flat_syndromes) {
          bias <- 1 
        } else {
          recent_hist <- train_df |> utils::tail(14)
          recent_pred <- stats::predict(m, newdata = recent_hist, type = "response")
          bias <- mean(recent_hist$casi) / mean(recent_pred)
          if(is.na(bias) || is.infinite(bias)) bias <- 1
        }
        
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
    })
    results_list[[a]] <- area_val
  }
  return(results_list)
}

#' Run Sensitivity Stress Test
#'
#' Injects synthetic outbreaks into clean historical periods to calculate pure POD.
#'
#' @param validation_results_list The output from run_rolling_validation.
#' @return A list of dataframes (POD curves), named by area.
#' @export
run_sensitivity_analysis <- function(validation_results_list) {
  
  # Helper: Sigmoid generator with 7-day cumulative option
  generate_outbreak_shape <- function(total_cases, duration = 14, cumulative = TRUE) {
    t <- seq(-6, 6, length.out = duration)
    curve <- 1 / (1 + exp(-t))
    daily_incident <- (curve / sum(curve)) * total_cases
    
    if (cumulative) {
      rolling_curve <- numeric(duration)
      for (i in 1:duration) {
        start_idx <- max(1, i - 6)
        rolling_curve[i] <- sum(daily_incident[start_idx:i])
      }
      final_curve <- rolling_curve
    } else {
      final_curve <- daily_incident
    }
    
    return(round(final_curve))
  }
  
  areas <- names(validation_results_list)
  final_results <- list()
  
  for(a in areas) {
    val_df <- validation_results_list[[a]]
    syndromes <- unique(val_df$sindrome)
    
    area_pod <- purrr::map_dfr(syndromes, function(s) {
      
      # 1. Pre-calculate the historical baseline status to find "Green" days
      s_data <- val_df |> 
        dplyr::filter(sindrome == s) |>
        dplyr::mutate(
          Baseline_99 = stats::qnbinom(0.99, mu = Adaptive_Pred, size = Theta),
          Is_Green = Observed <= Baseline_99
        ) |>
        dplyr::arrange(Date)
      
      valid_dates <- unique(s_data$Date)
      valid_dates <- valid_dates[valid_dates < (max(valid_dates) - 13)]
      
      # 2. Identify ONLY the dates where the NEXT 14 DAYS are perfectly Green
      clean_start_dates_idx <- purrr::map_lgl(valid_dates, function(d) {
        window_slice <- s_data |> dplyr::filter(Date >= d, Date <= d + 13)
        nrow(window_slice) == 14 && all(window_slice$Is_Green)
      })
      
      green_pool <- valid_dates[clean_start_dates_idx]
      
      # 3. Safety Fallback: Ensure we have enough clean periods to sample from
      if(length(green_pool) < 20) {
        warning(sprintf("Syndrome '%s' in '%s' is highly volatile. Not enough clean 14-day windows. Falling back to random dates.", s, a))
        sampling_pool <- valid_dates
      } else {
        sampling_pool <- green_pool
      }
      
      magnitudes <- 0:13
      mag_res <- list()
      
      for(m in magnitudes) {
        total_fake <- 2^m
        detected <- 0
        n_trials <- 50 
        
        for(t in 1:n_trials) {
          # 4. Sample EXCLUSIVELY from the clean pool
          start_date <- sample(sampling_pool, 1)
          window <- seq(start_date, start_date + 13, by="day")
          
          fake_cases <- generate_outbreak_shape(total_fake, 14, cumulative = TRUE)
          
          slice <- s_data |> dplyr::filter(Date %in% window) |> dplyr::arrange(Date)
          if(nrow(slice) < 14) next
          
          injected_obs <- slice$Observed + fake_cases
          new_bias <- mean(injected_obs) / mean(slice$Raw_Pred)
          new_thresh_99 <- stats::qnbinom(0.99, mu = slice$Raw_Pred * new_bias, size = slice$Theta)
          
          if(sum(injected_obs > new_thresh_99) > 0) detected <- detected + 1
        }
        mag_res[[paste0(m)]] <- dplyr::tibble(Total_Cases = total_fake, POD = detected/n_trials)
      }
      dplyr::bind_rows(mag_res) |> dplyr::mutate(sindrome = s)
    })
    final_results[[a]] <- area_pod
  }
  return(final_results)
}
#' Calculate Exploratory Stats (Overdispersion & ACF)
#' Returns a list containing two dataframes to keep targets clean
#' @export
get_exploratory_stats <- function(df_long) {
  # 1. Overdispersion
  overdisp <- df_long |>
    dplyr::group_by(sindrome) |>
    dplyr::summarise(
      n_weeks = dplyr::n(),
      mean_delta = mean(delta_casi, na.rm=TRUE),
      var_delta = stats::var(delta_casi, na.rm=TRUE),
      overdisp_index = var_delta / abs(mean_delta),
      prop_zeros = mean(delta_casi == 0, na.rm=TRUE),
      .groups="drop"
    ) |>
    dplyr::arrange(dplyr::desc(overdisp_index))
  
  # 2. Autocorrelation (includes the fix for NAs)
  acf_data <- df_long |>
    dplyr::group_by(sindrome) |>
    dplyr::summarise(
      acf_vals = list(stats::acf(delta_casi[!is.na(delta_casi)], lag.max=10, plot=FALSE)$acf[-1]),
      lags = list(1:10),
      .groups = "drop"
    ) |>
    tidyr::unnest(c(acf_vals, lags))
  
  list(overdispersion = overdisp, acf = acf_data)
}

#' Calculate Historical Baselines
#' Computes the static benchmarks (Min/Max, Naive Poisson)
#' @export
get_baselines <- function(df_long) {
  df_long |>
    dplyr::group_by(sindrome) |>
    dplyr::summarise(
      roll_max_casi = max(utils::tail(casi, 52*2), na.rm = TRUE),
      roll_min_casi = min(utils::tail(casi, 52*2), na.rm = TRUE),
      poisson_mean  = mean(casi, na.rm = TRUE),
      p95_delta     = stats::quantile(utils::tail(delta_casi, 52), 0.95, na.rm = TRUE),
      p99_delta     = stats::quantile(utils::tail(delta_casi, 52), 0.99, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Prepare All Data (Grouped by Area)
#' Adds deltas and returns a LIST named by area
#' @export
prepare_all_data_list <- function(clean_data) {
  clean_data |>
    # Calculate Deltas for everyone
    dplyr::group_by(area, sindrome) |>
    dplyr::mutate(delta_casi = casi - dplyr::lag(casi)) |>
    dplyr::ungroup() |>
    # Split into a list: list("Lombardia" = df, "Milano" = df, ...)
    split(~area)
}

#' Calculate Stats for All Areas
#' Maps the existing stats function over the list of areas
#' @export
calculate_all_stats <- function(data_list) {
  # Applies get_exploratory_stats to every dataframe in the list
  purrr::map(data_list, get_exploratory_stats)
}

#' Calculate Baselines for All Areas
#' Maps the existing baseline function over the list of areas
#' @export
calculate_all_baselines <- function(data_list) {
  purrr::map(data_list, get_baselines)
}

#' Run Retrospective Validation using Farrington Flexible (Daily Data)
#'
#' Runs the Noufaily-adjusted Farrington algorithm twice to extract
#' both 95% (Yellow) and 99% (Red) thresholds.
#'
#' @param clean_data The dataframe from load_and_process_data
#' @return A list of dataframes (validation results), named by area.
#' @export
run_farrington_validation <- function(clean_data) {
  library(surveillance)
  
  areas <- unique(clean_data$area)
  results_list <- list()
  
  for (a in areas) {
    area_data <- clean_data |> dplyr::filter(area == a)
    syndromes <- unique(area_data$sindrome)
    
    validation_start <- max(area_data$Data_inizio_sett, na.rm = TRUE) - lubridate::days(365)
    
    area_val <- purrr::map_dfr(syndromes, function(s) {
      
      # 1. Prepare clean data
      syn_data <- area_data |> 
        dplyr::filter(sindrome == s, !is.na(Data_inizio_sett), !is.na(casi)) |> 
        dplyr::arrange(Data_inizio_sett)
      
      if (nrow(syn_data) == 0) return(NULL)
      val_indices <- which(syn_data$Data_inizio_sett >= validation_start)
      if (length(val_indices) == 0) return(NULL)
      
      # 2. Create sts object
      obs_matrix <- as.matrix(as.integer(syn_data$casi))
      dimnames(obs_matrix) <- NULL 
      
      sts_obj <- surveillance::sts(
        observed = obs_matrix,
        epoch = as.numeric(syn_data$Data_inizio_sett),
        epochAsDate = TRUE,
        frequency = 365
      )
      
      # 3. Define Base Control Parameters (Using Noufaily method)
      base_control <- list(
        range = val_indices,
        noPeriods = 1,
        b = 2,               
        w = 3,              
        weightsThreshold = 2.58,
        pastWeeksNotIncluded = 26, 
        pThresholdTrend = 1,
        thresholdMethod = "nbPlugin" 
      )
      
      # 4. Create separate controls for 95% and 99%
      control_95 <- base_control
      control_95$alpha <- 0.05  # 95% Confidence (YELLOW)
      
      control_99 <- base_control
      control_99$alpha <- 0.01  # 99% Confidence (RED)
      
      # 5. Run both models safely
      res_95 <- tryCatch({
        surveillance::farringtonFlexible(sts_obj, control = control_95)
      }, error = function(e) {
        warning("Farrington 95% failed for ", s, ": ", e$message)
        return(NULL)
      })
      
      res_99 <- tryCatch({
        surveillance::farringtonFlexible(sts_obj, control = control_99)
      }, error = function(e) {
        warning("Farrington 99% failed for ", s, ": ", e$message)
        return(NULL)
      })
      
      if (is.null(res_95) || is.null(res_99)) return(NULL)
      
      # 6. Extract thresholds safely
      # These vectors are ALREADY the exact length of val_indices
      ub_95 <- as.numeric(surveillance::upperbound(res_95))
      ub_99 <- as.numeric(surveillance::upperbound(res_99))
      
      # Expected is identical for both runs, extract from res_99
      exp_raw <- res_99@control$expected
      if (is.null(exp_raw) || length(exp_raw) != length(val_indices)) {
        exp_vec <- rep(NA_real_, length(val_indices))
      } else {
        exp_vec <- as.numeric(exp_raw)
      }
      
      # 7. Build combined dataframe
      dplyr::tibble(
        Date = syn_data$Data_inizio_sett[val_indices],
        sindrome = s,
        # syn_data is the full dataset, so we MUST use [val_indices] here
        Observed = syn_data$casi[val_indices], 
        Expected = exp_vec,
        # ub_95 and ub_99 are already subsetted by the surveillance package, no index needed!
        Limit_95 = ub_95,
        Limit_99 = ub_99,
        Status = dplyr::case_when(
          is.na(Limit_99) ~ "GREEN",
          Observed > Limit_99 ~ "RED (99%)",
          Observed > Limit_95 ~ "YELLOW (95%)",
          TRUE ~ "GREEN"
        )
      )
    })
    
    results_list[[a]] <- area_val
  }
  return(results_list)
}