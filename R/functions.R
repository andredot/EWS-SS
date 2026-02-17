#' Load and Clean Emergency Department Data
#'
#' Imports the Excel file, selects columns based on known prefixes,
#' and reshapes into a long format suitable for GAM modeling.
#'
#' @param file_path String. Path to the .xlsx file.
#' @return A tibble with columns: Data_inizio_sett, area, sindrome, casi, week_num, etc.
#' @export
load_and_process_data <- function(file_path) {
  library(tidyverse)
  library(readxl)
  library(lubridate)
  
  df_raw <- read_excel(file_path)
  
  # Define area mappings
  prefix_map <- list(
    "Lombardia" = "N_accessi_RL_",
    "Milano"    = "N_accessi_ASST_Milanesi_",
    "Valtellina" = "N_accessi_ASST_Valtellina_"
  )
  
  all_areas <- list()
  
  for (area_name in names(prefix_map)) {
    prefix <- prefix_map[[area_name]]
    
    # Process specific area
    clean_area <- df_raw |>
      select(Data_inizio_sett, starts_with(prefix)) |>
      rename_with(~ str_remove(.x, prefix), starts_with(prefix)) |>
      pivot_longer(cols = -Data_inizio_sett, names_to = "sindrome", values_to = "casi") |>
      mutate(
        Data_inizio_sett = as.Date(Data_inizio_sett),
        area = area_name # Add area column for grouping later
      ) |>
      arrange(sindrome, Data_inizio_sett) |>
      group_by(sindrome) |>
      mutate(
        week_num = row_number(),
        week_of_year = isoweek(Data_inizio_sett),
        dow = wday(Data_inizio_sett, label=TRUE, week_start=1) |> as.factor() |> fct_relevel("Mon")
      ) |>
      ungroup()
    
    all_areas[[area_name]] <- clean_area
  }
  
  # Return one big dataframe (easier for targets to track)
  return(bind_rows(all_areas))
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
  
  for (a in areas) {
    area_data <- clean_data |> filter(area == a)
    syndromes <- unique(area_data$sindrome)
    
    # We focus on the last 60 days for the report plot
    report_window_start <- max(area_data$Data_inizio_sett) - days(60)
    
    area_preds <- map_dfr(syndromes, function(s) {
      syn_data <- area_data |> filter(sindrome == s)
      
      # Train on everything BEFORE the report window to simulate "live" prediction
      train_df <- syn_data |> filter(Data_inizio_sett < report_window_start)
      test_df  <- syn_data |> filter(Data_inizio_sett >= report_window_start)
      
      # 1. Fit Static GAM
      m <- gam(casi ~ s(week_num, bs = "ts", k = 20) + 
                 s(week_of_year, bs = "cc", k = 12) + dow,
               data = train_df, family = nb())
      
      theta <- m$family$getTheta(TRUE)
      
      # 2. Predict
      raw_pred <- predict(m, newdata = test_df, type = "response")
      
      # 3. Calculate Bias (Adaptive Shift) from the end of training data
      last_14 <- train_df |> tail(14)
      last_14_pred <- predict(m, newdata = last_14, type = "response")
      bias <- mean(last_14$casi) / mean(last_14_pred)
      if(is.na(bias) || is.infinite(bias)) bias <- 1
      
      # 4. Adaptive Forecast
      adaptive_pred <- raw_pred * bias
      
      tibble(
        Data_inizio_sett = test_df$Data_inizio_sett,
        sindrome = s,
        casi = test_df$casi, # Observed
        expected = as.numeric(adaptive_pred),
        limit_95 = qnbinom(0.95, mu = as.numeric(adaptive_pred), size = theta),
        limit_99 = qnbinom(0.99, mu = as.numeric(adaptive_pred), size = theta)
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
  
  for (a in areas) {
    area_data <- clean_data |> filter(area == a)
    syndromes <- unique(area_data$sindrome)
    validation_start <- max(area_data$Data_inizio_sett) - days(365)
    
    area_val <- map_dfr(syndromes, function(s) {
      syn_data <- area_data |> filter(sindrome == s)
      val_indices <- which(syn_data$Data_inizio_sett >= validation_start)
      
      preds_list <- list()
      
      # -- The Rolling Loop --
      for(idx in val_indices) {
        current_date <- syn_data$Data_inizio_sett[idx]
        train_df <- syn_data |> filter(Data_inizio_sett < current_date)
        
        # Optimization: Refit model only on 1st of month
        if(day(current_date) == 1 || idx == val_indices[1]) {
          m <- gam(casi ~ s(week_num, bs = "ts", k = 20) + 
                     s(week_of_year, bs = "cc", k = 12) + dow,
                   data = train_df, family = nb())
          theta <- m$family$getTheta(TRUE)
        }
        
        test_row <- syn_data[idx, ]
        raw_pred <- predict(m, newdata = test_row, type = "response")
        
        recent_hist <- train_df |> tail(14)
        recent_pred <- predict(m, newdata = recent_hist, type = "response")
        bias <- mean(recent_hist$casi) / mean(recent_pred)
        if(is.na(bias) || is.infinite(bias)) bias <- 1
        
        preds_list[[as.character(current_date)]] <- tibble(
          Date = current_date,
          sindrome = s,
          Observed = test_row$casi,
          Raw_Pred = as.numeric(raw_pred),
          Adaptive_Pred = as.numeric(raw_pred * bias),
          Theta = theta
        )
      }
      bind_rows(preds_list)
    })
    results_list[[a]] <- area_val
  }
  return(results_list)
}


#' Run Sensitivity Stress Test
#'
#' Injects synthetic outbreaks into the validation results to calculate POD.
#'
#' @param validation_results_list The output from run_rolling_validation.
#' @return A list of dataframes (POD curves), named by area.
#' @export
run_sensitivity_analysis <- function(validation_results_list) {
  
  # Helper: Sigmoid generator
  generate_outbreak_shape <- function(total_cases, duration = 14) {
    t <- seq(-6, 6, length.out = duration)
    curve <- 1 / (1 + exp(-t))
    scaled_curve <- (curve / sum(curve)) * total_cases
    return(round(scaled_curve))
  }
  
  areas <- names(validation_results_list)
  final_results <- list()
  
  for(a in areas) {
    val_df <- validation_results_list[[a]]
    syndromes <- unique(val_df$sindrome)
    
    area_pod <- map_dfr(syndromes, function(s) {
      s_data <- val_df |> filter(sindrome == s)
      valid_dates <- unique(s_data$Date)
      valid_dates <- valid_dates[valid_dates < (max(valid_dates) - 14)]
      
      magnitudes <- 0:13
      mag_res <- list()
      
      for(m in magnitudes) {
        total_fake <- 2^m
        detected <- 0
        n_trials <- 20 # Can increase to 50 for smoother curves
        
        for(t in 1:n_trials) {
          start_date <- sample(valid_dates, 1)
          window <- seq(start_date, start_date + 13, by="day")
          fake_cases <- generate_outbreak_shape(total_fake, 14)
          
          slice <- s_data |> filter(Date %in% window) |> arrange(Date)
          if(nrow(slice) < 14) next
          
          injected_obs <- slice$Observed + fake_cases
          new_bias <- mean(injected_obs) / mean(slice$Raw_Pred)
          new_thresh_95 <- qnbinom(0.95, mu = slice$Raw_Pred * new_bias, size = slice$Theta)
          
          if(sum(injected_obs > new_thresh_95) > 0) detected <- detected + 1
        }
        mag_res[[paste0(m)]] <- tibble(Total_Cases = total_fake, POD = detected/n_trials)
      }
      bind_rows(mag_res) |> mutate(sindrome = s)
    })
    final_results[[a]] <- area_pod
  }
  return(final_results)
}
