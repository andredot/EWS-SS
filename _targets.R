library(targets)
library(tarchetypes)

source("R/functions.R")

list(
  # --- 1. GLOBAL DATA ---
  tar_target(raw_file, "Estrazione_accessi_PS.xlsx", format = "file"),
  tar_target(clean_data, load_and_process_data(raw_file)),
  
  # --- 2. GLOBAL MODELS (Cached) ---
  tar_target(ops_results, run_operational_model(clean_data)),
  tar_target(val_results, run_rolling_validation(clean_data)),
  tar_target(sens_results, run_sensitivity_analysis(val_results)),
  
  # 1. Create a list of dataframes: list(Lombardia = ..., Milano = ...)
  tar_target(all_data_list, prepare_all_data_list(clean_data)),
  
  # 2. Create a list of stats: list(Lombardia = list(overdisp=...), ...)
  tar_target(all_stats_list, calculate_all_stats(all_data_list)),
  
  # 3. Create a list of baselines
  tar_target(all_baselines_list, calculate_all_baselines(all_data_list)),
  
  # ============================================================================
  #  REPORTS
  # ============================================================================
  
  # --- LOMBARDIA ---
  tar_render(
    report_daily_Lombardia, 
    "report.qmd", 
    params = list(area = "Lombardia"), 
    output_file = "EWS_Report_Lombardia.html"
  ),
  tar_render(
    report_val_Lombardia, "validation_report.qmd", 
    params = list(area = "Lombardia"), output_file = "Validation_Report_Lombardia.html"
  ),
  
  # --- MILANO ---
  tar_render(
    report_daily_Milano, 
    "report.qmd", 
    params = list(area = "Milano"), 
    output_file = "EWS_Report_Milano.html"
  ),
  tar_render(
    report_val_Milano, "validation_report.qmd", 
    params = list(area = "Milano"), output_file = "Validation_Report_Milano.html"
  ),
  
  # --- VALTELLINA ---
  tar_render(
    report_daily_Valtellina, 
    "report.qmd", 
    params = list(area = "Valtellina"), 
    output_file = "EWS_Report_Valtellina.html"
  ),
  tar_render(
    report_val_Valtellina, "validation_report.qmd", 
    params = list(area = "Valtellina"), output_file = "Validation_Report_Valtellina.html"
  )
)