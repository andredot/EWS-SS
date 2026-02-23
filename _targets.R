library(targets)
library(tarchetypes)

source("R/functions.R")

list(
  # --- 1. GLOBAL DATA ---
  tar_target(raw_file, "Estrazione_accessi_PS.xlsx", format = "file"),
  tar_target(clean_data, load_and_process_data(raw_file)),
  
  # --- 2. GAM MODELS (Cached) ---
  tar_target(ops_results, run_operational_model(clean_data)),
  tar_target(val_results, run_rolling_validation(clean_data)),
  tar_target(sens_results, run_sensitivity_analysis(val_results)),
  
  tar_target(all_data_list, prepare_all_data_list(clean_data)),
  tar_target(all_stats_list, calculate_all_stats(all_data_list)),
  tar_target(all_baselines_list, calculate_all_baselines(all_data_list)),

  # --- 3. Farrington MODELS (Cached) ---
  tar_target(farrington_results, run_farrington_validation(clean_data)),
    
  # ============================================================================
  #  REPORTS
  # ============================================================================
  
  # --- LOMBARDIA ---
  tar_render(
    report_daily_Lombardia, 
    "report.qmd", 
    params = list(area = "Lombardia"), 
    output_file = "output/EWS_Report_Lombardia.html"
  ),
  tar_render(
    report_val_Lombardia, "validation_report.qmd", 
    params = list(area = "Lombardia"), output_file = "output/Validation_Report_Lombardia.html"
  ),
  tar_render(
    report_farrington_Lombardia, "farrington_validation.qmd", 
    params = list(area = "Lombardia"), output_file = "output/Farrington_Validation_Lombardia.html"
  ),
  
  # --- MILANO ---
  tar_render(
    report_daily_Milano, 
    "report.qmd", 
    params = list(area = "Milano"), 
    output_file = "output/EWS_Report_Milano.html"
  ),
  tar_render(
    report_val_Milano, "validation_report.qmd", 
    params = list(area = "Milano"), output_file = "output/Validation_Report_Milano.html"
  ),
  tar_render(
    report_farrington_Milano, "farrington_validation.qmd", 
    params = list(area = "Milano"), output_file = "output/Farrington_Validation_Milano.html"
  ),
  
  # --- VALTELLINA ---
  tar_render(
    report_daily_Valtellina, 
    "report.qmd", 
    params = list(area = "Valtellina"), 
    output_file = "output/EWS_Report_Valtellina.html"
  ),
  tar_render(
    report_val_Valtellina, "validation_report.qmd", 
    params = list(area = "Valtellina"), output_file = "output/Validation_Report_Valtellina.html"
  ),
  tar_render(
    report_farrington_Valtellina, "farrington_validation.qmd", 
    params = list(area = "Valtellina"), output_file = "output/Farrington_Validation_Valtellina.html"
  )
)