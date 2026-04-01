library(targets)
library(tarchetypes)
source("R/functions.R")

out_dir <- file.path(getwd(), "output")
if (!dir.exists(out_dir)) {
  
  dir.create(out_dir)
}

report_date <- format(Sys.Date(), "%Y_%m_%d")

list(
  # Preprocessing  ----------------------------------------------------------
  
  tar_target(raw_file, "Estrazione_accessi_PS.xlsx", format = "file"),
  tar_target(clean_data, load_and_process_data(raw_file)),
  
  tar_target(all_data_list, prepare_all_data_list(clean_data)),
  tar_target(all_stats_list, calculate_all_stats(all_data_list)),
  tar_target(all_baselines_list, calculate_all_baselines(all_data_list)),
  
  # Analysis  ---------------------------------------------------------------
  
  tar_target(ops_results, run_operational_model(clean_data)),
  tar_target(val_results, run_rolling_validation(clean_data)),
  
  tar_target(farrington_results, run_farrington_validation(clean_data)),
  tar_target(glrnb_results, run_glrnb_validation(clean_data)),
  tar_target(poisson_results, run_poisson_baseline(clean_data)),
  
  tar_target(
    sens_results_gam, 
    run_sensitivity_analysis(val_results, outbreak_durations = c(4, 14, 49), n_trials = 50)
  ),
  tar_target(
    sens_results_farrington,
    run_farrington_sensitivity(clean_data, outbreak_durations = c(4, 14, 49), n_trials = 50)
  ),

  # Report ------------------------------------------------------------------
  
  tar_render(
    report_daily_Lombardia, 
    "report/report.qmd", 
    params = list(area = "Lombardia"), 
    output_file = file.path(out_dir, paste0(report_date, "_EWS_Report_Lombardia.html"))
  ),
  tar_render(
    report_daily_Milano, 
    "report/report.qmd", 
    params = list(area = "Milano"), 
    output_file = file.path(out_dir, paste0(report_date, "_EWS_Report_Milano.html"))
  ),
  tar_render(
    report_daily_Valtellina, 
    "report/report.qmd", 
    params = list(area = "Valtellina"), 
    output_file = file.path(out_dir, paste0(report_date, "_EWS_Report_Valtellina.html"))
  ),
  
  tar_render(
    report_val_Lombardia, 
    "report/validation_report.qmd", 
    params = list(area = "Lombardia"), 
    output_file = file.path(out_dir, paste0(report_date, "_Validation_Report_Lombardia.html"))
  ),
  tar_render(
    report_val_Milano, 
    "report/validation_report.qmd", 
    params = list(area = "Milano"), 
    output_file = file.path(out_dir, paste0(report_date, "_Validation_Report_Milano.html"))
  ),
  tar_render(
    report_val_Valtellina, 
    "report/validation_report.qmd", 
    params = list(area = "Valtellina"), 
    output_file = file.path(out_dir, paste0(report_date, "_Validation_Report_Valtellina.html"))
  ),
  
  tar_render(
    report_comparison_Lombardia,
    "report/model_comparison.qmd",
    params = list(area = "Lombardia"),
    output_file = file.path(out_dir, paste0(report_date, "_Model_Comparison_Lombardia.html"))
  ),
  tar_render(
    report_comparison_Milano,
    "report/model_comparison.qmd",
    params = list(area = "Milano"),
    output_file = file.path(out_dir, paste0(report_date, "_Model_Comparison_Milano.html"))
  ),
  tar_render(
    report_comparison_Valtellina,
    "report/model_comparison.qmd",
    params = list(area = "Valtellina"),
    output_file = file.path(out_dir, paste0(report_date, "_Model_Comparison_Valtellina.html"))
  )
)