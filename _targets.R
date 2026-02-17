library(targets)
library(tarchetypes)
source("R/functions.R")

list(
  tar_target(raw_file, "Estrazione_accessi_PS.xlsx", format = "file"),
  tar_target(clean_data, load_and_process_data(raw_file)),
  
  # Branch 1: The Daily Operational Report Data
  tar_target(ops_results, run_operational_model(clean_data)),
  
  # Branch 2: The Heavy Validation Data
  tar_target(val_results, run_rolling_validation(clean_data)),
  tar_target(sens_results, run_sensitivity_analysis(val_results)),
  
  tar_render(
    report_daily,
    "report.qmd",
    params = list(area = areas),
    output_file = paste0("EWS_Report_", areas, ".html"),
    pattern = map(areas)
  ),
  
  tar_render(
    report_validation,
    "validation_report.qmd",
    params = list(area = areas),
    output_file = paste0("Validation_Report_", areas, ".html"),
    pattern = map(areas)
    )
)