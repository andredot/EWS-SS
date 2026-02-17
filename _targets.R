library(targets)
library(tarchetypes)
library(tidyverse)
library(readxl)
library(mgcv)

# 1. Source your custom functions (put your logic here)
source("R/functions.R") 

# 2. Set options
tar_option_set(packages = c("tidyverse", "mgcv", "readxl", "metrics"))

# 3. Define the pipeline
list(
  # --- Step 1: Load Data ---
  tar_target(raw_file, "Estrazione_accessi_PS.xlsx", format = "file"),
  tar_target(df_long, load_and_clean_data(raw_file)),
  
  # --- Step 2: Define Areas to Loop Over ---
  # We map over the areas so we get results for all 3
  tar_target(areas, c("Lombardia", "Milano", "Valtellina")),
  
  # --- Step 3: Rolling Validation (The Slow Part) ---
  # This runs the validation loop for each area
  tar_target(
    validation_results,
    run_rolling_validation(df_long, area_name = areas),
    pattern = map(areas) # Creates 3 branches (one per area)
  ),
  
  # --- Step 4: Sensitivity Analysis (The Very Slow Part) ---
  tar_target(
    sensitivity_results,
    run_sensitivity_simulation(validation_results),
    pattern = map(validation_results)
  ),
  
  # --- Step 5: Render Reports ---
  # This triggers the .qmd render, passing the pre-calculated data
  tar_render(
    report,
    "validation_report.qmd",
    params = list(area = areas), # Dynamic parameters
    output_file = paste0("validation_report_", areas, ".html")
  )
)