#!/usr/bin/env Rscript
# ==============================================================================
# BACKGROUND PIPELINE EXECUTION SCRIPT
# Run with: Rscript run_pipeline.R &
# Or in R: source("run_pipeline.R")
# ==============================================================================

# Record start time
start_time <- Sys.time()
cat("="
    , rep("=", 60), "\n", sep = "")
cat("Pipeline started at:", format(start_time, "%Y-%m-%d %H:%M:%S"), "\n")
cat(rep("=", 61), "\n\n", sep = "")

# Set up logging
log_file <- paste0("pipeline_log_", format(Sys.Date(), "%Y%m%d"), ".txt")
sink(log_file, append = FALSE, split = TRUE)  # Log to file AND console

# Load targets
library(targets)

# Run the pipeline with error handling
tryCatch({
  
  cat("Running tar_make()...\n\n")
  tar_make()
  
  cat("\n", rep("=", 61), "\n", sep = "")
  cat("Pipeline completed SUCCESSFULLY\n")
  
}, error = function(e) {
  
  cat("\n", rep("=", 61), "\n", sep = "")
  cat("Pipeline FAILED with error:\n")
  cat(conditionMessage(e), "\n")
  
}, finally = {
  
  # Record end time and duration
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "mins")
  
  cat(rep("=", 61), "\n", sep = "")
  cat("End time:", format(end_time, "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Total duration:", round(duration, 2), "minutes\n")
  cat(rep("=", 61), "\n", sep = "")
  
  # Stop logging
  sink()
  
  # Print summary to console
  cat("\nLog saved to:", log_file, "\n")
})

# Optional: Play sound when done (works on macOS)
# system("afplay /System/Library/Sounds/Glass.aiff")

# Optional: Send notification (works on macOS)
# system('osascript -e "display notification \\"Pipeline finished\\" with title \\"targets\\""')