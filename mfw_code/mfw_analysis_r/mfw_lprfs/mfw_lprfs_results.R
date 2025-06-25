library(here)

scripts_dir <- here::here("mfw_code", "mfw_analysis_r", "mfw_lprfs")

# --- 3. Define the scripts to be run in the correct order ---
scripts_to_run <- c(
  "mfw_lprfs_data_prep.R", 
  "mfw_lprfs_shocks.R",    
  "mfw_lprfs_lp.R"         
)

# --- 4. Execute the Full Analysis Pipeline ---
cat("--- STARTING FULL LOCAL PROJECTION ANALYSIS PIPELINE ---\n")
for (script in scripts_to_run) {
  
  # Construct the full path to the script
  script_path <- file.path(scripts_dir, script)
  
  # Check if the script file exists before trying to run it
  if (!file.exists(script_path)) {
    stop(paste("Script not found:", script_path))
  }
  
  # Use tryCatch for robust error handling
  tryCatch({
    
    cat(paste("\n--- Running script:", script, "---\n"))
    # The 'source' function executes the R script
    source(script_path)
    cat(paste("--- Successfully finished:", script, "---\n"))
    
  }, error = function(e) {
    # If any script fails, stop the master script and report the error
    stop(paste("An error occurred while running", script, ":\n", e$message))
  })
}

cat("\n--- All Local Projection scripts have been run successfully. ---\n")
