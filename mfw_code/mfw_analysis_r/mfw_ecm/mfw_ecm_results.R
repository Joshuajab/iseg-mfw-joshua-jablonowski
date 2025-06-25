library(here)
library(knitr)

# --- 2. Define Script and Output Locations ---
scripts_dir <- here::here("mfw_code", "mfw_analysis_r", "mfw_ecm")
output_dir <- here::here("mfw_code", "mfw_analysis_r", "mfw_ecm_output")

# --- 3. Define Analysis Scripts to Run (in order) ---

scripts_to_run <- c(
  "mfw_ecm_data_prep.R",      
  "mfw_ecm_nls_aic.R",        
  "mfw_ecm_granger_aic_b.R",  
  "mfw_ecm_granger_aic_s.R",  
  "mfw_ecm_nls.R",            
  "mfw_ecm_granger_b.R",      
  "mfw_ecm_granger_s.R"       
)

# --- 4. Execute the Full Analysis Pipeline ---
cat("--- START RUNNING ECM SCRIPTS ---\n")
for (script in scripts_to_run) {
  script_path <- file.path(scripts_dir, script)
  
  
  if (!file.exists(script_path)) {
    stop(paste("Script not found:", script_path))
  }
  
  tryCatch({
    cat(paste("\n--- Running script:", script, "---\n"))
    source(script_path)
    cat(paste("--- Successfully finished:", script, "---\n"))
  }, error = function(e) {
    
    stop(paste("An error occurred while running", script, ":\n", e$message))
  })
}
cat("\n--- All ECM analysis scripts have been run successfully. ---\n\n")


# --- 5. Load and Compile Final Results ---
cat("--- DISPLAYING FINAL RESULTS ---\n")

# Load NLS model object
nls_model_filename <- here::here(output_dir, "best_nls_ecm_model.rds")
if (!file.exists(nls_model_filename)) stop("NLS model file not found. Please ensure mfw_ecm_nls_aic.R ran successfully.")
best_nls_model <- readRDS(nls_model_filename)

# Load Granger causality results for s --> b
granger_b_filename <- here::here(output_dir, "granger_b_results.rds")
if (!file.exists(granger_b_filename)) stop("Granger (b) results file not found.")
granger_b_results <- readRDS(granger_b_filename)

# Load Granger causality results for b --> s
granger_s_filename <- here::here(output_dir, "granger_s_results.rds")
if (!file.exists(granger_s_filename)) stop("Granger (s) results file not found.")
granger_s_results <- readRDS(granger_s_filename)


# --- 6. Format and Print Result Tables ---

# Helper function to add significance stars based on p-value
add_stars <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value < 0.01) return("**")
  if (p_value < 0.05) return("*")
  return("")
}

# --- TABLE III: NLS Estimation Results ---
cat("\n--- TABLE III: ESTIMATION OF A LONG-RUN RELATIONSHIP (NLS) ---\n")
nls_results_table <- summary(best_nls_model)$coefficients
nls_table_formatted <- data.frame(
  Variable = c("Error-correction coefficient (rho)", "Long-run coefficient (beta)"),
  Coefficient = sprintf("%.4f", nls_results_table[c("rho", "beta"), "Estimate"]),
  `p-value` = sprintf("%.4f", nls_results_table[c("rho", "beta"), "Pr(>|t|)"]),
  Signif = sapply(nls_results_table[c("rho", "beta"), "Pr(>|t|)"], add_stars),
  check.names = FALSE
)
print(knitr::kable(nls_table_formatted, caption = "NLS Long-Run Relationship"))
cat("\n\n")


# --- TABLE IV: Granger Causality Results ---
cat("--- TABLE IV: SIMS-STOCK-WATSON TESTS FOR GRANGER-CAUSALITY ---\n")
granger_table <- data.frame(
  `H0` = c("gamma = 0 (Long-Run)", "alpha_2i = 0 (Short-Run)"),
  `s -> b` = paste0(
    sprintf("%.3f", granger_b_results$F_Statistic),
    sapply(granger_b_results$P_Value, add_stars)
  ),
  `b -> s` = paste0(
    sprintf("%.3f", granger_s_results$F_Statistic),
    sapply(granger_s_results$P_Value, add_stars)
  ),
  check.names = FALSE
)
print(knitr::kable(granger_table, caption = "Granger Causality F-Statistics"))
cat("** Significance at the 1% level.\n")
cat("* Significance at the 5% level.\n")
cat("\n--- Workflow Complete. ---\n")