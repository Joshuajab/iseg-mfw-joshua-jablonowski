library(dplyr)
library(AICcmodavg)
library(here)

# --- Step 1: Load Prepared Data ---

# Define the path to the prepared data file
data_filename <- here::here("mfw_code", "mfw_analysis_r", "mfw_ecm_output", "df_global.rds")

# Check if the file exists before trying to load it
if (!file.exists(data_filename)) {
  stop("Data file not found: '", data_filename, "'. Please run the data prep script first.")
}

# Load the data frame from the file
df_global <- readRDS(data_filename)
df_analysis <- df_global

# --- 1. Set up the Model Selection Loop ---

# Define the grid of lag combinations to test
lag_combinations <- expand.grid(m = 1:3, n = 1:3)

# Initialize a list to store the fitted model objects
nls_models_list <- list()

# Define the base starting values that are common to all models
base_start_values <- list(omega = 0.1, rho = -0.2, beta = 0.02)

# --- 2. Loop Through Combinations, Build Formulas & Start Values, and Fit Models ---

for (i in 1:nrow(lag_combinations)) {
  m <- lag_combinations$m[i]
  n <- lag_combinations$n[i]
  
  # --- A: Programmatically build the formula string ---
  
  # Start with the non-linear base of the formula
  base_formula <- "delta_s_t ~ omega + rho * (s_t_lag1 - beta * b_t_lag2)"
  
  # Create the string for the gamma and delta terms
  
  gamma_terms <- paste0("gamma", 1:m, " * delta_s_t_lag", 1:m, collapse = " + ")
  delta_terms <- paste0("delta", 1:n, " * delta_b_t_lag", 1:n, collapse = " + ")
  
  # Combine all parts into the final formula string
  full_formula_string <- paste(base_formula, gamma_terms, delta_terms, sep = " + ")
  model_formula <- as.formula(full_formula_string)
  
  # --- B: Programmatically build the list of starting values ---
  
  # Create named lists for the gamma and delta starting values
  gamma_starts <- setNames(as.list(rep(0.01, m)), paste0("gamma", 1:m))
  delta_starts <- setNames(as.list(rep(0.01, n)), paste0("delta", 1:n))
  
  # Combine the base starts with the dynamic gamma and delta starts
  start_values <- c(base_start_values, gamma_starts, delta_starts)
  
  # --- C: Fit the NLS model and store it ---
  
  model_name <- paste0("nls_ecm_m", m, "_n", n)
  cat(paste0("Fitting ", model_name, "\n"))
  
  tryCatch({
    fit <- nls(model_formula, 
               data = df_analysis, 
               start = start_values,
               control = nls.control(maxiter = 1000, warnOnly = TRUE),
               algorithm = "port")
    
    nls_models_list[[model_name]] <- fit
    
  }, error = function(e) cat("Error fitting", model_name, ":", e$message, "\n"))
}

# --- 3. Calculate AIC, Find Best Model, and Save ---

aic_table <- AICcmodavg::aictab(cand.set = nls_models_list, 
                                modnames = names(nls_models_list),
                                second.ord = FALSE) # Use AIC instead of AICc
print(aic_table)

# Extract the best model object
best_nls_model_name <- as.character(aic_table$Modnames[1])
best_nls_model <- nls_models_list[[best_nls_model_name]]

# Define the output filename
output_filename <- here::here("mfw_code", "mfw_analysis_r", "mfw_ecm_output", "best_nls_ecm_model.rds")

# Save the single best NLS model object
saveRDS(best_nls_model, file = output_filename)

cat("\n--- Best Fitting NLS Model ---\n")
cat("Model Name:", best_nls_model_name, "\n")
cat("AIC:", round(aic_table$AIC[1], 2), "\n")
cat("Saved to file:", output_filename, "\n")
print(summary(best_nls_model))