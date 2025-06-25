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

# --- Step 2: Systematic Model Fitting ---

# 2A: Define the grid of lag combinations (m for delta_b, n for delta_s) to test
lag_combinations <- expand.grid(m = 1:3, n = 1:3)

# 2B: Initialize a list to store the fitted model objects
model_list <- list()

# 2C: Loop through each combination, build a formula, and fit the model
for (i in 1:nrow(lag_combinations)) {
  m <- lag_combinations$m[i]
  n <- lag_combinations$n[i]
  
  # Programmatically create the summation terms for the lagged differences
  lags_b <- paste0("delta_b_t_lag", (1:m) + 1, collapse = " + ")
  lags_s <- paste0("delta_s_t_lag", 1:n, collapse = " + ")
  
  # Build the full model formula based on the ECM structure
  model_formula <- as.formula(
    paste("b_t_lag1 ~ b_t_lag2 + b_ec_term +", lags_b, "+", lags_s)
  )
  
  # Fit the linear model using the static, complete data frame
  fit <- lm(model_formula, data = df_analysis)
  
  # Store the fitted model in our list with a descriptive name
  model_name <- paste0("b_ecm_m", m, "_n", n)
  model_list[[model_name]] <- fit
}

# --- Step 3: Compare Models with AICc and Print Results ---

# Create the AIC comparison table from the list of fitted models.
aic_table <- aictab(
  cand.set = model_list, 
  modnames = names(model_list)
)

# Print the final results table
cat("--- AICc Model Selection Table ---\n")
print(aic_table)

# Programmatically extract the best model's name
best_model_name <- as.character(aic_table$Modnames[1])
best_model <- model_list[[best_model_name]]

# --- Step 4: Save the Best Model Object for Later Use ---

# Define a filename for the saved model
output_filename <- here::here("mfw_code", "mfw_analysis_r", "mfw_ecm_output", "best_b_ecm_model.rds")

# Save the single 'best_model' object to the file
saveRDS(best_model, file = output_filename)

cat("\n--- Best Fitting Model ---\n")
cat("Model Name:", best_model_name, "\n")
cat("AICc:", round(aic_table$AICc[1], 2), "\n")
cat("Formula:\n")
print(formula(best_model))
cat("\nSummary of Best Model:\n")
print(summary(best_model))