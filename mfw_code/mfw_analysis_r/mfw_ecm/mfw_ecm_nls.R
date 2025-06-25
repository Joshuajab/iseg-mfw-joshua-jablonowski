library(dplyr)
library(lmtest)     
library(sandwich)   
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

model_filename <- here::here("mfw_code", "mfw_analysis_r", "mfw_ecm_output", "best_nls_ecm_model.rds")

# Check if the file exists before trying to load it
if (!file.exists(model_filename)) {
  stop("Model file not found:", model_filename, ". Please run the NLS model selection script first.")
}

# Load the saved model object
best_nls_model <- readRDS(model_filename)

cat("Model Formula:\n")
print(formula(best_nls_model))

# --- Step 2: Display Model Coefficients and Significance ---

# 2A: Show the standard summary output from the nls fit
cat("\n--- Standard NLS Model Summary ---\n")
print(summary(best_nls_model))