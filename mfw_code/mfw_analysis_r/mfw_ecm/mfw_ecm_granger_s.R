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

# Define the filename where the best model was saved
model_filename <- here::here("mfw_code", "mfw_analysis_r", "mfw_ecm_output", "best_s_ecm_model.rds")

# Check if the file exists before trying to load it
if (!file.exists(model_filename)) {
  stop("Model file not found:", model_filename, ". Please run the model selection script first.")
}

# Load the saved model object
unrestricted_model <- readRDS(model_filename)
print(formula(unrestricted_model))

# --- Step 2: Perform Granger Causality Tests on the Loaded Model ---

# 2A: Test for Long-Run Granger Causality (b --> s); H0: Coefficient of 's_ec_term' = 0
restricted_model_lr <- update(unrestricted_model, . ~ . - s_ec_term)
cat("ROBUST Wald Test for Long-Run Causality:\n")
lr_wald_test_robust <- lmtest::waldtest(unrestricted_model, restricted_model_lr, test = "F", 
                                        vcov = sandwich::vcovHC(unrestricted_model, type = "HC1"))
print(lr_wald_test_robust)

# 2B: Test for Short-Run Granger Causality (b --> s); H0: All coefficients for delta_b_t lags are simultaneously zero.

all_coeffs <- names(coef(unrestricted_model))
# Find which of those are the delta_b_t lag terms we need to test
sr_causality_vars <- all_coeffs[grep("^delta_b_t_lag", all_coeffs)]

# Create the restricted model by removing these variables
terms_to_remove <- paste(sr_causality_vars, collapse = " - ")
restricted_formula_sr <- as.formula(paste(". ~ . -", terms_to_remove))
restricted_model_sr <- update(unrestricted_model, restricted_formula_sr)

cat("ROBUST Wald Test for Short-Run Causality:\n")
sr_wald_test_robust <- lmtest::waldtest(unrestricted_model, restricted_model_sr, test = "F", 
                                        vcov = sandwich::vcovHC(unrestricted_model, type = "HC1"))
print(sr_wald_test_robust)

# --- 3. Compile and Save Results ---
results_s <- data.frame(
  Test = c("Long-Run (gamma = 0)", "Short-Run (alpha_2i = 0)"),
  F_Statistic = c(lr_wald_test_robust$F[2], sr_wald_test_robust$F[2]),
  P_Value = c(lr_wald_test_robust$`Pr(>F)`[2], sr_wald_test_robust$`Pr(>F)`[2])
)

output_dir <- here::here("mfw_code", "mfw_analysis_r", "mfw_ecm_output")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

output_filename <- here::here(output_dir, "granger_s_results.rds")
saveRDS(results_s, file = output_filename)

cat("--- Granger Test (b-->s) Complete ---\n")
cat("Results saved to:", output_filename, "\n")