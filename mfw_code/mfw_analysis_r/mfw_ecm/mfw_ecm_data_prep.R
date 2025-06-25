library(readxl)
library(dplyr)
library(lubridate)
library(dynlm)
library(here)

cat("--- mfw_ecm_data_prep.R: Starting Data Preparation ---\n")

# --- Step 1: Data Retrieval and Initial Preparation ---
cat("Loading data from specified Excel file...\n")
file_path <- here::here("mfw_data", "pfmh_data.xlsx")
full_data <- read_excel(file_path)

data_ger <- full_data %>%
  filter(isocode == "DEU" & year >= 1950) %>%
  select(year, s_t = pb, b_t = debt) %>%
  mutate(
    s_t = as.numeric(s_t),
    b_t = as.numeric(b_t),
    year = as.integer(year)
  ) %>%
  arrange(year) %>%
  na.omit()

# --- Step 2: Estimate Cointegrating Relationships & Define EC terms ---
s_t_ts <- ts(data_ger$s_t, start = min(data_ger$year), frequency = 1)
b_t_ts <- ts(data_ger$b_t, start = min(data_ger$year), frequency = 1)

b_ec_model <- dynlm(stats::lag(b_t_ts, 2) ~ stats::lag(s_t_ts, 1) - 1)
b_beta_hat <- coef(b_ec_model)[1]

s_ec_model <- dynlm(stats::lag(s_t_ts, 1) ~ stats::lag(b_t_ts, 2) - 1)
s_beta_hat <- coef(s_ec_model)[1]

b_ec_term <- as.numeric(stats::lag(b_t_ts, 2)) - b_beta_hat * as.numeric(stats::lag(s_t_ts, 1))
s_ec_term <- as.numeric(stats::lag(s_t_ts, 1)) - s_beta_hat * as.numeric(stats::lag(b_t_ts, 2))

# --- Step 3: Create the Comprehensive Data Frame ---
df_global <- data_ger %>%
  mutate(
    s_t_lag1 = lag(s_t, 1),
    s_t_lag2 = lag(s_t, 2),
    s_t_lag3 = lag(s_t, 3),
    s_t_lag4 = lag(s_t, 4),
    s_t_lag5 = lag(s_t, 5), 
    b_t_lag1 = lag(b_t, 1),
    b_t_lag2 = lag(b_t, 2),
    b_t_lag3 = lag(b_t, 3),
    b_t_lag4 = lag(b_t, 4),
    b_t_lag5 = lag(b_t, 5) 
  ) %>%
  mutate(
    delta_s_t = s_t - s_t_lag1,
    delta_s_t_lag1 = s_t_lag1 - s_t_lag2,
    delta_s_t_lag2 = s_t_lag2 - s_t_lag3,
    delta_s_t_lag3 = s_t_lag3 - s_t_lag4,
    delta_s_t_lag4 = s_t_lag4 - s_t_lag5, 
    delta_b_t = b_t - b_t_lag1,
    delta_b_t_lag1 = b_t_lag1 - b_t_lag2,
    delta_b_t_lag2 = b_t_lag2 - b_t_lag3,
    delta_b_t_lag3 = b_t_lag3 - b_t_lag4,
    delta_b_t_lag4 = b_t_lag4 - b_t_lag5,
    b_ec_term = b_ec_term,
    s_ec_term = s_ec_term
  ) %>%
  na.omit()

# --- Step 4: Finalize and Save to File ---
output_dir <- here::here("mfw_code", "mfw_analysis_r", "mfw_ecm_output")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

output_filename <- here::here("mfw_code", "mfw_analysis_r", "mfw_ecm_output", "df_global.rds")
saveRDS(df_global, file = output_filename)

cat("\n--- Data Preparation Complete ---\n")
cat("Successfully created 'df_global' with", ncol(df_global), "columns and", nrow(df_global), "rows.\n")
cat("Data frame saved to file: df_global.rds\n")

cat("\n--- End of mfw_ecm_data_prep.R ---\n")