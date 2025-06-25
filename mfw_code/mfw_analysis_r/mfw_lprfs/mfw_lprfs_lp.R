# --- 1. Load Libraries ---
library(dplyr)
library(here)
library(lpirfs)

# --- 2. Define Script Parameters ---
LP_HORIZONS <- 10
LAGS_DEPENDENT <- 4
LAGS_CONTROLS <- 4
CONF_INTERVAL <- 1.96 

# --- 3. Load Prepared Data and Shocks ---
data_global_filename <- here::here("mfw_code", "mfw_analysis_r", "mfw_lprfs_output", "df_lprfs_global.rds")
if (!file.exists(data_global_filename)) {
  stop("Global data file not found. Please run the data prep script first.")
}
df_lprfs_global <- readRDS(data_global_filename)

shocks_filename <- here::here("mfw_code", "mfw_analysis_r", "mfw_lprfs_output", "df_lprfs_shocks.rds")
if (!file.exists(shocks_filename)) {
  stop("Shocks data file not found. Please run the shock identification script first.")
}
df_lprfs_shocks <- readRDS(shocks_filename)

# --- 4. Prepare Data for Local Projections ---

lp_input_data <- df_lprfs_global %>%
  dplyr::mutate(log_debt = log((Debt_to_GDP * GDP) / (GDPdeflator / 100))) %>% 
  dplyr::select(Date, log_debt) %>%      
  slice_tail(n = nrow(df_lprfs_shocks)) %>%
  bind_cols(df_lprfs_shocks)

# --- 5. Estimate Local Projections using lp_lin_iv ---
results_lp <- lp_lin_iv(
  endog_data      = lp_input_data %>% dplyr::select(log_debt),
  shock           = lp_input_data %>% dplyr::select(shock_primary_balance),
  exog_data       = lp_input_data %>% dplyr::select(log_exp, log_rev, log_gdp, log_p, BondYield),
  lags_endog_lin  = LAGS_DEPENDENT,
  lags_exog       = LAGS_CONTROLS,
  confint         = CONF_INTERVAL,
  hor             = LP_HORIZONS,
  use_nw          = TRUE, 
  trend           = 0     
)

# --- 6. Plot the Impulse Response Function ---
if (!is.null(results_lp)) {
  
  cat("\nLocal Projection estimation complete.\n")
  cat("Displaying generic plot from the 'lpirfs' package...\n")
  
  # The plot will appear in the 'Plots' pane in RStudio
  print(plot_lin(results_lp))
  
} else {
  cat("\nLocal Projection estimation failed or returned NULL. No plot generated.\n")
}

cat("\n--- Script mfw_lprf_lp.R finished ---\n")