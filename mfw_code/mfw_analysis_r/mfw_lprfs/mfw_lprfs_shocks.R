# Load Libraries
library(dplyr)
library(here)
library(vars)
library(lubridate)

# Load Prepared Data
data_filename <- here::here("mfw_code", "mfw_analysis_r", "mfw_lprfs_output", "df_lprfs_global.rds")
if (!file.exists(data_filename)) {
  stop("Data file not found: '", data_filename, "'. Please run the data prep script first.")
}
df_lprfs_global <- readRDS(data_filename)

# Prepare Data for VAR
df_var_data <- df_lprfs_global %>%
  arrange(Date) %>%
  mutate(
    gdp_real = GDP / (GDPdeflator / 100),
    exp_real = Expenditure / (GDPdeflator / 100),
    rev_real = Revenue / (GDPdeflator / 100)
  ) %>%
  mutate(
    log_gdp = log(ifelse(gdp_real > 0, gdp_real, NA)),
    log_exp = log(ifelse(exp_real > 0, exp_real, NA)),
    log_rev = log(ifelse(rev_real > 0, rev_real, NA)),
    log_p = log(ifelse(GDPdeflator > 0, GDPdeflator, NA)),
  ) %>%
  dplyr::select(
    log_exp,
    log_rev,
    log_gdp,
    log_p,
    BondYield
  )

# Estimate the Reduced-Form VAR
var_data_ts <- ts(df_var_data, start = c(year(min(df_lprfs_global$Date)), quarter(min(df_lprfs_global$Date))), frequency = 4)
lag_selection <- vars::VARselect(var_data_ts, lag.max = 8, type = "const")
p <- lag_selection$selection["AIC(n)"] 
var_model <- vars::VAR(var_data_ts, p = p, type = "const")

# Identify Structural Shocks (Blanchard & Perotti method)
alpha_exp_y <- 0
alpha_exp_p <- -0.5
alpha_exp_i <- 0
alpha_rev_y <- 0.72
alpha_rev_p <- 0.98
alpha_rev_i <- 0

residuals_matrix <- residuals(var_model)
u_exp <- residuals_matrix[, "log_exp"]
u_rev <- residuals_matrix[, "log_rev"]
u_gdp <- residuals_matrix[, "log_gdp"]
u_GDPdeflator <- residuals_matrix[, "log_p"]
u_yield <- residuals_matrix[, "BondYield"]

u_exp_ca <- u_exp - (alpha_exp_y * u_gdp + alpha_exp_p * u_GDPdeflator + alpha_exp_i * u_yield)
u_rev_ca <- u_rev - (alpha_rev_y * u_gdp + alpha_rev_p * u_GDPdeflator + alpha_rev_i * u_yield)

e_exp <- u_exp_ca
rev_on_exp_model <- lm(u_rev_ca ~ e_exp)
e_rev <- residuals(rev_on_exp_model)

# Finalize Data and Save
shocks_df <- data.frame(
  shock_exp = e_exp,
  shock_rev = e_rev
) %>%
  mutate(
    shock_primary_balance = shock_rev - shock_exp
  )

p <- var_model$p 
aligned_var_data <- df_var_data[(p + 1):nrow(df_var_data), ]
final_data_with_shocks <- bind_cols(aligned_var_data, shocks_df)

output_dir <- here::here("mfw_code", "mfw_analysis_r", "mfw_lprfs_output")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
output_filename <- here::here("mfw_code", "mfw_analysis_r", "mfw_lprfs_output", "df_lprfs_shocks.rds")
saveRDS(final_data_with_shocks, file = output_filename)