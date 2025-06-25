library(readr)
library(dplyr)
library(zoo)
library(xts)
library(here)

# --- 2. Central Configuration ---

folder_path <- here::here("mfw_data", "mfw_lprfs_data")

file_metadata <- list(
  GDP = list(
    file = "GDP_quarterly.csv",
    date_col = "TIME PERIOD",
    value_col = "Gross domestic product at market prices (MNA.Q.Y.DE.W2.S1.S1.B.B1GQ._Z._Z._Z.EUR.V.N)",
    date_format = "%YQ%q"
  ),
  Expenditure = list(
    file = "government_expenditure_quarterly.csv",
    date_col = "TIME PERIOD",
    value_col = "Government total expenditure (seasonally adjusted) (GFS.Q.Y.DE.W0.S13.S1.P.D.OTE._Z._Z._T.XDC._Z.S.V.N._T)",
    date_format = "%YQ%q"
  ),
  Revenue = list(
    file = "government_revenue_quarterly.csv",
    date_col = "TIME PERIOD",
    value_col = "Government total revenue (seasonally adjusted) (GFS.Q.Y.DE.W0.S13.S1.P.C.OTR._Z._Z._Z.XDC._Z.S.V.N._T)",
    date_format = "%YQ%q"
  ),
  BondYield = list(
    file = "10_year_bond_yield_quarterly.csv",
    date_col = "TIME PERIOD",
    value_col = "Long-term interest rate for convergence purposes - 10 years maturity, denominated in Euro - Germany (IRS.M.DE.L.L40.CI.0000.EUR.N.Z) - Modified value (Quarterly)",
    date_format = "%YQ%q"
  ),
  Debt_to_GDP = list(
    file = "government_debt_to_gdp_quarterly.csv",
    date_col = "TIME PERIOD",
    value_col = "Government debt (consolidated) (as % of GDP) (GFS.Q.N.DE.W0.S13.S1.C.L.LE.GD.T._Z.XDC_R_B1GQ_CY._T.F.V.N._T)",
    date_format = "%YQ%q"
  ),
  GDPdeflator = list(
    file = "GDP_deflator_quarterly.csv", 
    date_col = "TIME PERIOD",    
    value_col = "Gross domestic product at market prices (MNA.Q.Y.DE.W2.S1.S1.B.B1GQ._Z._Z._Z.IX.D.N)", 
    date_format = "%YQ%q"          
  )
)

# --- 3. General-Purpose Processing Function ---
process_timeseries_file <- function(meta, base_path, var_name) {
  full_path <- file.path(base_path, meta$file)
  
  
  df <- tryCatch({
    readr::read_csv(full_path, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
  }, error = function(e) {
    warning(paste("Error reading", meta$file, ":", e$message)); return(NULL)
  })
  
  if (is.null(df) || nrow(df) == 0) {
    warning(paste(meta$file, "is empty or could not be read.")); return(NULL)
  }
  
  if (!all(c(meta$date_col, meta$value_col) %in% names(df))) {
    warning(paste("Required columns not found in", meta$file, ". Check metadata.")); return(NULL)
  }
  
  dates <- zoo::as.yearqtr(df[[meta$date_col]], format = meta$date_format)
  values <- as.numeric(df[[meta$value_col]])
  ts_obj <- xts::xts(values, order.by = dates)
  
  colnames(ts_obj) <- var_name
  return(ts_obj)
}

# --- 4. Loop Through Configuration and Process All Files ---
ts_list <- lapply(names(file_metadata), function(name) {
  meta <- file_metadata[[name]]
  process_timeseries_file(meta, folder_path, name)
})
ts_list <- Filter(Negate(is.null), ts_list)

# --- 5. Merge, Align, Finalize, and Save ---

if (length(ts_list) > 0) {
  
  merged_ts <- do.call(xts::merge.xts, c(ts_list, all = TRUE, fill = NA))
  
  df_merged_aligned <- data.frame(Date = zoo::as.Date(index(merged_ts)), coredata(merged_ts))
  df_final <- na.omit(df_merged_aligned)
  
  
  if (nrow(df_final) > 0) {
    
    output_dir <- here::here("mfw_code", "mfw_analysis_r", "mfw_lprfs_output")
    
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    output_filename <- here::here("mfw_code", "mfw_analysis_r", "mfw_lprfs_output", "df_lprfs_global.rds")
    
    saveRDS(df_final, file = output_filename)
    
    cat("Script finished. Final data frame saved to:", output_filename, "\n")
    
  } else {
    warning("Final dataframe is empty after NA omission. No file was saved.")
  }
} else {
  warning("No time series were successfully processed. No file was saved.")
}