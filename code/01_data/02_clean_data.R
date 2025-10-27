# ============================================================================
# Clean and Prepare Euro Stoxx 50 Data
# ============================================================================
# Purpose: Quality checks, missing data handling, filter stocks
# Input: data/raw/euro_stoxx50_prices.rds
# Output: data/processed/prices_clean.rds
# Date: October 26, 2025
# ============================================================================

# Clear environment
rm(list = ls())

# Load required libraries
library(xts)
library(zoo)
library(tidyverse)
library(PerformanceAnalytics)

cat("=======================================================\n")
cat("Euro Stoxx 50 Data Cleaning Script\n")
cat("=======================================================\n\n")

# ============================================================================
# Load Raw Data
# ============================================================================

cat("Loading raw price data...\n")

if(!file.exists("data/raw/euro_stoxx50_prices.rds")) {
  stop("Error: Raw data file not found. Please run 01_download_data.R first.")
}

prices_raw <- readRDS("data/raw/euro_stoxx50_prices.rds")

cat("✓ Data loaded successfully\n")
cat("  Dimensions:", nrow(prices_raw), "rows ×", ncol(prices_raw), "columns\n")
cat("  Date range:", as.character(start(prices_raw)), "to", 
    as.character(end(prices_raw)), "\n\n")

# ============================================================================
# Data Quality Assessment
# ============================================================================

cat("=======================================================\n")
cat("Data Quality Assessment\n")
cat("=======================================================\n\n")

# Analysis period
analysis_start <- "2007-01-01"
analysis_end <- "2018-12-31"

# Subset to analysis period
prices_analysis <- prices_raw[paste0(analysis_start, "/", analysis_end)]

cat("Analysis period subset:\n")
cat("  Start:", as.character(start(prices_analysis)), "\n")
cat("  End:", as.character(end(prices_analysis)), "\n")
cat("  Observations:", nrow(prices_analysis), "\n\n")

# Calculate missing data statistics per stock
cat("Calculating missing data statistics...\n\n")

missing_stats <- data.frame(
  ticker = colnames(prices_analysis),
  total_obs = nrow(prices_analysis),
  missing_count = colSums(is.na(prices_analysis)),
  missing_pct = colSums(is.na(prices_analysis)) / nrow(prices_analysis) * 100,
  first_valid_date = sapply(1:ncol(prices_analysis), function(i) {
    first_valid <- which(!is.na(prices_analysis[,i]))[1]
    if(length(first_valid) > 0) as.character(index(prices_analysis)[first_valid]) else NA
  }),
  last_valid_date = sapply(1:ncol(prices_analysis), function(i) {
    last_valid <- tail(which(!is.na(prices_analysis[,i])), 1)
    if(length(last_valid) > 0) as.character(index(prices_analysis)[last_valid]) else NA
  }),
  mean_price = apply(prices_analysis, 2, mean, na.rm = TRUE),
  sd_price = apply(prices_analysis, 2, sd, na.rm = TRUE),
  stringsAsFactors = FALSE
)

# Sort by missing percentage
missing_stats <- missing_stats[order(missing_stats$missing_pct, decreasing = TRUE), ]

cat("Stocks with missing data (sorted by missing %):\n")
print(missing_stats[missing_stats$missing_count > 0, ])

cat("\nOverall missing data summary:\n")
cat("  Total data points:", nrow(prices_analysis) * ncol(prices_analysis), "\n")
cat("  Total missing:", sum(is.na(prices_analysis)), "\n")
cat("  Missing percentage:", 
    round(sum(is.na(prices_analysis)) / (nrow(prices_analysis) * ncol(prices_analysis)) * 100, 2), "%\n\n")

# Save quality report
write.csv(missing_stats, "data/processed/data_quality_report.csv", row.names = FALSE)
cat("✓ Quality report saved to: data/processed/data_quality_report.csv\n\n")

# ============================================================================
# Filter Stocks Based on Data Quality
# ============================================================================

cat("=======================================================\n")
cat("Filtering Stocks\n")
cat("=======================================================\n\n")

# Filtering criteria
max_missing_pct <- 5  # Keep stocks with < 5% missing data

cat("Filtering criteria:\n")
cat("  Maximum missing data allowed:", max_missing_pct, "%\n\n")

# Identify good stocks
good_tickers <- missing_stats$ticker[missing_stats$missing_pct < max_missing_pct]

cat("Filtering results:\n")
cat("  Original stocks:", ncol(prices_analysis), "\n")
cat("  Stocks meeting criteria:", length(good_tickers), "\n")
cat("  Stocks removed:", ncol(prices_analysis) - length(good_tickers), "\n\n")

if(length(good_tickers) < ncol(prices_analysis)) {
  removed_tickers <- setdiff(colnames(prices_analysis), good_tickers)
  cat("Removed stocks:\n")
  for(ticker in removed_tickers) {
    missing_pct <- missing_stats$missing_pct[missing_stats$ticker == ticker]
    cat("  -", ticker, "(", round(missing_pct, 2), "% missing)\n")
  }
  cat("\n")
}

# Filter to good stocks
prices_filtered <- prices_analysis[, good_tickers]

cat("After filtering:\n")
cat("  Stocks:", ncol(prices_filtered), "\n")
cat("  Observations:", nrow(prices_filtered), "\n")
cat("  Date range:", as.character(start(prices_filtered)), "to", 
    as.character(end(prices_filtered)), "\n\n")

# ============================================================================
# Handle Remaining Missing Values
# ============================================================================

cat("=======================================================\n")
cat("Handling Missing Values\n")
cat("=======================================================\n\n")

# Count remaining NAs
remaining_nas <- sum(is.na(prices_filtered))
cat("Remaining missing values:", remaining_nas, "\n")

if(remaining_nas > 0) {
  cat("Applying forward fill (na.locf)...\n")
  
  # Forward fill: carry last observation forward
  prices_clean <- na.locf(prices_filtered, na.rm = FALSE)
  
  # Check if any NAs remain at the beginning
  initial_nas <- sum(is.na(prices_clean))
  
  if(initial_nas > 0) {
    cat("Applying backward fill for initial NAs...\n")
    prices_clean <- na.locf(prices_clean, fromLast = TRUE)
  }
  
  # Final check
  final_nas <- sum(is.na(prices_clean))
  cat("✓ Missing values after cleaning:", final_nas, "\n\n")
  
} else {
  prices_clean <- prices_filtered
  cat("✓ No missing values to handle\n\n")
}

# ============================================================================
# Final Data Validation
# ============================================================================

cat("=======================================================\n")
cat("Final Data Validation\n")
cat("=======================================================\n\n")

# Check for any remaining issues
cat("Checking for data issues...\n\n")

# 1. Check for zeros or negative prices (shouldn't exist)
zero_prices <- sum(prices_clean <= 0, na.rm = TRUE)
cat("  Prices <= 0:", zero_prices, "\n")

# 2. Check for extreme values
price_ranges <- data.frame(
  ticker = colnames(prices_clean),
  min_price = apply(prices_clean, 2, min, na.rm = TRUE),
  max_price = apply(prices_clean, 2, max, na.rm = TRUE),
  mean_price = apply(prices_clean, 2, mean, na.rm = TRUE),
  cv = apply(prices_clean, 2, sd, na.rm = TRUE) / 
       apply(prices_clean, 2, mean, na.rm = TRUE)
)

cat("\nPrice statistics (first 10 stocks):\n")
print(head(price_ranges, 10))

# 3. Summary
cat("\n=======================================================\n")
cat("Clean Data Summary\n")
cat("=======================================================\n\n")

cat("Final dataset:\n")
cat("  Stocks:", ncol(prices_clean), "\n")
cat("  Trading days:", nrow(prices_clean), "\n")
cat("  Start date:", as.character(start(prices_clean)), "\n")
cat("  End date:", as.character(end(prices_clean)), "\n")
cat("  Missing values:", sum(is.na(prices_clean)), "\n")
cat("  Total data points:", nrow(prices_clean) * ncol(prices_clean), "\n\n")

# ============================================================================
# Save Clean Data
# ============================================================================

cat("Saving cleaned data...\n")

# Save as RDS
saveRDS(prices_clean, "data/processed/prices_clean.rds")
cat("✓ Clean prices saved to: data/processed/prices_clean.rds\n")

# Save as CSV for inspection
write.zoo(prices_clean, "data/processed/prices_clean.csv", sep = ",")
cat("✓ CSV saved to: data/processed/prices_clean.csv\n")

# Save list of final tickers
final_tickers <- data.frame(
  ticker = colnames(prices_clean),
  included = TRUE
)
write.csv(final_tickers, "data/processed/final_ticker_list.csv", row.names = FALSE)
cat("✓ Final ticker list saved to: data/processed/final_ticker_list.csv\n")

cat("\n=======================================================\n")
cat("Data Cleaning Complete!\n")
cat("=======================================================\n\n")

cat("Next steps:\n")
cat("1. Calculate returns: source('code/01_data/03_calculate_returns.R')\n")
cat("2. Review quality report: data/processed/data_quality_report.csv\n\n")

cat("Script completed at:", as.character(Sys.time()), "\n")