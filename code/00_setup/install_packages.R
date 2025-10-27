# ============================================================================
# Install Required Packages for Kelly Criterion Project
# ============================================================================
# Purpose: Install all R packages needed for the project
# Run this once when setting up the project
# Date: October 26, 2025
# ============================================================================

cat("=======================================================\n")
cat("Installing packages for Kelly Criterion Project\n")
cat("=======================================================\n\n")

# Define required packages
required_packages <- c(
  # Data manipulation and processing
  "tidyverse",          # Data wrangling (dplyr, ggplot2, etc.)
  "data.table",         # Fast data manipulation
  
  # Financial data
  "quantmod",           # Download financial data from Yahoo Finance
  "TTR",                # Technical trading rules
  
  # Time series
  "xts",                # Extended time series
  "zoo",                # Time series utilities
  
  # Portfolio optimization
  "quadprog",           # Quadratic programming (for Kelly optimization)
  "MASS",               # Modern applied statistics
  
  # Performance analytics
  "PerformanceAnalytics", # Portfolio performance metrics
  
  # Visualization
  "ggplot2",            # Advanced plotting
  "corrplot",           # Correlation plots
  "gridExtra",          # Arrange multiple plots
  "scales",             # Scale functions for visualization
  
  # Reporting
  "knitr",              # Dynamic report generation
  "rmarkdown",          # R Markdown documents
  "kableExtra"          # Enhanced tables
)

# Function to install packages if not already installed
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  if(length(new_packages) > 0) {
    cat("Installing", length(new_packages), "new packages...\n")
    cat("Packages to install:", paste(new_packages, collapse = ", "), "\n\n")
    
    install.packages(new_packages, dependencies = TRUE)
    cat("\n✓ Installation complete!\n\n")
  } else {
    cat("✓ All required packages are already installed!\n\n")
  }
}

# Install missing packages
install_if_missing(required_packages)

# Load packages and check versions
cat("=======================================================\n")
cat("Package Versions:\n")
cat("=======================================================\n")

for(pkg in required_packages) {
  if(pkg %in% installed.packages()[,"Package"]) {
    version <- as.character(packageVersion(pkg))
    cat(sprintf("%-25s: %s\n", pkg, version))
  } else {
    cat(sprintf("%-25s: NOT INSTALLED\n", pkg))
  }
}

cat("\n=======================================================\n")
cat("Setup Complete!\n")
cat("=======================================================\n")

# Test that key packages work
cat("\nTesting key packages...\n")
library(quantmod)
cat("✓ quantmod loaded successfully!\n")
library(xts)
cat("✓ xts loaded successfully!\n")
library(quadprog)
cat("✓ quadprog loaded successfully!\n")

# Print R version info
cat("\n=======================================================\n")
cat("R Session Information:\n")
cat("=======================================================\n")
print(sessionInfo())