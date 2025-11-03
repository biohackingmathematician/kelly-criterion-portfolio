# Streamlined portfolio optimization: Kelly (long-only) and Min-Variance 

library(tidyverse)
library(Matrix)

# ============================================================================
# ANALYTICAL KELLY (LONG-ONLY)
# ============================================================================

optimize_kelly_long_only <- function(returns_matrix, risk_free_rate = 0.01, kelly_fraction = 1.0) { # Default to Full - Kelly Constrained on Long-Only

  # Validate kelly_fraction
  if (kelly_fraction <= 0 || kelly_fraction > 2) {
    stop("kelly_fraction must be between 0 and 2")
  }
  
  # Remove date column and convert to matrix
  ret_mat <- returns_matrix %>%
    select(-DATE) %>%
    as.matrix()
  
  # Remove rows with any NA
  complete_rows <- complete.cases(ret_mat)
  ret_mat <- ret_mat[complete_rows, , drop = FALSE]
  
  if (nrow(ret_mat) < 20) {
    stop("Insufficient data: need at least 20 complete observations")
  }
  
  message(sprintf("Kelly (Long-only, f=%.2f): %d observations, %d assets", 
                  kelly_fraction, nrow(ret_mat), ncol(ret_mat)))
  
  # Calculate statistics
  mean_returns <- colMeans(ret_mat, na.rm = TRUE)
  cov_matrix <- cov(ret_mat, use = "complete.obs")
  n_assets <- ncol(ret_mat)
  
  # Detect frequency and calculate risk-free rate per period
  n_obs <- nrow(ret_mat)
  date_range <- as.numeric(diff(range(returns_matrix$DATE)))
  periods_per_year <- n_obs / (date_range / 365.25)
  r_period <- risk_free_rate / periods_per_year
  
  message(sprintf("  Detected frequency: %.0f periods per year", periods_per_year))
  
  
  # Regularize covariance matrix if needed
  eigenvalues <- eigen(cov_matrix, only.values = TRUE)$values
  min_eigenvalue <- min(eigenvalues)
  
  if (min_eigenvalue < 1e-8) {
    ridge <- max(1e-6, abs(min_eigenvalue) * 2)
    cov_matrix <- cov_matrix + diag(ridge, ncol(cov_matrix))
  }
  
  # Start with unconstrained solution: F* = Σ^(-1) * (μ - r)
  cov_inv <- solve(cov_matrix)
  excess_returns <- mean_returns - r_period
  weights <- as.vector(cov_inv %*% excess_returns)
  names(weights) <- colnames(ret_mat)
  
  # Iteratively remove negative weights
  max_iterations <- 50
  iteration <- 0
  
  while(any(weights < 0) && iteration < max_iterations) {
    iteration <- iteration + 1
    
    # Keep only positive assets
    positive_assets <- weights > 0
    
    if (sum(positive_assets) == 0) {
      warning("All weights became negative. Using equal weights.")
      weights <- rep(1/length(weights), length(weights))
      break
    }
    
    # Subset data
    mean_ret_subset <- mean_returns[positive_assets]
    cov_subset <- cov_matrix[positive_assets, positive_assets, drop = FALSE]
    
    # Recalculate
    if (length(mean_ret_subset) == 1) {
      weights_subset <- 1
    } else {
      cov_inv_subset <- solve(cov_subset)
      excess_ret_subset <- mean_ret_subset - r_period
      weights_subset <- as.vector(cov_inv_subset %*% excess_ret_subset)
    }
    
    # Create full weight vector
    weights_new <- rep(0, length(weights))
    weights_new[positive_assets] <- weights_subset
    
    if (all(weights_new >= 0)) {
      weights <- weights_new
      break
    }
    
    weights <- weights_new
  }
  
  # Normalize to sum to 1 (Full Kelly constraint)
  if (sum(weights) > 0) {
    weights <- weights / sum(weights)
  } else {
    weights <- rep(1/length(weights), length(weights))
  }
  
  # APPLY FRACTIONAL KELLY
  # Scale weights by kelly_fraction, remainder goes to risk-free
  weights_risky <- weights * kelly_fraction
  weight_risk_free <- 1 - kelly_fraction
  
  names(weights_risky) <- colnames(ret_mat)

   # Calculate portfolio statistics
  port_return_risky <- sum(weights * mean_returns)
  port_return_total <- kelly_fraction * port_return_risky + weight_risk_free * r_period
  
  port_variance_full <- t(weights) %*% cov_matrix %*% weights
  port_variance_fractional <- (kelly_fraction^2) * port_variance_full
  port_sd <- sqrt(as.numeric(port_variance_fractional))
  
  # ANNUALIZED METRICS
  annualized_return = port_return_total * periods_per_year
  annualized_volatility = port_sd * sqrt(periods_per_year)
  annualized_sharpe = (annualized_return - risk_free_rate) / annualized_volatility
  
  # Also calculate daily Sharpe for comparison
  daily_sharpe = (port_return_total - r_period) / port_sd
  
  message(sprintf("  Converged after %d iterations", iteration))
  message(sprintf("  Non-zero risky positions: %d", sum(weights_risky > 0.001)))
  message(sprintf("  Allocation to risky assets: %.1f%%", kelly_fraction * 100))
  message(sprintf("  Allocation to risk-free: %.1f%%", weight_risk_free * 100))
  message(sprintf("  Expected return (daily): %.4f%%, (annual): %.2f%%", 
                  port_return_total * 100, annualized_return * 100))
  message(sprintf("  Volatility (daily): %.4f%%, (annual): %.2f%%", 
                  port_sd * 100, annualized_volatility * 100))
  message(sprintf("  Sharpe ratio (daily): %.4f, (annual): %.2f", 
                  daily_sharpe, annualized_sharpe))
  
  list(
    weights = weights_risky,
    weights_full_kelly = weights,
    weight_risk_free = weight_risk_free,
    kelly_fraction = kelly_fraction,
    weights_df = tibble(
      ticker = names(weights_risky),
      weight = weights_risky,
      weight_full_kelly = weights
    ) %>% 
      filter(weight > 0.0001) %>% 
      arrange(desc(weight)),
    expected_return = port_return_total,          # Daily
    volatility = port_sd,                         # Daily
    annualized_return = annualized_return,        
    annualized_volatility = annualized_volatility, 
    sharpe_ratio = annualized_sharpe,             # Use annualized
    sharpe_ratio_daily = daily_sharpe,            # Keep daily for reference
    iterations = iteration,
    n_assets = sum(weights_risky > 0.001),
    covariance_matrix = cov_matrix,
    mean_returns = mean_returns,
    risk_free_rate = r_period,
    periods_per_year = periods_per_year           
  )
}
  

# ============================================================================
# MINIMUM VARIANCE (Markovitz)
# ============================================================================

optimize_min_variance <- function(returns_matrix) {
  
  ret_mat <- returns_matrix %>%
    select(-DATE) %>%
    as.matrix()

  n_obs <- nrow(ret_mat)
  date_range <- as.numeric(diff(range(returns_matrix$DATE)))
  periods_per_year <- n_obs / (date_range / 365.25)
  
  complete_rows <- complete.cases(ret_mat)
  ret_mat <- ret_mat[complete_rows, , drop = FALSE]
  
  if (nrow(ret_mat) < 20) {
    stop("Insufficient data: need at least 20 complete observations")
  }
  
  message(sprintf("Min-Variance: %d observations, %d assets", 
                  nrow(ret_mat), ncol(ret_mat)))
  
  mean_returns <- colMeans(ret_mat, na.rm = TRUE)
  cov_matrix <- cov(ret_mat, use = "complete.obs")
  
  # Regularize
  eigenvalues <- eigen(cov_matrix, only.values = TRUE)$values
  min_eigenvalue <- min(eigenvalues)
  
  if (min_eigenvalue < 1e-8) {
    ridge <- max(1e-6, abs(min_eigenvalue) * 2)
    cov_matrix <- cov_matrix + diag(ridge, ncol(cov_matrix))
  }
  
  n_assets <- ncol(ret_mat)
  
  # Analytical solution: w = Σ^(-1) * 1 / (1' Σ^(-1) 1)
  cov_inv <- solve(cov_matrix)
  ones <- rep(1, n_assets)
  
  weights <- as.vector(cov_inv %*% ones) / sum(cov_inv %*% ones)
  weights <- pmax(weights, 0)  # No shorts
  weights <- weights / sum(weights)  # Renormalize
  
  names(weights) <- colnames(ret_mat)

  port_return <- sum(weights * mean_returns)
  port_variance <- t(weights) %*% cov_matrix %*% weights
  port_sd <- sqrt(as.numeric(port_variance))
  
  # ANNUALIZED METRICS
  annualized_return = port_return * periods_per_year
  annualized_volatility = port_sd * sqrt(periods_per_year)
  
  message(sprintf("  Non-zero positions: %d", sum(weights > 0.001)))
  message(sprintf("  Expected return (annual): %.2f%%", annualized_return * 100))
  message(sprintf("  Volatility (annual): %.2f%%", annualized_volatility * 100))
  
  list(
    weights = weights,
    weights_df = tibble(
      ticker = names(weights),
      weight = weights
    ) %>% 
      filter(weight > 0.0001) %>% 
      arrange(desc(weight)),
    expected_return = port_return,
    volatility = port_sd,
    annualized_return = annualized_return,
    annualized_volatility = annualized_volatility,
    n_assets = sum(weights > 0.001),
    covariance_matrix = cov_matrix,
    mean_returns = mean_returns,
    periods_per_year = periods_per_year
  )
}

# ============================================================================
# EQUAL WEIGHT PORTFOLIO
# ============================================================================

create_equal_weight_portfolio <- function(returns_matrix) {
  
  ret_mat <- returns_matrix %>%
    select(-DATE) %>%
    as.matrix()
  
  n_assets <- ncol(ret_mat)
  weights <- rep(1/n_assets, n_assets)
  names(weights) <- colnames(ret_mat)
  
  mean_returns <- colMeans(ret_mat, na.rm = TRUE)
  cov_matrix <- cov(ret_mat, use = "complete.obs")
  
  port_return <- sum(weights * mean_returns)
  port_variance <- t(weights) %*% cov_matrix %*% weights
  port_sd <- sqrt(as.numeric(port_variance))
  
  list(
    weights = weights,
    weights_df = tibble(
      ticker = names(weights),
      weight = weights
    ),
    expected_return = port_return,
    volatility = port_sd,
    n_assets = n_assets,
    covariance_matrix = cov_matrix,
    mean_returns = mean_returns
  )
}