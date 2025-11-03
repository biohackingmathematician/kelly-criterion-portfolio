# Simplified rolling window backtest for Kelly and Min-Variance

library(tidyverse)
library(lubridate)

# ============================================================================
# ROLLING WINDOW BACKTEST
# ============================================================================

rolling_backtest <- function(returns_long, 
                              window_months = 24,
                              risk_free_rate = 0.01) {
  
  message("\n=== Rolling Window Backtest ===")
  message(sprintf("Window: %d months", window_months))
  
  # Convert to wide format
  returns_wide <- returns_long %>%
    select(DATE, ticker, return) %>%
    pivot_wider(names_from = ticker, values_from = return) %>%
    arrange(DATE)
  
  dates <- returns_wide$DATE
  n_periods <- nrow(returns_wide)
  
  # Initialize storage
  kelly_weights_list <- list()
  minvar_weights_list <- list()
  
  start_idx <- window_months + 1
  
  message(sprintf("Backtesting from period %d to %d", start_idx, n_periods))
  
  # Rolling optimization
  for (i in start_idx:n_periods) {
    
    train_start <- i - window_months
    train_end <- i - 1
    train_data <- returns_wide[train_start:train_end, ]
    
    if (nrow(train_data) < window_months * 0.8) next
    
    # Optimize Kelly
    kelly_port <- tryCatch({
      optimize_kelly_long_only(train_data, risk_free_rate)
    }, error = function(e) {
      message(sprintf("Period %d: Kelly failed - %s", i, e$message))
      NULL
    })
    
    # Optimize Min-Variance
    minvar_port <- tryCatch({
      optimize_min_variance(train_data)
    }, error = function(e) {
      message(sprintf("Period %d: MinVar failed - %s", i, e$message))
      NULL
    })
    
    # Store weights
    if (!is.null(kelly_port)) {
      kelly_weights_list[[i]] <- tibble(
        date = dates[i],
        ticker = names(kelly_port$weights),
        weight = kelly_port$weights
      )
    }
    
    if (!is.null(minvar_port)) {
      minvar_weights_list[[i]] <- tibble(
        date = dates[i],
        ticker = names(minvar_port$weights),
        weight = minvar_port$weights
      )
    }
    
    if (i %% 12 == 0) {
      message(sprintf("  Progress: %d/%d periods", i - start_idx + 1, n_periods - start_idx + 1))
    }
  }
  
  # Combine weights
  kelly_weights <- bind_rows(kelly_weights_list)
  minvar_weights <- bind_rows(minvar_weights_list)
  
  # Equal weight portfolio
  tickers <- returns_wide %>% select(-DATE) %>% colnames()
  equal_weights <- expand_grid(
    date = dates[start_idx:n_periods],
    ticker = tickers
  ) %>%
    mutate(weight = 1/length(tickers))
  
  # Calculate returns
  kelly_returns <- calculate_portfolio_returns(returns_long, kelly_weights, "Kelly")
  minvar_returns <- calculate_portfolio_returns(returns_long, minvar_weights, "MinVariance")
  equal_returns <- calculate_portfolio_returns(returns_long, equal_weights, "EqualWeight")
  
  all_returns <- bind_rows(kelly_returns, minvar_returns, equal_returns)
  
  message("Backtest complete!")
  
  list(
    returns = all_returns,
    kelly_weights = kelly_weights,
    minvar_weights = minvar_weights,
    equal_weights = equal_weights
  )
}

# ============================================================================
# CALCULATE PORTFOLIO RETURNS
# ============================================================================

calculate_portfolio_returns <- function(returns_long, weights, portfolio_name) {
  
  portfolio_returns <- returns_long %>%
    select(DATE, ticker, return) %>%
    inner_join(weights, by = c("DATE" = "date", "ticker" = "ticker")) %>%
    group_by(DATE) %>%
    summarise(
      portfolio_return = sum(return * weight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(portfolio = portfolio_name)
  
  return(portfolio_returns)
}

# ============================================================================
# PERFORMANCE METRICS
# ============================================================================

calculate_performance_metrics <- function(returns_df, risk_free_rate = 0.01) {
  
  # Detect frequency
  dates <- returns_df %>% pull(DATE) %>% unique() %>% sort()
  avg_days_between <- mean(diff(as.numeric(dates)))
  
  if (avg_days_between < 2) {
    periods_per_year <- 252  # Daily
  } else if (avg_days_between < 10) {
    periods_per_year <- 52   # Weekly
  } else {
    periods_per_year <- 12   # Monthly
  }
  
  r_period <- risk_free_rate / periods_per_year
  
  metrics <- returns_df %>%
    group_by(portfolio) %>%
    summarise(
      # Returns
      mean_return = mean(portfolio_return, na.rm = TRUE),
      total_return = prod(1 + portfolio_return) - 1,
      cagr = (prod(1 + portfolio_return)^(periods_per_year/n()) - 1),
      
      # Risk
      volatility = sd(portfolio_return, na.rm = TRUE),
      annualized_vol = volatility * sqrt(periods_per_year),
      downside_deviation = sqrt(mean(pmin(portfolio_return - r_period, 0)^2)),
      
      # Risk-adjusted
      sharpe_ratio = (mean_return - r_period) / volatility,
      sortino_ratio = (mean_return - r_period) / downside_deviation,
      
      # Wealth
      final_wealth = prod(1 + portfolio_return),
      
      n_periods = n(),
      .groups = "drop"
    )
  
  return(metrics)
}

# ============================================================================
# DRAWDOWNS
# ============================================================================

calculate_drawdowns <- function(returns_df) {
  
  drawdowns <- returns_df %>%
    group_by(portfolio) %>%
    arrange(DATE) %>%
    mutate(
      cumulative_wealth = cumprod(1 + portfolio_return),
      running_max = cummax(cumulative_wealth),
      drawdown = (cumulative_wealth - running_max) / running_max
    ) %>%
    ungroup()
  
  max_drawdowns <- drawdowns %>%
    group_by(portfolio) %>%
    summarise(
      max_drawdown = min(drawdown, na.rm = TRUE),
      avg_drawdown = mean(drawdown[drawdown < 0], na.rm = TRUE),
      .groups = "drop"
    )
  
  list(
    drawdowns = drawdowns,
    max_drawdowns = max_drawdowns
  )
}

# ============================================================================
# INFORMATION RATIO
# ============================================================================

calculate_information_ratio <- function(returns_df, benchmark = "EqualWeight") {
  
  benchmark_returns <- returns_df %>%
    filter(portfolio == benchmark) %>%
    select(DATE, benchmark_return = portfolio_return)
  
  ir <- returns_df %>%
    filter(portfolio != benchmark) %>%
    left_join(benchmark_returns, by = "DATE") %>%
    mutate(active_return = portfolio_return - benchmark_return) %>%
    group_by(portfolio) %>%
    summarise(
      mean_active_return = mean(active_return, na.rm = TRUE),
      tracking_error = sd(active_return, na.rm = TRUE),
      information_ratio = mean_active_return / tracking_error,
      .groups = "drop"
    )
  
  return(ir)
}