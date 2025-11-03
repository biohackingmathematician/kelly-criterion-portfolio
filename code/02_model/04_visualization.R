# Visualization functions

library(tidyverse)
library(scales)
library(patchwork)

# ============================================================================
# CUMULATIVE RETURNS PLOT
# ============================================================================

plot_cumulative_returns <- function(returns_df, title = "Out-of-Sample Cumulative Returns") {
  
  cum_returns <- returns_df %>%
    group_by(portfolio) %>%
    arrange(DATE) %>%
    mutate(
      cumulative_wealth = cumprod(1 + portfolio_return)
    ) %>%
    ungroup()
  
  portfolio_colors <- c(
    "Kelly" = "#E74C3C",
    "MinVariance" = "#2ECC71",
    "EqualWeight" = "#34495E"
  )
  
  p <- ggplot(cum_returns, aes(x = DATE, y = cumulative_wealth, 
                                color = portfolio, linetype = portfolio)) +
    geom_line(linewidth = 1) +
    scale_color_manual(values = portfolio_colors) +
    scale_linetype_manual(values = c("Kelly" = "solid", 
                                      "MinVariance" = "dashed",
                                      "EqualWeight" = "dotted")) +
    scale_y_continuous(labels = number_format(accuracy = 0.1)) +
    labs(
      title = title,
      x = "Date",
      y = "Cumulative Wealth ($1 invested)",
      color = "Portfolio",
      linetype = "Portfolio"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  return(p)
}

# ============================================================================
# DRAWDOWN PLOT
# ============================================================================

plot_drawdowns <- function(drawdowns_df, title = "Portfolio Drawdowns") {
  
  portfolio_colors <- c(
    "Kelly" = "#E74C3C",
    "MinVariance" = "#2ECC71",
    "EqualWeight" = "#34495E"
  )
  
  p <- ggplot(drawdowns_df, aes(x = DATE, y = drawdown, 
                                 color = portfolio, fill = portfolio)) +
    geom_area(alpha = 0.3, position = "identity") +
    geom_line(linewidth = 0.8) +
    scale_color_manual(values = portfolio_colors) +
    scale_fill_manual(values = portfolio_colors) +
    scale_y_continuous(labels = percent_format()) +
    labs(
      title = title,
      x = "Date",
      y = "Drawdown",
      color = "Portfolio",
      fill = "Portfolio"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  return(p)
}

# ============================================================================
# PORTFOLIO WEIGHTS BAR CHART
# ============================================================================

plot_portfolio_weights <- function(weights_df, top_n = 10, 
                                   title = "Portfolio Composition") {
  
  top_weights <- weights_df %>%
    arrange(desc(weight)) %>%
    head(top_n) %>%
    mutate(ticker = fct_reorder(ticker, weight))
  
  p <- ggplot(top_weights, aes(x = weight, y = ticker, fill = weight)) +
    geom_col() +
    scale_fill_gradient(low = "#3498DB", high = "#E74C3C") +
    scale_x_continuous(labels = percent_format()) +
    labs(
      title = title,
      x = "Weight",
      y = "Asset",
      fill = "Weight"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
  
  return(p)
}

# ============================================================================
# PERFORMANCE COMPARISON TABLE
# ============================================================================

create_performance_table <- function(metrics_df, max_dd_df, ir_df = NULL) {
  
  combined <- metrics_df %>%
    left_join(max_dd_df, by = "portfolio") %>%
    mutate(
      Portfolio = portfolio,
      CAGR = sprintf("%.2f%%", cagr * 100),
      `Final Wealth` = sprintf("%.2f", final_wealth),
      `Max DD` = sprintf("%.2f%%", abs(max_drawdown) * 100),
      `Avg DD` = sprintf("%.2f%%", abs(avg_drawdown) * 100),
      Mean = sprintf("%.2f%%", mean_return * 100),
      `St. Dev.` = sprintf("%.2f%%", annualized_vol * 100),
      Sortino = sprintf("%.3f", sortino_ratio),
      Sharpe = sprintf("%.3f", sharpe_ratio)
    )
  
  if (!is.null(ir_df)) {
    combined <- combined %>%
      left_join(ir_df %>% select(portfolio, information_ratio), 
                by = "portfolio") %>%
      mutate(`Info Ratio` = sprintf("%.3f", information_ratio))
  }
  
  # Select display columns
  display_cols <- c("Portfolio", "CAGR", "Final Wealth", "Max DD", "Avg DD",
                    "Mean", "St. Dev.", "Sortino", "Sharpe")
  
  if (!is.null(ir_df)) {
    display_cols <- c(display_cols, "Info Ratio")
  }
  
  table_df <- combined %>%
    select(any_of(display_cols))
  
  return(table_df)
}

# ============================================================================
# RISK-RETURN SCATTER
# ============================================================================

plot_risk_return_scatter <- function(metrics_df, title = "Risk-Return Profile") {
  
  portfolio_colors <- c(
    "Kelly" = "#E74C3C",
    "MinVariance" = "#2ECC71",
    "EqualWeight" = "#34495E"
  )
  
  p <- ggplot(metrics_df, aes(x = annualized_vol, y = cagr, 
                               color = portfolio, shape = portfolio)) +
    geom_point(size = 5) +
    geom_text(aes(label = portfolio), vjust = -1, size = 3.5, show.legend = FALSE) +
    scale_color_manual(values = portfolio_colors) +
    scale_x_continuous(labels = percent_format()) +
    scale_y_continuous(labels = percent_format()) +
    labs(
      title = title,
      x = "Volatility (Annualized)",
      y = "CAGR",
      color = "Portfolio",
      shape = "Portfolio"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  return(p)
}

# ============================================================================
# ROLLING SHARPE RATIO
# ============================================================================

plot_rolling_sharpe <- function(returns_df, window = 12, risk_free_rate = 0.01) {
  
  # Detect frequency
  dates <- returns_df %>% pull(DATE) %>% unique() %>% sort()
  avg_days_between <- mean(diff(as.numeric(dates)))
  
  if (avg_days_between < 2) {
    periods_per_year <- 252
  } else if (avg_days_between < 10) {
    periods_per_year <- 52
  } else {
    periods_per_year <- 12
  }
  
  r_period <- risk_free_rate / periods_per_year
  
  rolling_sharpe <- returns_df %>%
    group_by(portfolio) %>%
    arrange(DATE) %>%
    mutate(
      rolling_mean = zoo::rollapply(portfolio_return, window, mean, 
                                     fill = NA, align = "right"),
      rolling_sd = zoo::rollapply(portfolio_return, window, sd, 
                                   fill = NA, align = "right"),
      rolling_sharpe = (rolling_mean - r_period) / rolling_sd
    ) %>%
    ungroup() %>%
    filter(!is.na(rolling_sharpe))
  
  portfolio_colors <- c(
    "Kelly" = "#E74C3C",
    "MinVariance" = "#2ECC71",
    "EqualWeight" = "#34495E"
  )
  
  p <- ggplot(rolling_sharpe, aes(x = DATE, y = rolling_sharpe, color = portfolio)) +
    geom_line(linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_color_manual(values = portfolio_colors) +
    labs(
      title = sprintf("Rolling Sharpe Ratio (%d-period window)", window),
      x = "Date",
      y = "Sharpe Ratio",
      color = "Portfolio"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5, face = "bold"),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  return(p)
}

# ============================================================================
# COMBINED DASHBOARD
# ============================================================================

create_performance_dashboard <- function(returns_df, drawdowns_df, metrics_df) {
  
  p1 <- plot_cumulative_returns(returns_df)
  p2 <- plot_drawdowns(drawdowns_df)
  p3 <- plot_risk_return_scatter(metrics_df)
  p4 <- plot_rolling_sharpe(returns_df, window = 12)
  
  dashboard <- (p1 | p3) / (p2 | p4)
  
  dashboard <- dashboard + 
    plot_annotation(
      title = "Portfolio Performance Dashboard",
      theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
    )
  
  return(dashboard)
}