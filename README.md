# kelly-criterion-portfolio
Replication of Carta &amp; Conversano (2020) Kelly Criterion study
# Kelly Criterion Portfolio Optimization

Replication and extension of Carta & Conversano (2020): "Practical Implementation of the Kelly Criterion: Optimal Growth Rate, Number of Trades, and Rebalancing Frequency for Equity Portfolios"

## Team Members
Aniqa Nayim, Agna Chan, Pranav Kasibhatla, Syed Bashir Hydari

## Project Overview

This project replicates the empirical study by Carta & Conversano (2020) that tests the Kelly Criterion for portfolio optimization using Euro Stoxx 50 data (2007-2018). We compare Kelly portfolios with traditional mean-variance portfolios across different:

- **Rebalancing frequencies:** daily, weekly, monthly, quarterly
- **Portfolio sizes:** 5, 10, 15, 20 stocks
- **Performance metrics:** CAGR, Sharpe ratio, Max Drawdown, VaR₉₉, ES₉₉

### Extensions
We extend the original paper by implementing regime-dependent Kelly fractions using Kim's (2024) Kelly Criterion Extension (KCE) to add dynamic market condition awareness.

## Project Structure
```
kelly-criterion-portfolio/
├── data/
│   ├── raw/              # Original downloaded data
│   └── processed/        # Cleaned data ready for analysis
├── code/
│   ├── 00_setup/         # Installation and configuration
│   ├── 01_data/          # Data collection scripts
│   ├── 02_optimization/  # Portfolio optimization
│   ├── 03_analysis/      # Performance analysis
│   └── 04_visualization/ # Plotting functions
├── output/
│   ├── figures/          # Generated plots
│   ├── tables/           # Results tables
│   └── reports/          # Final documents
└── docs/                 # Documentation and reports
```

## Setup Instructions

### Prerequisites
- R (version 4.0 or higher)
- RStudio or Positron
- Git

### 1. Clone Repository
```bash
git clone https://github.com/your-username/kelly-criterion-portfolio.git
cd kelly-criterion-portfolio
```

### 2. Install Required R Packages
Open R or Positron and run:
```r
source("code/00_setup/install_packages.R")
```

This will install all required packages including:
- `quantmod` - Financial data download
- `tidyverse` - Data manipulation
- `xts`, `zoo` - Time series
- `quadprog` - Quadratic programming for Kelly optimization
- `PerformanceAnalytics` - Portfolio metrics

### 3. Download Data
```r
source("code/01_data/01_download_data.R")
```

This downloads Euro Stoxx 50 daily prices from Yahoo Finance (2005-2018).

## Data

### Source
- **Index:** Euro Stoxx 50
- **Provider:** Yahoo Finance (via quantmod)
- **Period:** January 2007 - December 2018 (with 2005-2006 buffer for rolling estimation)
- **Frequency:** Daily adjusted close prices

### Tickers
Approximately 35 major constituents including:
- **Germany:** SAP, Siemens, Allianz, BASF, Daimler
- **France:** LVMH, L'Oréal, TotalEnergies, BNP Paribas, Airbus
- **Netherlands:** ASML, Philips, ING Group
- **Spain:** Santander, BBVA, Iberdrola, Inditex
- **Italy:** ENI, Intesa Sanpaolo, Enel

See `data/raw/stoxx50_tickers.csv` for complete list.

## Methodology

### Kelly Criterion
The Kelly Criterion maximizes the expected logarithmic growth rate of wealth:
```
maximize: μ'w - (1/2)w'Σw
subject to: Σw = 1, w ≥ 0
```

Where:
- `w` = portfolio weights
- `μ` = expected returns
- `Σ` = covariance matrix

### Estimation
- **Rolling window:** 24 months (≈504 trading days)
- **Rebalancing:** Monthly
- **Constraints:** Long-only, no leverage (weights sum to 1)

### Performance Metrics
- **CAGR:** Compound Annual Growth Rate
- **Sharpe Ratio:** Risk-adjusted returns
- **Max Drawdown:** Largest peak-to-trough decline
- **VaR₉₉:** Value at Risk (99% confidence)
- **ES₉₉:** Expected Shortfall (99% confidence)

## Timeline

| Week | Dates | Milestone |
|------|-------|-----------|
| 1 | Oct 28 - Nov 3 | Data Collection & Cleaning |
| 2 | Nov 4 - Nov 10 | Model Setup (Kelly & Mean-Variance) |
| 3 | Nov 11 - Nov 17 | Run Baseline Replication |
| 4 | Nov 18 - Nov 24 | Validation & Reconciliation |
| 5 | Nov 25 - Dec 1 | Develop Extensions |
| 6 | Dec 2 - Dec 8 | Analysis & Draft Report |
| 7 | Dec 9 - Dec 15 | Final Submission & Presentation |

## Repository Structure

### Code Files
- `00_setup/install_packages.R` - Package installation
- `01_data/01_download_data.R` - Download price data
- `01_data/02_clean_data.R` - Data quality checks and cleaning
- `01_data/03_calculate_returns.R` - Compute log returns
- `02_optimization/kelly_optimization.R` - Kelly portfolio optimization
- `02_optimization/mv_optimization.R` - Mean-variance optimization

### Output Files
- `output/figures/` - Performance plots, drawdown charts
- `output/tables/` - Summary statistics, performance metrics
- `output/reports/` - Final report and presentation

## References

Carta, A., & Conversano, C. (2020). Practical Implementation of the Kelly Criterion: Optimal Growth Rate, Number of Trades, and Rebalancing Frequency for Equity Portfolios. *Frontiers in Applied Mathematics and Statistics*, 6, 577050. https://doi.org/10.3389/fams.2020.577050

Kim, S. (2024). Kelly Criterion Extension. *Mathematics*, 12(11), 1725. https://doi.org/10.3390/math12111725

## License

This project is for academic purposes only.

## Contact

For questions, contact: [cc5314@columbia.edu]