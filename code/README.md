# Code

## Scripts

| File | Purpose |
|------|---------|
| `config.R` | All configuration parameters (sectors, years, paths, thresholds) |
| `main.R` | Orchestrates the full pipeline; source this to run everything |
| `utils.R` | Shared functions for column name construction |
| `01_prepare_data.R` | Loads CEX data, filters households, computes expenditure shares |
| `02_estimate_elasticities.R` | IV estimation of relative and individual income elasticities |
| `03_budget_shares.R` | Computes marginal and aggregate marginal budget shares |
| `04_price_indices.R` | Constructs sample CPI, official CPI, and MCPI |
| `05_quintile_analysis.R` | Income quintile decomposition of the MCPI |
| `06_summary_statistics.R` | Descriptive statistics, elasticity tables with SEs and IV diagnostics |
| `07_plots.R` | All plotting code (PDF output) |
| `08_robustness.R` | Robustness checks (baseline sector sensitivity) |

## Conventions

- No script executes code at the top level; all define functions only.
- Only `main.R` sources other scripts.
- All file paths use `here::here()` via parameters defined in `config.R`.
- No global mutable state; all functions receive inputs as parameters.
