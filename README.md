# Marginal Consumer Price Index (MCPI) Estimation

Independent research project completed during second year of undergrad under the supervision of a faculty advisor. Estimates the Marginal Consumer Price Index using household-level expenditure data from the Bureau of Labor Statistics Consumer Expenditure Surveys (CEX) and sectoral CPI data from FRED (2014--2023).

The MCPI weights sectoral price changes by *marginal* budget shares -- how an additional dollar of income is allocated across consumption categories -- rather than *average* budget shares used in the standard CPI. This captures how inflation disproportionately affects households at different income levels.

## Project Structure

```
mcpi_estimation/
├── code/         R scripts (see code/README.md)
├── data/         Raw and processed data (see data/README.md)
├── docs/         Methodology documentation (LaTeX)
└── output/       Figures (PDF) and tables (CSV)
```

## Quick Start

1. **Install R** (>= 4.2.0) and optionally RStudio.
2. **Open** `mcpi_estimation.Rproj` in RStudio.
3. **Restore packages**: run `renv::restore()` to install all dependencies
   from the lockfile.
4. **Place data** in `data/raw/` -- see `data/README.md` for download
   instructions.
5. **Run the analysis**: `source("code/main.R")`.
6. **View outputs** in `output/figures/` (PDF plots) and `output/tables/`
   (CSV tables).

## Dependencies

Managed via `renv`. Key packages:

- `cepumd` -- CEX Public Use Microdata processing
- `ivreg` -- Instrumental variables regression
- `dplyr`, `tidyr` -- Data manipulation
- `ggplot2`, `scales` -- Plotting
- `here` -- Project-relative file paths