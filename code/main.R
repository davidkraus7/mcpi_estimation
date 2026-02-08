# main.R -- Run the full MCPI estimation pipeline
#
# Usage:
#   source("code/main.R")      (from the project root in RStudio)
#   Rscript code/main.R        (from the command line)

library(here)

# Load configuration and utilities
source(here("code", "config.R"))
source(here("code", "utils.R"))

# Load function definitions
source(here("code", "01_prepare_data.R"))
source(here("code", "02_estimate_elasticities.R"))
source(here("code", "03_budget_shares.R"))
source(here("code", "04_price_indices.R"))
source(here("code", "05_quintile_analysis.R"))
source(here("code", "06_summary_statistics.R"))
source(here("code", "07_plots.R"))
source(here("code", "08_robustness.R"))


# ---- Step 1: Prepare household-level data ----

message("Step 1: Preparing CEX data...")
df <- prepare_data(
  sectors               = SECTORS,
  sectors_ucc_2015_2020 = SECTORS_UCC_2015_2020,
  first_year            = FIRST_YEAR,
  last_year             = LAST_YEAR,
  cex_data_dir          = CEX_DATA_DIR,
  cex_stubs_zip         = CEX_STUBS_ZIP,
  min_age               = MIN_AGE,
  max_age               = MAX_AGE,
  min_income            = MIN_INCOME,
  income_trim           = INCOME_TRIM,
  exp_trim              = EXP_TRIM
)


# ---- Step 2: Estimate elasticities (IV) ----

message("Step 2: Estimating relative income elasticities (IV)...")
iv_results <- relative_elasticities(df, SECTORS, BASELINE_SECTOR)
rel_elasticities <- iv_results$estimates
rel_std_errors   <- iv_results$std_errors
iv_diagnostics   <- iv_results$diagnostics

message("Step 2b: Computing aggregate expenditures...")
aggr_expenditures <- aggregate_expenditures(df, SECTORS)

message("Step 2c: Recovering individual elasticities...")
ind_elasticities <- individual_elasticities(
  df, SECTORS, BASELINE_SECTOR,
  rel_elasticities, aggr_expenditures
)

message("Step 2d: Budget constraint check...")
bc_results <- BC_check(ind_elasticities, aggr_expenditures,
                       SECTORS, FIRST_YEAR, LAST_YEAR)


# ---- Step 3: Marginal budget shares ----

message("Step 3: Computing marginal budget shares...")
df_mbs <- mbs_hubmer(df, ind_elasticities, SECTORS, FIRST_YEAR, LAST_YEAR)

df_ambs <- aggregate_marginal_budget_shares(df_mbs, aggr_expenditures, SECTORS)

avg_exp_shares <- average_expenditure_shares(df, SECTORS)

ambs_t_avg   <- ambs_time_averages(df_ambs, SECTORS)
avg_bs_t_avg <- avg_bs_time_averages(avg_exp_shares, SECTORS)
t_avg_bs     <- data.frame(
  sector   = SECTORS,
  avg_ambs = ambs_t_avg$avg_ambs,
  avg_bs   = avg_bs_t_avg$avg_bs,
  row.names = SECTORS
)


# ---- Step 4: Price indices ----

message("Step 4: Constructing CPI and MCPI...")
sample_cpi <- consumer_price_index(
  avg_exp_shares, FIRST_YEAR, LAST_YEAR, CPI_SECTORAL_DIR, BASE_YEAR
)

official_cpi <- official_consumer_price_index(
  CPI_AGGREGATE, FIRST_YEAR, LAST_YEAR
)

# Normalise official CPI to 100 in base year
base_official <- official_cpi$official_CPI[
  official_cpi$Date == as.Date(paste0(BASE_YEAR, "-01-01"))
]
official_cpi$official_CPI <- (official_cpi$official_CPI / base_official) * 100

mcpi <- marginal_consumer_price_index(
  avg_exp_shares, t_avg_bs, FIRST_YEAR, LAST_YEAR, CPI_SECTORAL_DIR, BASE_YEAR
)


# ---- Step 5: Income quintile analysis ----

message("Step 5: Quintile-specific MCPI analysis...")
quintiles_df <- quintile_mcpi_analysis(
  df, SECTORS, BASELINE_SECTOR, avg_exp_shares,
  FIRST_YEAR, LAST_YEAR, CPI_SECTORAL_DIR, BASE_YEAR
)


# ---- Step 6: Summary statistics and elasticity tables ----

message("Step 6: Computing summary statistics...")
compute_summary_statistics(df, SECTORS, OUTPUT_TABLES_DIR)

message("Step 6b: Saving elasticity estimates and IV diagnostics...")
save_elasticity_table(
  rel_elasticities, rel_std_errors, ind_elasticities,
  iv_diagnostics, SECTORS, BASELINE_SECTOR, OUTPUT_TABLES_DIR
)


# ---- Step 7: Generate plots ----

message("Step 7: Generating plots...")
save_all_plots(
  sample_cpi     = sample_cpi,
  official_cpi   = official_cpi,
  mcpi           = mcpi,
  quintiles_df   = quintiles_df,
  t_avg_bs       = t_avg_bs,
  ind_elasticities = ind_elasticities,
  sectors        = SECTORS,
  output_dir     = OUTPUT_FIGURES_DIR
)


# ---- Step 8: Robustness checks ----

message("Step 8: Baseline sector sensitivity check...")
ALT_BASELINES <- c("APPAREL", "FOODHOME", "SHELTER")
baseline_sensitivity(df, SECTORS, aggr_expenditures,
                     ALT_BASELINES, OUTPUT_TABLES_DIR)

message("Pipeline complete. Outputs saved to output/.")
