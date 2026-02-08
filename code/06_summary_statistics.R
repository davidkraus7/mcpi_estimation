# 06_summary_statistics.R -- Descriptive statistics and elasticity tables
#
# Computes summary statistics for expenditures and shares, and formats
# the IV elasticity estimates with standard errors and diagnostics.

library(dplyr)
library(tidyr)

compute_summary_statistics <- function(df, sectors, output_dir) {

  expenditure_stats <- df |>
    select(ends_with("_expenditure")) |>
    summarise(across(everything(), list(
      mean = ~ mean(., na.rm = TRUE),
      min  = ~ min(., na.rm = TRUE),
      max  = ~ max(., na.rm = TRUE),
      sd   = ~ sd(., na.rm = TRUE)
    ), .names = "{.col}_{.fn}")) |>
    pivot_longer(cols = everything(),
                 names_to = "Variable_Statistic",
                 values_to = "value") |>
    separate(Variable_Statistic,
             into = c("Variable", "Statistic"),
             sep = "_(?=[^_]+$)") |>
    pivot_wider(names_from = Statistic, values_from = value)

  share_stats <- df |>
    select(ends_with("_expenditure_share")) |>
    summarise(across(everything(), list(
      mean = ~ mean(., na.rm = TRUE),
      min  = ~ min(., na.rm = TRUE),
      max  = ~ max(., na.rm = TRUE),
      sd   = ~ sd(., na.rm = TRUE)
    ), .names = "{.col}_{.fn}")) |>
    pivot_longer(cols = everything(),
                 names_to = "Variable_Statistic",
                 values_to = "value") |>
    separate(Variable_Statistic,
             into = c("Variable", "Statistic"),
             sep = "_(?=[^_]+$)") |>
    pivot_wider(names_from = Statistic, values_from = value)

  combined_stats <- bind_rows(expenditure_stats, share_stats)

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  output_file <- file.path(output_dir, "summary_statistics.csv")
  write.csv(combined_stats, output_file, row.names = FALSE)
  message("Summary statistics saved to ", output_file)

  return(combined_stats)
}


save_elasticity_table <- function(rel_elasticities, rel_std_errors,
                                  ind_elasticities, iv_diagnostics,
                                  sectors, baseline_sector, output_dir) {

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # --- Time-averaged relative elasticities with SEs ---
  non_baseline <- sectors[sectors != baseline_sector]

  elast_rows <- lapply(non_baseline, function(sector) {
    est_col <- sector_rel_elasticity_col(sector)
    se_col  <- sector_rel_elasticity_se_col(sector)
    f_col   <- paste0(sanitize_sector_name(sector), "_first_stage_F")

    avg_est <- mean(rel_elasticities[[est_col]], na.rm = TRUE)
    avg_se  <- mean(rel_std_errors[[se_col]], na.rm = TRUE)
    avg_f   <- mean(iv_diagnostics[[f_col]], na.rm = TRUE)

    data.frame(
      sector             = sector,
      rel_elasticity     = round(avg_est, 4),
      std_error          = round(avg_se, 4),
      first_stage_F      = round(avg_f, 1),
      stringsAsFactors   = FALSE
    )
  })

  elast_table <- bind_rows(elast_rows)

  output_file <- file.path(output_dir, "elasticity_estimates.csv")
  write.csv(elast_table, output_file, row.names = FALSE)
  message("Elasticity estimates saved to ", output_file)

  # --- Individual elasticities (time-averaged) ---
  ind_avg_rows <- lapply(sectors, function(sector) {
    elast_col <- sector_elasticity_col(sector)
    data.frame(
      sector          = sector,
      income_elasticity = round(mean(ind_elasticities[[elast_col]], na.rm = TRUE), 4),
      stringsAsFactors = FALSE
    )
  })

  ind_table <- bind_rows(ind_avg_rows)

  output_file <- file.path(output_dir, "individual_elasticities.csv")
  write.csv(ind_table, output_file, row.names = FALSE)
  message("Individual elasticities saved to ", output_file)

  # --- First-stage diagnostics by year ---
  output_file <- file.path(output_dir, "iv_diagnostics.csv")
  write.csv(iv_diagnostics, output_file, row.names = FALSE)
  message("IV diagnostics saved to ", output_file)

  invisible(list(relative = elast_table, individual = ind_table))
}
