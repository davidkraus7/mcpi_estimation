# 08_robustness.R -- Robustness checks
#
# Tests sensitivity of individual income elasticity estimates to the choice
# of baseline sector. The budget constraint identifies individual elasticities
# regardless of baseline, so results should be invariant.

library(dplyr)

baseline_sensitivity <- function(df, sectors, aggr_expenditures,
                                 alt_baselines, output_dir) {

  results <- list()

  for (baseline in alt_baselines) {

    message("  Robustness: baseline = ", baseline)

    iv_res    <- relative_elasticities(df, sectors, baseline)
    ind_elast <- individual_elasticities(df, sectors, baseline,
                                         iv_res$estimates, aggr_expenditures)

    # Time-averaged individual elasticities
    avg_elast <- sapply(sectors, function(s) {
      mean(ind_elast[[sector_elasticity_col(s)]], na.rm = TRUE)
    })

    results[[baseline]] <- data.frame(
      sector     = sectors,
      elasticity = round(avg_elast, 4),
      baseline   = baseline,
      stringsAsFactors = FALSE
    )
  }

  combined <- bind_rows(results)

  # Pivot to wide: one column per baseline
  wide <- tidyr::pivot_wider(
    combined,
    names_from  = baseline,
    values_from = elasticity,
    names_prefix = "baseline_"
  )

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  output_file <- file.path(output_dir, "robustness_baseline_sensitivity.csv")
  write.csv(wide, output_file, row.names = FALSE)
  message("Baseline sensitivity table saved to ", output_file)

  invisible(wide)
}
