# 02_estimate_elasticities.R -- Income elasticity estimation
#
# Estimates relative income elasticities via IV regression (Schaab & Tan),
# instrumenting log total expenditure with log post-tax income to address
# measurement error. Then recovers individual elasticities from the budget
# constraint.

library(dplyr)
library(ivreg)

relative_elasticities <- function(df, sectors, baseline_sector) {

  first_year <- min(df$year, na.rm = TRUE)
  last_year  <- max(df$year, na.rm = TRUE)
  n_years    <- last_year - first_year + 1

  est_list  <- vector("list", n_years)
  se_list   <- vector("list", n_years)
  diag_list <- vector("list", n_years)

  for (year_period in first_year:last_year) {

    est_row  <- data.frame(period = year_period)
    se_row   <- data.frame(period = year_period)
    diag_row <- data.frame(period = year_period)

    baseline_share_col <- sector_share_col(baseline_sector)

    year_df <- df |> filter(year == year_period)

    for (sector in sectors) {

      rel_elast_col <- sector_rel_elasticity_col(sector)
      se_col        <- sector_rel_elasticity_se_col(sector)

      if (sector == baseline_sector) {
        est_row[[rel_elast_col]] <- 0
        se_row[[se_col]]         <- 0
        next
      }

      share_col <- sector_share_col(sector)

      temp_df <- year_df |>
        mutate(log_ratio = log(.data[[share_col]] / .data[[baseline_share_col]]))

      # IV regression: instrument log(totexp) with log(fincbtxm)
      model <- ivreg(
        log_ratio ~ log(totexp) + num_kids + age_ref + fam_size |
                     log(fincbtxm) + num_kids + age_ref + fam_size,
        data = temp_df
      )

      model_summary <- summary(model, diagnostics = TRUE)

      est_row[[rel_elast_col]] <-
        model_summary$coefficients["log(totexp)", "Estimate"]
      se_row[[se_col]] <-
        model_summary$coefficients["log(totexp)", "Std. Error"]

      # First-stage F-statistic (Weak instruments test)
      f_col <- paste0(sanitize_sector_name(sector), "_first_stage_F")
      diag_row[[f_col]] <- model_summary$diagnostics["Weak instruments", "statistic"]
    }

    idx <- year_period - first_year + 1
    est_list[[idx]]  <- est_row
    se_list[[idx]]   <- se_row
    diag_list[[idx]] <- diag_row
  }

  list(
    estimates   = bind_rows(est_list),
    std_errors  = bind_rows(se_list),
    diagnostics = bind_rows(diag_list)
  )
}


individual_elasticities <- function(df, sectors, baseline_sector,
                                    rel_elasticities, aggr_expenditures) {

  first_year <- min(df$year, na.rm = TRUE)
  last_year  <- max(df$year, na.rm = TRUE)
  n_years    <- last_year - first_year + 1

  result_list <- vector("list", n_years)

  for (year_period in first_year:last_year) {

    year_elasticities <- data.frame(period = year_period)

    year_rel <- rel_elasticities |> filter(period == year_period)
    year_aggr <- aggr_expenditures |> filter(period == year_period)

    # Compute weighted sum of relative elasticities
    weighted_sum <- 0
    for (sector in sectors) {
      rel_col  <- sector_rel_elasticity_col(sector)
      share_col <- sector_share_col(sector)
      weighted_sum <- weighted_sum + year_aggr[[share_col]] * year_rel[[rel_col]]
    }

    # Recover baseline elasticity from budget constraint
    baseline_elast_col <- sector_elasticity_col(baseline_sector)
    year_elasticities[[baseline_elast_col]] <- 1 - weighted_sum

    # Recover individual elasticities for all sectors
    for (sector in sectors) {
      if (sector == baseline_sector) next
      elast_col <- sector_elasticity_col(sector)
      rel_col <- sector_rel_elasticity_col(sector)
      year_elasticities[[elast_col]] <- year_rel[[rel_col]] + year_elasticities[[baseline_elast_col]]
    }

    result_list[[year_period - first_year + 1]] <- year_elasticities
  }

  return(bind_rows(result_list))
}


BC_check <- function(ind_elasticities, aggr_expenditures, sectors,
                     first_year, last_year) {

  n_years <- last_year - first_year + 1
  result_list <- vector("list", n_years)

  for (year_period in first_year:last_year) {
    year_elast <- ind_elasticities |> filter(period == year_period)
    year_aggr  <- aggr_expenditures |> filter(period == year_period)

    bc_sum <- 0
    for (sector in sectors) {
      elast_col <- sector_elasticity_col(sector)
      share_col <- sector_share_col(sector)
      bc_sum <- bc_sum + year_aggr[[share_col]] * year_elast[[elast_col]]
    }

    result_list[[year_period - first_year + 1]] <-
      data.frame(year = year_period, bc_sum = bc_sum)
    message("BC check ", year_period, ": ", round(bc_sum, 6))
  }

  return(bind_rows(result_list))
}
