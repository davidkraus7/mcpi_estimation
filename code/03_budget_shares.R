# 03_budget_shares.R -- Marginal and average budget share computation
#
# Computes aggregate expenditures, household-level marginal budget shares
# (MBS), aggregate MBS (AMBS), and average expenditure shares. Also provides
# time-averaging helpers used for CPI/MCPI construction.

library(dplyr)

aggregate_expenditures <- function(df, sectors) {

  first_year <- min(df$year, na.rm = TRUE)
  last_year  <- max(df$year, na.rm = TRUE)
  n_years    <- last_year - first_year + 1

  result_list <- vector("list", n_years)

  for (year_period in first_year:last_year) {

    year_result <- data.frame(period = year_period)
    year_df <- df |> filter(year == year_period)

    tot_exp <- 0
    for (sector in sectors) {
      exp_col <- sector_expenditure_col(sector)
      aggr_exp <- sum(year_df[[exp_col]], na.rm = TRUE)
      year_result[[exp_col]] <- aggr_exp
      tot_exp <- tot_exp + aggr_exp
    }

    year_result$total_expenditure <- tot_exp

    for (sector in sectors) {
      exp_col   <- sector_expenditure_col(sector)
      share_col <- sector_share_col(sector)
      year_result[[share_col]] <- year_result[[exp_col]] / year_result$total_expenditure
    }

    result_list[[year_period - first_year + 1]] <- year_result
  }

  return(bind_rows(result_list))
}


mbs_hubmer <- function(df, ind_elasticities, sectors, first_year, last_year) {

  n_years <- last_year - first_year + 1
  result_list <- vector("list", n_years)

  for (year_period in first_year:last_year) {

    year_df <- df[df$year == year_period, ]

    for (sector in sectors) {
      exp_col   <- sector_expenditure_col(sector)
      elast_col <- sector_elasticity_col(sector)
      mbs_col   <- sector_mbs_col(sector)

      year_elast <- ind_elasticities |> filter(period == year_period)
      year_df[[mbs_col]] <- (year_df[[exp_col]] / year_df$totexp) * year_elast[[elast_col]]
    }

    result_list[[year_period - first_year + 1]] <- year_df
  }

  return(bind_rows(result_list))
}


aggregate_marginal_budget_shares <- function(df_mbs, aggr_expenditures, sectors) {

  first_year <- min(df_mbs$year, na.rm = TRUE)
  last_year  <- max(df_mbs$year, na.rm = TRUE)
  n_years    <- last_year - first_year + 1

  result_list <- vector("list", n_years)

  for (year_period in first_year:last_year) {

    df_ambs_year <- data.frame(year = year_period)

    df_mbs_year <- df_mbs |> filter(year == year_period)
    aggr_year   <- aggr_expenditures |> filter(period == year_period)

    aggr_totexp   <- aggr_year$total_expenditure
    aggr_finlwt21 <- sum(df_mbs_year$finlwt21, na.rm = TRUE)

    # Compute household weights: expenditure-weighted and survey-weighted
    df_mbs_year <- df_mbs_year |>
      mutate(weight = (totexp / aggr_totexp) * (finlwt21 / aggr_finlwt21))

    sum_weights <- sum(df_mbs_year$weight, na.rm = TRUE)

    for (sector in sectors) {
      mbs_col  <- sector_mbs_col(sector)
      ambs_col <- sector_ambs_col(sector)

      ambs <- sum((df_mbs_year[[mbs_col]] * df_mbs_year$weight) / sum_weights,
                  na.rm = TRUE)
      df_ambs_year[[ambs_col]] <- ambs
    }

    result_list[[year_period - first_year + 1]] <- df_ambs_year
  }

  return(bind_rows(result_list))
}


average_expenditure_shares <- function(df, sectors) {

  share_cols <- sector_share_col(sectors)

  avg_exp_shares <- df |>
    group_by(year) |>
    summarize(across(
      all_of(share_cols),
      ~ weighted.mean(.x, w = finlwt21, na.rm = TRUE)
    ))

  return(avg_exp_shares)
}


ambs_time_averages <- function(df_ambs, sectors) {

  data.frame(
    sector = sectors,
    avg_ambs = sapply(sectors, function(s) mean(df_ambs[[sector_ambs_col(s)]])),
    row.names = sectors
  )
}


avg_bs_time_averages <- function(avg_exp_shares, sectors) {

  data.frame(
    sector = sectors,
    avg_bs = sapply(sectors, function(s) mean(avg_exp_shares[[sector_share_col(s)]])),
    row.names = sectors
  )
}
