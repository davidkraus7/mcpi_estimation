# 05_quintile_analysis.R -- Income quintile decomposition of the MCPI
#
# Splits the household data into income quintiles and computes
# quintile-specific MCPIs to compare inflation across income groups.

library(dplyr)

quintile_mcpi_analysis <- function(df, sectors, baseline_sector,
                                   avg_exp_shares, first_year, last_year,
                                   sectoral_cpi_dir, base_year) {

  # Define quintile groups
  quintiles <- list(
    top    = list(label = "top",    lower = 0.8, upper = 1.0),
    middle = list(label = "middle", lower = 0.4, upper = 0.6),
    bottom = list(label = "bottom", lower = 0.0, upper = 0.2)
  )

  mcpi_list <- list()

  for (q in quintiles) {

    lower_threshold <- quantile(df$fincbtxm, q$lower, na.rm = TRUE)
    upper_threshold <- quantile(df$fincbtxm, q$upper, na.rm = TRUE)

    # Subset households for this quintile
    if (q$lower == 0.0) {
      df_q <- df[df$fincbtxm <= upper_threshold, ]
    } else if (q$upper == 1.0) {
      df_q <- df[df$fincbtxm > lower_threshold, ]
    } else {
      df_q <- df[df$fincbtxm > lower_threshold & df$fincbtxm <= upper_threshold, ]
    }

    # Run the Hubmer/Schaab pipeline for this quintile
    q_iv_results <- relative_elasticities(df_q, sectors, baseline_sector)
    q_rel_elast  <- q_iv_results$estimates
    q_aggr_exp   <- aggregate_expenditures(df_q, sectors)
    q_ind_elast  <- individual_elasticities(df_q, sectors, baseline_sector,
                                            q_rel_elast, q_aggr_exp)
    q_mbs       <- mbs_hubmer(df_q, q_ind_elast, sectors, first_year, last_year)
    q_ambs      <- aggregate_marginal_budget_shares(q_mbs, q_aggr_exp, sectors)

    q_ambs_t_avg   <- ambs_time_averages(q_ambs, sectors)
    q_avg_bs_t_avg <- avg_bs_time_averages(avg_exp_shares, sectors)
    q_t_avg_bs     <- data.frame(
      sector   = sectors,
      avg_ambs = q_ambs_t_avg$avg_ambs,
      avg_bs   = q_avg_bs_t_avg$avg_bs,
      row.names = sectors
    )

    q_mcpi <- marginal_consumer_price_index(avg_exp_shares, q_t_avg_bs,
                                            first_year, last_year,
                                            sectoral_cpi_dir, base_year)

    mcpi_col <- paste0(q$label, "_mcpi")
    colnames(q_mcpi)[colnames(q_mcpi) == "mcpi"] <- mcpi_col

    mcpi_list[[q$label]] <- q_mcpi
  }

  # Merge all quintile MCPIs into a single data frame
  quintiles_df <- Reduce(function(x, y) merge(x, y, by = "Date", all.x = TRUE),
                         mcpi_list)

  return(quintiles_df)
}
