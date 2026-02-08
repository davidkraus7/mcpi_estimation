# 01_prepare_data.R -- Load and clean CEX household-level expenditure data
#
# Reads Consumer Expenditure Survey interview data from zip archives,
# computes sector expenditures and expenditure shares per household,
# applies sample restrictions, and returns a clean data frame.

library(cepumd)
library(dplyr)

prepare_data <- function(sectors, sectors_ucc_2015_2020, first_year, last_year,
                         cex_data_dir, cex_stubs_zip,
                         min_age, max_age, min_income,
                         income_trim, exp_trim) {

  yearly_list <- vector("list", last_year - first_year + 1)

  for (year in first_year:last_year) {

    message("Started constructing data for ", year)

    # Use alternate UCC codes for 2015-2020 to match hierarchy groupings
    ucc_sectors <- if (year %in% 2015:2020) sectors_ucc_2015_2020 else sectors

    # Parse hierarchy groupings for the year
    hg_df <- ce_hg(year,
                   interview,
                   hg_zip_path = cex_stubs_zip)

    # Build data for the first sector (establishes the base data frame)
    sector <- sectors[1]
    ucc_sector <- ucc_sectors[1]

    message("Constructing for ", sector)

    uccs <- ce_uccs(hg_df,
                    expenditure = NULL,
                    ucc_group = ucc_sector,
                    uccs_only = TRUE)

    file1 <- paste0("intrvw", substr(as.character(year - 1), 3, 4), ".zip")
    file2 <- paste0("intrvw", substr(as.character(year), 3, 4), ".zip")

    data_df <- ce_prepdata(year,
                           interview,
                           hg_df,
                           uccs,
                           int_zp = c(
                             file.path(cex_data_dir, file1),
                             file.path(cex_data_dir, file2)),
                           fincbtxm,
                           age_ref,
                           fam_size,
                           as_comp3,
                           as_comp4,
                           as_comp5,
                           bls_urbn,
                           region)

    # Create num_kids variable from child-age composition variables
    data_df$as_comp3 <- as.numeric(data_df$as_comp3)
    data_df$as_comp4 <- as.numeric(data_df$as_comp4)
    data_df$as_comp5 <- as.numeric(data_df$as_comp5)

    data_df$num_kids <- rowSums(data_df[, c("as_comp3", "as_comp4", "as_comp5")], na.rm = TRUE)

    data_df <- data_df |>
      select(-as_comp3, -as_comp4, -as_comp5)

    # Compute total expenditure on the first sector per household
    exp_col <- sector_expenditure_col(sector)
    exp_col_names <- c(exp_col)

    summarized_data_df <- data_df |>
      group_by(newid) |>
      summarize(first_year = if_else(all(is.na(ref_yr)), Inf, min(ref_yr, na.rm = TRUE)),
                first_month = if_else(all(is.na(ref_mo)), 0, min(ref_mo, na.rm = TRUE)),
                last_year = if_else(all(is.na(ref_yr)), -Inf, max(ref_yr, na.rm = TRUE)),
                last_month = if_else(all(is.na(ref_mo)), 0, max(ref_mo, na.rm = TRUE)),
                !!exp_col := sum(cost, na.rm = TRUE),
                .groups = "drop")

    yearly_data_df <- data_df |>
      select(-ucc, -cost, -ref_mo, -ref_yr) |>
      distinct(newid, .keep_all = TRUE) |>
      left_join(summarized_data_df, by = "newid")

    rm(data_df, summarized_data_df)

    # Loop through remaining sectors
    for (i in 2:length(sectors)) {

      sector <- sectors[i]
      ucc_sector <- ucc_sectors[i]

      message("Constructing for ", sector)

      uccs <- ce_uccs(hg_df,
                      expenditure = NULL,
                      ucc_group = ucc_sector,
                      uccs_only = TRUE)

      data_df <- ce_prepdata(year,
                             interview,
                             hg_df,
                             uccs,
                             int_zp = c(
                               file.path(cex_data_dir, file1),
                               file.path(cex_data_dir, file2)))

      exp_col <- sector_expenditure_col(sector)
      exp_col_names <- append(exp_col_names, exp_col)

      summarized_data_df <- data_df |>
        group_by(newid) |>
        summarize(!!exp_col := sum(cost, na.rm = TRUE),
                  .groups = "drop")

      yearly_data_df <- yearly_data_df |>
        left_join(summarized_data_df, by = "newid")

      rm(data_df, summarized_data_df)
    }

    yearly_list[[year - first_year + 1]] <- yearly_data_df
    rm(yearly_data_df)
  }

  final_data_df <- bind_rows(yearly_list)
  rm(yearly_list)

  # Consolidate households whose expenditures span two years
  summarized_data_df <- final_data_df |>
    group_by(newid) |>
    summarize(
      mo_scope = sum(mo_scope, na.rm = TRUE),
      first_year = min(first_year, na.rm = TRUE),
      first_month = min(first_month[first_year == min(first_year)]),
      last_year = max(last_year, na.rm = TRUE),
      last_month = max(last_month[last_year == max(last_year)]),
      across(all_of(exp_col_names), \(x) sum(x, na.rm = TRUE))
    )

  final_data_df <- final_data_df |>
    select(-mo_scope, -first_year, -first_month, -last_year, -last_month,
           -all_of(exp_col_names)) |>
    distinct(newid, .keep_all = TRUE) |>
    left_join(summarized_data_df, by = "newid")

  rm(summarized_data_df)

  # Convert types
  final_data_df$bls_urbn  <- as.numeric(final_data_df$bls_urbn)
  final_data_df$age_ref   <- as.numeric(final_data_df$age_ref)
  final_data_df$fincbtxm  <- as.numeric(final_data_df$fincbtxm)
  final_data_df$fam_size  <- as.numeric(final_data_df$fam_size)
  final_data_df$region    <- as.numeric(final_data_df$region)

  # Apply sample restrictions
  final_data_df <- final_data_df |>
    filter(mo_scope == 3) |>
    filter(!first_month == 0 & !last_month == 0 &
           !first_year == Inf & !last_year == -Inf) |>
    filter(bls_urbn == 1) |>
    filter(age_ref >= min_age & age_ref <= max_age)

  # Compute total expenditure and expenditure shares
  final_data_df <- final_data_df |>
    rowwise() |>
    mutate(totexp = sum(c_across(all_of(exp_col_names)), na.rm = TRUE)) |>
    ungroup() |>
    mutate(across(all_of(exp_col_names),
                  .fns = list(share = ~ . / totexp),
                  .names = "{col}_share")) |>
    mutate(across(ends_with("_share"), ~ ifelse(. == 0, 0.00001, .)))

  # Income and expenditure trimming
  final_data_df <- final_data_df |>
    filter(fincbtxm >= min_income & totexp > 0)

  inc_low  <- quantile(final_data_df$fincbtxm, income_trim[1], na.rm = TRUE)
  inc_high <- quantile(final_data_df$fincbtxm, income_trim[2], na.rm = TRUE)
  exp_low  <- quantile(final_data_df$totexp, exp_trim[1], na.rm = TRUE)
  exp_high <- quantile(final_data_df$totexp, exp_trim[2], na.rm = TRUE)

  final_data_df <- final_data_df |>
    filter(fincbtxm > inc_low & fincbtxm < inc_high &
           totexp > exp_low & totexp < exp_high)

  # Construct year and quarter variables from last_year and last_month
  final_data_df <- final_data_df |>
    mutate(year = case_when(
      last_month == 1 ~ last_year - 1,
      last_month >= 2 & last_month <= 12 ~ last_year,
      TRUE ~ NA_real_
    )) |>
    mutate(quarter = case_when(
      last_month %in% c(1, 11, 12) ~ 4,
      last_month %in% c(10, 9, 8) ~ 3,
      last_month %in% c(7, 6, 5) ~ 2,
      last_month %in% c(4, 3, 2) ~ 1,
      TRUE ~ NA_integer_
    ))

  return(final_data_df)
}
