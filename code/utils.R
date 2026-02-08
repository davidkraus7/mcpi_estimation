# utils.R -- Shared utility functions for column name construction
# These helpers centralise the naming convention used throughout the pipeline.

sanitize_sector_name <- function(sector) {

  gsub(" ", "_", sector)
}

sector_expenditure_col <- function(sector) {
  paste0(sanitize_sector_name(sector), "_expenditure")
}

sector_share_col <- function(sector) {
  paste0(sanitize_sector_name(sector), "_expenditure_share")
}

sector_mbs_col <- function(sector) {
  paste0(sanitize_sector_name(sector), "_mbs")
}

sector_ambs_col <- function(sector) {
  paste0(sanitize_sector_name(sector), "_ambs")
}

sector_elasticity_col <- function(sector) {
  paste0(sanitize_sector_name(sector), "_elasticity")
}

sector_rel_elasticity_col <- function(sector) {
  paste0(sanitize_sector_name(sector), "_relative_elasticity")
}

sector_rel_elasticity_se_col <- function(sector) {
  paste0(sanitize_sector_name(sector), "_relative_elasticity_se")
}
