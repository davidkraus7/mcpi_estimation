# config.R -- Project configuration
# All parameters governing the analysis are defined here.

library(here)

# --- Sector definitions ---

SECTORS <- c(
  "FOODHOME", "FOODAWAY", "ALCBEVG", "SHELTER", "UTILS", "HHOPER",
  "HHFURNSH", "APPAREL", "GASOIL", "VEHOTHXP", "VEHPURCH", "PUBTRANS",
  "MEDSERVS", "540000", "MEDSUPPL", "FEESADM", "TVAUDIO", "PETSPLAY",
  "ENTEROTH", "PERSCARE", "READING", "EDUCATN", "TOBACCO"
)

# Alternate UCC codes used for years 2015-2020 in the CEX hierarchy files
SECTORS_UCC_2015_2020 <- c(
  "FOODHO", "FOODAW", "ALCBEV", "SHELTE", "UTILS", "HHOPER",
  "HHFURN", "APPARE", "GASOIL", "VEHOTH", "VEHPUR", "PUBTRA",
  "MEDSER", "540000", "MEDSUP", "FEESAD", "TVAUDI", "PETSPL",
  "ENTERO", "PERSCA", "READIN", "EDUCAT", "TOBACC"
)

# --- Time range ---

FIRST_YEAR <- 2014
LAST_YEAR  <- 2023

# --- Baseline sector for elasticity estimation ---

BASELINE_SECTOR <- "APPAREL"

# --- CPI base year ---

BASE_YEAR <- 2014

# --- File paths ---

CEX_DATA_DIR     <- here("data", "raw", "cex", "zipdata")
CEX_STUBS_ZIP    <- here("data", "raw", "cex", "stubs.zip")
CPI_DIR          <- here("data", "raw", "cpi")
CPI_SECTORAL_DIR <- here("data", "raw", "cpi", "sectoral")
CPI_AGGREGATE    <- here("data", "raw", "cpi", "CPI.csv")

OUTPUT_FIGURES_DIR <- here("output", "figures")
OUTPUT_TABLES_DIR  <- here("output", "tables")

# --- Sample restrictions ---

MIN_AGE  <- 25
MAX_AGE  <- 64
MIN_INCOME <- 1000
INCOME_TRIM  <- c(0.01, 0.99)
EXP_TRIM     <- c(0.01, 0.99)
