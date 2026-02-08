# 04_price_indices.R -- CPI and MCPI construction
#
# Loads sectoral price data, constructs a sample CPI using average
# expenditure shares, loads the official BLS CPI, and constructs the
# Marginal Consumer Price Index (MCPI) using time-averaged aggregate
# marginal budget shares.

library(dplyr)


# --- Internal helper: load and normalise sectoral price data ---

load_sectoral_prices <- function(sectoral_cpi_dir, base_year, first_year, last_year) {

  csv_files <- list.files(sectoral_cpi_dir, pattern = "\\.csv$", full.names = TRUE)

  read_and_rename <- function(file_path) {
    data <- read.csv(file_path)
    industry <- tools::file_path_sans_ext(basename(file_path))
    colnames(data) <- c("Date", paste0(industry, "_price"))
    return(data)
  }

  price_levels <- Reduce(function(x, y) {
    merge(x, y, by = "Date", all = TRUE)
  }, lapply(csv_files, read_and_rename))

  price_levels <- na.omit(price_levels)
  price_levels$Date <- as.Date(price_levels$Date)
  price_levels$Year <- as.numeric(format(price_levels$Date, "%Y"))

  for (col in grep("_price$", names(price_levels), value = TRUE)) {
    price_levels[[col]] <- as.numeric(price_levels[[col]])
  }

  # Normalise to 100 in the base year
  base_year_prices <- price_levels[price_levels$Year == base_year, ]

  for (col in grep("_price$", names(price_levels), value = TRUE)) {
    if (col %in% names(base_year_prices)) {
      base_value <- base_year_prices[[col]][1]
      if (!is.na(base_value) && base_value != 0) {
        price_levels[[col]] <- (price_levels[[col]] / base_value) * 100
      } else {
        warning(paste("Base value is NA or zero for column:", col))
      }
    } else {
      warning(paste("Missing price column for base year:", col))
    }
  }

  price_levels_sample <- subset(price_levels, Year >= first_year & Year <= last_year)
  return(price_levels_sample)
}


# --- Sample CPI (Laspeyres-type, using average expenditure shares) ---

consumer_price_index <- function(avg_exp_shares, first_year, last_year,
                                 sectoral_cpi_dir, base_year) {

  prices <- load_sectoral_prices(sectoral_cpi_dir, base_year, first_year, last_year)

  cpi_timeseries <- data.frame(Date = unique(prices$Date), sample_CPI = NA)

  for (i in seq_along(cpi_timeseries$Date)) {
    date_i <- cpi_timeseries$Date[i]
    year_i <- as.numeric(format(as.Date(date_i), "%Y"))

    avg_exp_shares_i <- avg_exp_shares[avg_exp_shares$year == year_i, ]

    cpi_timeseries$sample_CPI[i] <- sum(sapply(names(avg_exp_shares_i)[-1], function(industry_base) {
      industry_base <- sub("_expenditure_share$", "", industry_base)
      price_col <- paste0(industry_base, "_price")
      share_col <- paste0(industry_base, "_expenditure_share")

      if (price_col %in% names(prices)) {
        price <- as.numeric(prices[prices$Date == date_i, price_col])
        share <- as.numeric(avg_exp_shares_i[[share_col]])
        price * share
      } else {
        0
      }
    }), na.rm = TRUE)
  }

  return(cpi_timeseries)
}


# --- Official CPI from FRED ---

official_consumer_price_index <- function(cpi_file, first_year, last_year) {

  cpi_data <- read.csv(cpi_file)
  cpi_data$Date <- as.Date(cpi_data$DATE)
  cpi_data$DATE <- NULL
  colnames(cpi_data)[which(colnames(cpi_data) == "CPIAUCSL")] <- "official_CPI"

  start_date <- as.Date(paste0(first_year, "-01-01"))
  end_date   <- as.Date(paste0(last_year, "-12-31"))

  cpi_official <- subset(cpi_data, Date >= start_date & Date <= end_date)
  return(cpi_official)
}


# --- MCPI (using time-averaged aggregate marginal budget shares) ---

marginal_consumer_price_index <- function(avg_exp_shares, t_avg_bs,
                                          first_year, last_year,
                                          sectoral_cpi_dir, base_year) {

  prices <- load_sectoral_prices(sectoral_cpi_dir, base_year, first_year, last_year)

  mcpi_timeseries <- data.frame(Date = unique(prices$Date), mcpi = NA)

  for (i in seq_along(mcpi_timeseries$Date)) {
    date_i <- mcpi_timeseries$Date[i]

    mcpi_timeseries$mcpi[i] <- sum(sapply(names(avg_exp_shares)[-1], function(industry_base) {
      industry_base <- sub("_expenditure_share$", "", industry_base)
      price_col <- paste0(industry_base, "_price")
      share <- t_avg_bs[industry_base, "avg_ambs"]

      if (price_col %in% names(prices)) {
        price <- as.numeric(prices[prices$Date == date_i, price_col])
        price * share
      } else {
        0
      }
    }), na.rm = TRUE)
  }

  return(mcpi_timeseries)
}
