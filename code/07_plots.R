# 07_plots.R -- All plotting code
#
# Each function builds a ggplot, saves it as PDF, and returns the plot
# object invisibly for interactive use.

library(ggplot2)
library(scales)

# --- Colour palette ---

COL_PRIMARY   <- "#1f4e79"
COL_SECONDARY <- "#c00000"
COL_TERTIARY  <- "#2d6a4f"
COL_ACCENT    <- "#e67e22"

# --- Shared theme ---

theme_mcpi <- function() {
  theme_bw(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(color = "grey40", size = 10),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      panel.grid.minor = element_blank()
    )
}


# --- Plot 1: Sample CPI vs Official CPI ---

plot_cpi_comparison <- function(sample_cpi, official_cpi, output_dir) {

  plot_df <- merge(sample_cpi, official_cpi, by = "Date")

  p <- ggplot(data = plot_df) +
    geom_line(aes(x = Date, y = sample_CPI, color = "Sample CPI"), linewidth = 0.7) +
    geom_line(aes(x = Date, y = official_CPI, color = "Official CPI (BLS)"),
              linewidth = 0.7, linetype = "dashed") +
    scale_color_manual(values = c("Sample CPI" = COL_PRIMARY,
                                  "Official CPI (BLS)" = COL_SECONDARY)) +
    scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
    labs(
      title    = "Sample CPI vs Official CPI",
      subtitle = "Laspeyres-type index from CEX average budget shares vs BLS CPIAUCSL",
      x = NULL, y = "Price Index (base year = 100)"
    ) +
    theme_mcpi()

  ggsave(file.path(output_dir, "cpi_comparison.pdf"), p,
         device = "pdf", width = 8, height = 5)
  message("Saved cpi_comparison.pdf")
  invisible(p)
}


# --- Plot 2: CPI vs MCPI ---

plot_cpi_vs_mcpi <- function(sample_cpi, mcpi, output_dir) {

  plot_df <- merge(sample_cpi, mcpi, by = "Date")

  p <- ggplot(data = plot_df) +
    geom_line(aes(x = Date, y = sample_CPI, color = "CPI"), linewidth = 0.7) +
    geom_line(aes(x = Date, y = mcpi, color = "MCPI"), linewidth = 0.7) +
    scale_color_manual(values = c("CPI" = COL_PRIMARY, "MCPI" = COL_SECONDARY)) +
    scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
    labs(
      title    = "Consumer Price Index vs Marginal Consumer Price Index",
      subtitle = "Average vs marginal budget share weighting of sectoral prices",
      x = NULL, y = "Price Index (base year = 100)"
    ) +
    theme_mcpi()

  ggsave(file.path(output_dir, "cpi_vs_mcpi.pdf"), p,
         device = "pdf", width = 8, height = 5)
  message("Saved cpi_vs_mcpi.pdf")
  invisible(p)
}


# --- Plot 3: Quintile-specific MCPIs ---

plot_quintile_mcpi <- function(quintiles_df, output_dir) {

  p <- ggplot(data = quintiles_df) +
    geom_line(aes(x = Date, y = top_mcpi, color = "Top 20%"), linewidth = 0.7) +
    geom_line(aes(x = Date, y = middle_mcpi, color = "Middle 20%"), linewidth = 0.7) +
    geom_line(aes(x = Date, y = bottom_mcpi, color = "Bottom 20%"), linewidth = 0.7) +
    scale_color_manual(values = c("Top 20%"    = COL_PRIMARY,
                                  "Middle 20%" = COL_ACCENT,
                                  "Bottom 20%" = COL_SECONDARY)) +
    scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
    labs(
      title    = "MCPI by Income Quintile",
      subtitle = "Marginal Consumer Price Index for top, middle, and bottom income groups",
      x = NULL, y = "Price Index (base year = 100)"
    ) +
    theme_mcpi()

  ggsave(file.path(output_dir, "quintile_mcpi.pdf"), p,
         device = "pdf", width = 8, height = 5)
  message("Saved quintile_mcpi.pdf")
  invisible(p)
}


# --- Plot 4: Marginal vs average budget shares ---

plot_budget_share_comparison <- function(t_avg_bs, output_dir) {

  plot_df <- data.frame(
    sector = rownames(t_avg_bs),
    ambs   = t_avg_bs$avg_ambs,
    avg_bs = t_avg_bs$avg_bs
  )

  plot_df_long <- tidyr::pivot_longer(
    plot_df,
    cols = c(ambs, avg_bs),
    names_to = "type",
    values_to = "share"
  )

  plot_df_long$type <- ifelse(plot_df_long$type == "ambs",
                              "Marginal Budget Share",
                              "Average Budget Share")

  # Order sectors by average budget share for readability
  sector_order <- plot_df$sector[order(plot_df$avg_bs, decreasing = TRUE)]
  plot_df_long$sector <- factor(plot_df_long$sector, levels = sector_order)

  p <- ggplot(plot_df_long, aes(x = sector, y = share, fill = type)) +
    geom_col(position = "dodge", width = 0.7) +
    scale_fill_manual(values = c("Average Budget Share"  = COL_PRIMARY,
                                 "Marginal Budget Share" = COL_SECONDARY)) +
    labs(
      title    = "Average vs Marginal Budget Shares",
      subtitle = "Time-averaged across sample period, sorted by average share",
      x = NULL, y = "Share"
    ) +
    theme_mcpi() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

  ggsave(file.path(output_dir, "budget_share_comparison.pdf"), p,
         device = "pdf", width = 10, height = 6)
  message("Saved budget_share_comparison.pdf")
  invisible(p)
}


# --- Plot 5: Income elasticity estimates ---

plot_elasticities <- function(ind_elasticities, sectors, output_dir) {

  # Compute time-averaged individual elasticities
  elast_data <- data.frame(
    sector = sectors,
    elasticity = sapply(sectors, function(s) {
      mean(ind_elasticities[[sector_elasticity_col(s)]], na.rm = TRUE)
    })
  )

  elast_data$sector <- factor(elast_data$sector,
                               levels = elast_data$sector[order(elast_data$elasticity)])

  p <- ggplot(elast_data, aes(x = elasticity, y = sector)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
    geom_point(color = COL_PRIMARY, size = 2.5) +
    labs(
      title    = "Estimated Income Elasticities by Sector",
      subtitle = "IV estimates, time-averaged; dashed line = unitary elasticity",
      x = "Income Elasticity", y = NULL
    ) +
    theme_mcpi()

  ggsave(file.path(output_dir, "income_elasticities.pdf"), p,
         device = "pdf", width = 8, height = 7)
  message("Saved income_elasticities.pdf")
  invisible(p)
}


# --- Save all plots ---

save_all_plots <- function(sample_cpi, official_cpi, mcpi,
                           quintiles_df, t_avg_bs, ind_elasticities,
                           sectors, output_dir) {

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  plot_cpi_comparison(sample_cpi, official_cpi, output_dir)
  plot_cpi_vs_mcpi(sample_cpi, mcpi, output_dir)
  plot_quintile_mcpi(quintiles_df, output_dir)
  plot_budget_share_comparison(t_avg_bs, output_dir)
  plot_elasticities(ind_elasticities, sectors, output_dir)
}
