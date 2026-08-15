# Libraries ---------------------------------------------------------------
library("quantmod")
library("shiny")
library("tidyverse")
library("tidyquant")
library("scales")
library("gt")
library("ggplot2")
library("PerformanceAnalytics")
library("dplyr")
library("Matrix")
library("glue")

source(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "Portfolio_Data_Source.R"))
source(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "Ticker_Graphs_Source.R"))
source(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "Connor_Sharesies_Source.R"))
source(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "Pie_Chart_Source.R"))
source(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "Correlation_HHI_Source.R"))
source(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "Sector_Performance_Source.R"))
source(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "Beta_Analysis_Source.R"))
source(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "Risk_Analysis_Source.R"))
source(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "Portfolio_History_Source.R"))

source(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "Realized_Gains_Source.R"))

source(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "Monte_Sim_Source.R"))
source(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "DCA_BearBull_Sim.R"))

# Portfolio_Data ----------------------------------------------------------
View(latest_prices)
View(portfolio_data)
# Ticker_Graph ------------------------------------------------------------
plot_ticker("FHLC")
 
# Connor_Sharesies --------------------------------------------------------
View(portfolio_full)
write.csv(portfolio_full, "portfolio_full.csv")
View(portfolio_summary)
Connors_Sharesies_2.0_Performance
gtsave(Connors_Sharesies_2.0_Performance, "Connors_Sharesies_2.0_Performance.png", expand = 10)
Connor_Sharesies_2.0_Portfolio
gtsave(Connor_Sharesies_2.0_Portfolio, "Connor_Sharesies_2.0_Portfolio.png", expand = 10)
# Pie Charts ------------------------------------------------------------------
portfolio_by_ticker 
ggsave("portfolio_by_ticker.png", width = 8, height = 6, dpi = 300)
Ticker_by_rest
ggsave("portfolio_by_ticker_grouped.png", width = 8, height = 6, dpi = 300)
portfolio_by_sector
ggsave("portfolio_by_sector.png", width = 8, height = 6, dpi = 300)
portfolio_by_industry
ggsave("portfolio_by_industry.png", width = 8, height = 6, dpi = 300)


# Correlation/HHI --------------------------------------------------
return_correlation_matrix
print(hhi_ticker)
print(hhi_sector)
print(weight_by_sector)

# Realized_Gains ----------------------------------------------------------
View(portfolio_realised)
View(portfolio_realised_summary)
Realised_Gains_Portfolio
gtsave(Realised_Gains_Portfolio, "Realised_Gains_Portfolio.png", expand = 10)
Realised_Gains_Summary
gtsave(Realised_Gains_Summary, "Realised_Gains_Summary.png", expand = 10)

# Sector_Performance_Analysis ---------------------------------------------
portfolio_full %>%  head(20)
sector_contrib
Sector_performance_contribution
gtsave(Sector_performance_contribution, "Sector_performance_contribution.png", expand = 10)
sector_contrib_plot
ticker_level_contribution
gtsave(ticker_level_contribution, "ticker_level_contribution.png", expand = 10)

# Beta_Analysis -----------------------------------------------------------
head(stock_returns)
Beta_Table
gtsave(Beta_Table, "Beta_Analysis_Table.png", expand = 10)
Beta_bar_chart
Beta_Contribution_Table
Beta_contribution_chart

# Risk_Analysis ---------------------------------------------------------------
VaR_Table
gtsave(VaR_Table, "VaR_Table.png", expand = 10)
drawdown_chart
ggsave("drawdown_chart.png", width = 8, height = 5, dpi = 300)
Risk_Table
gtsave(Risk_Table, "Risk_Table.png", expand = 10)
vol_decomp_chart
ggsave("vol_decomp_chart.png", width = 8, height = 6, dpi = 300)

# Simulation --------------------------------------------------------------
mc_summary_table
gtsave(mc_summary_table, "mc_summary_table.png", expand = 10)
mc_fan_chart
mc_final_dist

#Scenario
mc_scenario_table
gtsave(mc_scenario_table, "mc_scenario_table.png", expand = 10)
mc_scenario_chart
ggsave("mc_scenario_chart.png", width = 8, height = 6, dpi = 300)

# Return Overtime------------------------------------------------------------------

record_portfolio_snapshot()
update_portfolio_snapshot()
plot_portfolio_history()

# DCA_BearBull Simulation -------------------------------------------------

drift_assumptions_table
dca_alloc_table
dca_summary_table
dca_fan_chart
dca_final_dist

