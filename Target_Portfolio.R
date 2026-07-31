# Target Portfolio --------------------------------------------------------
Tar_Connor_Sharesies_2.0_Portfolio
Tar_Connors_Sharesies_2.0_Performance
Tar_portfolio_by_industry
Tar_portfolio_by_sector
Tar_Ticker_by_rest
Tar_portfolio_by_ticker
View(Tar_portfolio_full)
Tar_Beta_Table
Tar_Beta_bar_chart
Tar_Beta_Contribution_Table
Tar_Beta_contribution_chart
weight_by_sector
hhi_sector
hhi_ticker
mc_final_dist
mc_fan_chart
mc_summary_table

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


# Data --------------------------------------------------------------------


Tar_data <- tribble(
  ~ticker,    ~shares,       ~total_cost, ~sector,              ~industry,                          ~currency,
  "AAPL",     0.65074510,    200,       "Technology",         "Consumer Electronics",             "USD",
  "GOOGL",    0.81404499,    300,       "Technology",         "Internet Services",                "USD",
  "NVDA",     1.21891760,    250,       "Technology",         "Semiconductors",                   "USD",
  "SAP",      0.54118411,    100,       "Technology",         "Enterprise Software",              "USD",
  "LRCX",     0.49459246,    150,       "Technology",         "Semiconductors",                   "USD",
  "MU",       0.17359102,    150,       "Technology",         "Semiconductors",                   "USD",
  "MRVL",     0.94887463,    250,       "Technology",         "Semiconductors",                   "USD",
  "NOK",      6.96864112,    100,       "Technology",         "Communication Equipment",          "USD",
  "CAT",      0.43983594,    400,       "Industrials",        "Construction Machinery",           "USD",
  "HWM",      1.58761659,    400,       "Industrials",        "Aerospace Components",             "USD",
  "RKLB",     2.33644860,    250,       "Industrials",        "Space & Aerospace",                "USD",
  "UNP",      0.73443008,    200,       "Industrials",        "Railways", "USD", 
  "AMZN",     2.23549974,    550,       "Consumer Cyclical",  "Internet Retail",                  "USD", 
  "DORM",     1.96803905,    250,       "Consumer Cyclical",  "Auto Parts",                       "USD",
  "MELI",     0.12439358,    200,       "Consumer Cyclical",  "Internet Retail",                  "USD",
  "BNY",      2.10644572,    300,     "Financials",         "Asset Management",                 "USD",
  "GS",       0.43310876,    450,     "Financials",         "Investment Banking",               "USD",
  "VRTX",     0.22379876,    100,     "Healthcare",       "Biotechnology",                    "USD",
  "XEL",      5.06072874,    400,     "Utilitis",         "Utilities - Regulated Electricity","USD",
  "IVV.AX",   50.1427157,    3545.09,      "ETF",                "US Equities",                      "AUD",
  "VEU",      6.16370809,    500,       "ETF",                "World Equities",                   "USD",
  "VHY.AX",   8.54209639,    709.02,       "ETF",                "World Equities",                   "AUD",
  "PICK",     12.1516526,    750,       "ETF",                "Natural Resources",                "USD",
  "FHLC",     10.2054701,    750,       "ETF",                "Health",                           "USD"
)

# FX rates: convert AUD and NZD prices/costs to USD
# Fetches the latest available rate from Yahoo Finance

fx_rates <- tq_get(c("NZDUSD=X", "AUDUSD=X"), get = "stock.prices", from = Sys.Date() - 5) %>%
  group_by(symbol) %>%
  slice_tail(n = 1) %>%
  transmute(
    currency  = dplyr::recode(symbol, "NZDUSD=X" = "NZD", "AUDUSD=X" = "AUD"),
    fx_to_usd = close
  )

# Convert total_cost to USD in-place
Tar_data <- Tar_data %>%
  left_join(fx_rates, by = "currency") %>%
  mutate(
    fx_to_usd  = replace_na(fx_to_usd, 1.0),
    total_cost = total_cost * fx_to_usd
  ) %>%
  dplyr::select(-fx_to_usd)

# Sum of Connors Sharesies 2.0 --------------------------------------------
Tar_two_day_prices <- Tar_data %>% 
  tq_get(get = "stock.prices", from = Sys.Date() - 10) %>% 
  filter(!is.na(close)) %>%          # drop any mid-session rows (market still open)
  group_by(ticker) %>% 
  slice_tail(n = 2) %>% 
  mutate(day = c("prev_close", "last_close")) %>% 
  # Convert AUD/NZD prices to USD so all values are comparable
  left_join(fx_rates, by = "currency") %>%
  mutate(
    fx_to_usd = replace_na(fx_to_usd, 1.0),
    close     = close * fx_to_usd
  ) %>%
  dplyr::select(ticker, day, close)

Tar_prices_wide <- Tar_two_day_prices %>% 
  pivot_wider(names_from = day, values_from = close)

Tar_portfolio_full <- Tar_data %>% 
  left_join(Tar_prices_wide, by = "ticker") %>% 
  mutate(
    avg_cost = total_cost / shares, 
    current_worth   = shares * last_close,
    prev_worth      = shares * prev_close,
    total_gain_loss = current_worth - total_cost,
    total_pct       = (total_gain_loss / total_cost) * 100,
    day_gain_loss   = current_worth - prev_worth,
    day_pct         = (day_gain_loss / prev_worth) * 100,
    weight = current_worth / sum(current_worth, na.rm = TRUE),
  )

Tar_portfolio_summary <- Tar_portfolio_full %>% 
  summarise(
    total_cost        = sum(total_cost),
    current_value     = sum(current_worth),
    day_change        = sum(day_gain_loss),
    day_pct_change    = (sum(day_gain_loss) / sum(prev_worth)) * 100,
    total_change      = sum(total_gain_loss),
    total_pct_change  = (sum(total_gain_loss) / sum(total_cost)) * 100
  ) 

#Portfolio Performances

Tar_Connors_Sharesies_2.0_Performance <- Tar_portfolio_summary %>% 
  gt() %>% 
  tab_header(
    title    = md("**Target-Connor's Sharesies 2.0**"),
    subtitle = "Portfolio Summary"
  ) %>% 
  cols_label(
    total_cost       = "Total Cost",
    current_value    = "Current Value",
    day_change       = "Day Change ($)",
    day_pct_change   = "Day Change (%)",
    total_change     = "Total Change ($)",
    total_pct_change = "Total Change (%)"
  ) %>% 
  fmt_currency(columns = c(total_cost, current_value, day_change, total_change)) %>% 
  fmt_number(columns = c(day_pct_change, total_pct_change), decimals = 2, pattern = "{x}%") %>% 
  # Green for positive changes
  tab_style(
    style = cell_text(color = "forestgreen", weight = "bold"),
    locations = cells_body(columns = c(day_change, day_pct_change), rows = day_change > 0)
  ) %>% 
  tab_style(
    style = cell_text(color = "forestgreen", weight = "bold"),
    locations = cells_body(columns = c(total_change, total_pct_change), rows = total_change > 0)
  ) %>% 
  # Red for negative changes
  tab_style(
    style = cell_text(color = "red", weight = "bold"),
    locations = cells_body(columns = c(day_change, day_pct_change), rows = day_change < 0)
  ) %>% 
  tab_style(
    style = cell_text(color = "red", weight = "bold"),
    locations = cells_body(columns = c(total_change, total_pct_change), rows = total_change < 0)
  ) %>% 
  # Style column headers white
  tab_style(
    style = cell_text(color = "white", weight = "bold"),
    locations = cells_column_labels()
  ) %>% 
  tab_options(
    table.font.size                = 14,
    heading.background.color       = "#1a1a2e",
    heading.title.font.size        = 18,
    heading.subtitle.font.size     = 13,
    column_labels.background.color = "#16213e",
    column_labels.font.weight      = "bold",
    row.striping.include_table_body = TRUE,
    row.striping.background_color  = "#f0f4ff"
  )


#Connor_Sharesies_2.0_Portfolio

Tar_Connor_Sharesies_2.0_Portfolio <- Tar_portfolio_full %>% 
  dplyr::select(ticker, sector, total_cost, current_worth, day_gain_loss, day_pct, total_gain_loss, total_pct) %>% 
  arrange(desc(total_pct)) %>% 
  gt() %>% 
  tab_header(
    title    = md("**Connor's Sharesies 2.0**"),
    subtitle = "Per-Stock Breakdown"
  ) %>% 
  cols_label(
    ticker          = "Ticker",
    sector          = "Sector",
    total_cost      = "Cost",
    current_worth   = "Value",
    day_gain_loss   = "Day ($)",
    day_pct         = "Day (%)",
    total_gain_loss = "Total ($)",
    total_pct       = "Total (%)"
  ) %>% 
  fmt_currency(columns = c(total_cost, current_worth, day_gain_loss, total_gain_loss)) %>% 
  fmt_number(columns = c(day_pct, total_pct), decimals = 1, pattern = "{x}%") %>% 
  # Green positive
  tab_style(
    style = cell_text(color = "forestgreen", weight = "bold"),
    locations = cells_body(columns = c(day_gain_loss, day_pct), rows = day_gain_loss > 0)
  ) %>% 
  tab_style(
    style = cell_text(color = "forestgreen", weight = "bold"),
    locations = cells_body(columns = c(total_gain_loss, total_pct), rows = total_gain_loss > 0)
  ) %>% 
  # Red negative
  tab_style(
    style = cell_text(color = "red", weight = "bold"),
    locations = cells_body(columns = c(day_gain_loss, day_pct), rows = day_gain_loss < 0)
  ) %>% 
  tab_style(
    style = cell_text(color = "red", weight = "bold"),
    locations = cells_body(columns = c(total_gain_loss, total_pct), rows = total_gain_loss < 0)
  ) %>% 
  tab_style(
    style = cell_text(color = "white", weight = "bold"),
    locations = cells_column_labels()
  ) %>% 
  # Shade by sector using row groups
  tab_row_group(label = "Technology",        rows = sector == "Technology") %>% 
  tab_row_group(label = "Industrials",       rows = sector == "Industrials") %>% 
  tab_row_group(label = "Financials",        rows = sector == "Financials") %>% 
  tab_row_group(label = "Consumer Cyclical", rows = sector == "Consumer Cyclical") %>% 
  tab_row_group(label = "Healthcare",        rows = sector == "Healthcare") %>% 
  tab_row_group(label = "ETF",               rows = sector == "ETF") %>% 
  tab_row_group(label = "Utilitis",          rows = sector == "Utilitis") %>% 
  tab_row_group(label = "Engery",            rows = sector == "Engery") %>% 
  cols_hide(sector) %>% 
  tab_options(
    table.font.size                = 13,
    heading.background.color       = "#323031",
    heading.title.font.size        = 18,
    heading.subtitle.font.size     = 13,
    column_labels.background.color = "#571F4E",
    column_labels.font.weight      = "bold",
    row_group.background.color     = "#5D5179",
    row_group.font.weight          = "bold",
    row.striping.include_table_body = TRUE,
    row.striping.background_color  = "#f7faff"
  )


# latest_prices: convert close to USD for display
Tar_latest_prices <- Tar_data %>%
  tq_get(get = "stock.prices", from = Sys.Date() - 5) %>%
  group_by(ticker) %>%
  slice_tail(n = 1) %>%
  left_join(fx_rates, by = "currency") %>%
  mutate(
    fx_to_usd = replace_na(fx_to_usd, 1.0),
    close     = close * fx_to_usd
  ) %>%
  dplyr::select(ticker, close)


# Pie Charts --------------------------------------------------------------
Tar_portfolio_by_ticker <- Tar_portfolio_full %>% 
  mutate(ticker = reorder(ticker, current_worth)) %>% 
  ggplot(aes(x = 1, y = current_worth, fill = ticker)) +
  geom_col(color = "white") + 
  coord_polar(theta = "y") +
  theme_void() + 
  labs(title = "Portfolio Weight Distribution") + 
  geom_text(aes(label = scales::percent(current_worth/sum(current_worth), 
                                        accuracy = 0.1)),
            position = position_stack(vjust = 0.5))

#Tickers Rest
Tar_Ticker_by_rest <- Tar_portfolio_full %>%
  mutate(ticker_grouped = ifelse(weight < 0.03, "Rest", ticker)) %>%
  group_by(ticker_grouped) %>%
  summarise(worth = sum(current_worth), .groups = "drop") %>%
  mutate(pct = worth / sum(worth),
         ticker_grouped = reorder(ticker_grouped, worth)) %>%
  mutate(ticker = reorder(ticker_grouped, worth)) %>% 
  ggplot(aes(x = 1, y = worth, fill = ticker_grouped)) +
  geom_col(color = "white") + 
  coord_polar(theta = "y") +
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)), 
            position = position_stack(vjust = 0.5), size = 3) +
  theme_void() +
  labs(title = "Portfolio Weight by Ticker", fill = "Ticker")

#Sector
Tar_portfolio_by_sector <- Tar_portfolio_full%>%
  group_by(sector) %>%
  summarise(worth = sum(current_worth), .groups = "drop") %>%
  mutate(pct = worth / sum(worth),
         sector = reorder(sector, worth),
         label = paste0(sector, "\n", 
                        scales::percent(pct, accuracy = 0.1))) %>%
  ggplot(aes(x = 1, y = worth, fill = sector)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5), size = 3) +
  theme_void() +
  labs(title = "Portfolio Weight by Sector") +
  theme(legend.position = "none")

#Industry#Isectorndustry
Tar_portfolio_by_industry <- Tar_portfolio_full %>%
  group_by(industry) %>%
  summarise(worth = sum(current_worth), .groups = "drop") %>%
  mutate(pct = worth / sum(worth)) %>%
  mutate(industry = reorder(industry, worth)) %>% 
  ggplot(aes(x = 1, y = worth, fill = industry)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)), 
            position = position_stack(vjust = 0.5), size = 3) +
  theme_void() +
  labs(title = "Portfolio Weight by Industry")



# Beta --------------------------------------------------------------------

Tar_portfolio_weights <- Tar_portfolio_full %>%
  filter(!is.na(current_worth)) %>%
  mutate(weight = current_worth / sum(current_worth)) %>% 
  dplyr::select(ticker, weight, sector)

Tar_all_tickers <- Tar_portfolio_weights$ticker
from_date <- "2025-05-01"
to_date   <- Sys.Date()
market_returns <- tq_get("SPY", get = "stock.prices", from = from_date, to = to_date) %>% 
  tq_transmute(select = adjusted, mutate_fun = periodReturn,
               period = "daily", col_rename = "mkt_return")
Tar_stock_returns <- tq_get(Tar_all_tickers, get = "stock.prices", from = from_date, to = to_date) %>% 
  group_by(symbol) %>%
  filter(!is.na(adjusted)) %>%
  tq_transmute(select = adjusted, mutate_fun = periodReturn,
               period = "daily", col_rename = "stk_return")

#Individual & Portfolio Beta Calculation

# !Note: ANZ.NZ, VAS.AX, IXJ.AX, IVV.AX, V500.AX trade in NZD/AUD;
# !their betas vs SPY include currency noise — interpret with caution.

Tar_beta_data <- Tar_stock_returns %>% 
  left_join(market_returns, by = "date") %>% 
  filter(!is.na(mkt_return), !is.na(stk_return), stk_return != 0 | mkt_return != 0)

Tar_individual_betas <- Tar_beta_data %>% 
  group_by(symbol) %>% 
  summarise(
    beta      = cov(stk_return, mkt_return) / var(mkt_return),
    r_squared = cor(stk_return, mkt_return)^2,
    n_obs     = n(),
    .groups   = "drop"
  ) %>% 
  left_join(Tar_portfolio_weights %>%  rename(symbol = ticker), by = "symbol") %>% 
  arrange(desc(beta))

# Weighted portfolio beta
Tar_portfolio_beta <- sum(Tar_individual_betas$beta * Tar_individual_betas$weight, na.rm = TRUE)

#Beta Table

Tar_Beta_Table <- Tar_individual_betas %>% 
  dplyr::select(symbol, sector, beta, r_squared, weight) %>% 
  gt() %>% 
  tab_header(
    title    = md("**Portfolio Beta Analysis**"),
    subtitle = glue::glue("vs S&P 500 (SPY) | Portfolio Beta: {round(Tar_portfolio_beta, 3)}")
  ) %>% 
  cols_label(
    symbol    = "Ticker",
    sector    = "Sector",
    beta      = "Beta",
    r_squared = "R²",
    weight    = "Portfolio Weight"
  ) %>% 
  fmt_number(columns = c(beta, r_squared), decimals = 3) %>% 
  fmt_percent(columns = weight, decimals = 1) %>% 
  data_color(
    columns = beta,
    method  = "numeric",
    palette = c("#4575b4", "#ffffbf", "#d73027")
  ) %>% 
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(columns = beta, rows = symbol == "PORTFOLIO")
  ) %>% 
  tab_options(table.font.size = 13, row.striping.include_table_body = TRUE,
              row.striping.background_color = "#f5f5f5")

#Beta Bar Chart 

Tar_Beta_bar_chart <- ggplot(Tar_individual_betas, aes(x = reorder(symbol, beta), y = beta, fill = sector)) +
  geom_col() +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
  geom_hline(yintercept = Tar_portfolio_beta, linetype = "solid", colour = "black", linewidth = 0.8) +
  annotate("text", x = 1.5, y = Tar_portfolio_beta + 0.08,
           label = paste("Portfolio β =", round(Tar_portfolio_beta, 2)),
           size = 3.5, hjust = 0) +
  annotate("text", x = 1.5, y = 1 + 0.08,
           label = "Market β = 1", colour = "grey40", size = 3.5, hjust = 0) +
  labs(
    title = "Individual Stock Beta vs S&P 500",
    subtitle = "Dashed line = market beta (1.0) | Solid line = portfolio beta",
    x = NULL, y = "Beta", fill = "Sector"
  ) +
  coord_flip() +
  theme_minimal(base_size = 13)

#Contribution to portfolio beta analysis

Tar_beta_contrib <- Tar_individual_betas %>% 
  mutate(
    beta_contribution = weight * beta,
    fill_col = if_else(beta_contribution >= 0, "Positive", "Negative")
  ) %>% 
  arrange(desc(beta_contribution))

# Table 
Tar_Beta_Contribution_Table <- Tar_beta_contrib %>% 
  dplyr::select(symbol, sector, weight, beta, beta_contribution) %>% 
  gt() %>% 
  tab_header(
    title    = md("**Contribution to Portfolio Beta**"),
    subtitle = glue::glue("Portfolio Beta: {round(Tar_portfolio_beta, 3)} | Benchmark: SPY")
  ) %>% 
  cols_label(
    symbol            = "Ticker",
    sector            = "Sector",
    weight            = "Weight",
    beta              = "Beta",
    beta_contribution = "Beta Contribution"
  ) %>% 
  fmt_percent(columns = weight, decimals = 1) %>% 
  fmt_number(columns = c(beta, beta_contribution), decimals = 3) %>% 
  data_color(
    columns = beta_contribution,
    method  = "numeric",
    palette = c("#4575b4", "#ffffbf", "#d73027")
  ) %>% 
  tab_footnote(
    footnote  = "Beta Contribution = Portfolio Weight × Stock Beta",
    locations = cells_column_labels(beta_contribution)
  ) %>% 
  tab_options(table.font.size = 13, row.striping.include_table_body = TRUE,
              row.striping.background_color = "#f5f5f5")

# Chart
Tar_Beta_contribution_chart <- ggplot(Tar_beta_contrib, aes(x = reorder(symbol, beta_contribution), y = beta_contribution, fill = fill_col)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey40") +
  geom_text(
    aes(
      label = round(beta_contribution, 3),
      hjust = if_else(beta_contribution >= 0, -0.15, 1.15)
    ),
    size = 3
  ) +
  scale_fill_manual(values = c("Positive" = "#d73027", "Negative" = "#4575b4"), guide = "none") +
  scale_x_discrete(expand = expansion(add = 1)) +
  labs(
    title    = "Contribution to Portfolio Beta",
    subtitle = glue::glue("Portfolio Beta: {round(Tar_portfolio_beta, 3)}"),
    x = NULL, y = "Beta Contribution"
  ) +
  coord_flip() +
  theme_minimal(base_size = 13)

# Correlation/HHI ----------------------------------------------------------------

if (!all(sapply(Tar_portfolio_full$ticker, exists))) {
  getSymbols(Tar_portfolio_full$ticker, from = "2026-04-01", to = Sys.Date())
}

# log returns matrix
tickers <- Tar_portfolio_full$ticker

returns_list <- lapply(tickers, function(t) {
  sym <- get(gsub("\\.", "\\.", t))
  prices <- Cl(sym)
  ret <- diff(log(prices))
  ret <- as.data.frame(ret)
  ret$date <- rownames(ret)
  colnames(ret)[1] <- t
  ret
})

returns_df <- Reduce(function(a, b) merge(a, b, by = "date", all = FALSE), returns_list)
rownames(returns_df) <- returns_df$date
returns_df$date <- NULL

# Correlation matrix
cor_matrix <- cor(returns_df, use = "pairwise.complete.obs")

cor_long <- as.data.frame(as.table(cor_matrix)) %>% 
  rename(Ticker1 = Var1, Ticker2 = Var2, Correlation = Freq)

return_correlation_matrix <- ggplot(cor_long, aes(x = Ticker1, y = Ticker2, fill = Correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#d73027", mid = "white", high = "#1a6faf",
                       midpoint = 0, limits = c(-1, 1)) +
  geom_text(aes(label = round(Correlation, 1)), size = 2.2) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(face = "bold")) +
  labs(title = "Return Correlation Matrix",
       subtitle = "Daily log returns, Apr 1 – May 9 2026",
       x = NULL, y = NULL, fill = "Correlation")

# HHI = sum of squared weights (ranges 0 to 1; closer to 1 = more concentrated)
# HHI by ticker
hhi_ticker <- Tar_portfolio_full %>% 
  summarise(HHI = sum(weight^2),
            `Equivalent N` = round(1 / sum(weight^2))) %>% 
  mutate(Level = case_when(
    HHI < 0.10 ~ "Diversified",
    HHI < 0.18 ~ "Moderate",
    TRUE        ~ "Concentrated"
  ))

# HHI by sector
hhi_sector <- Tar_portfolio_full %>% 
  group_by(sector) %>% 
  summarise(sector_weight = sum(weight), .groups = "drop") %>% 
  summarise(HHI = sum(sector_weight^2),
            `Equivalent N` = round(1 / sum(sector_weight^2))) %>% 
  mutate(Level = case_when(
    HHI < 0.10 ~ "Diversified",
    HHI < 0.18 ~ "Moderate",
    TRUE        ~ "Concentrated"
  ))

# Per-sector weight breakdown
weight_by_sector <- Tar_portfolio_full %>% 
  group_by(sector) %>% 
  summarise(weight = sum(weight), .groups = "drop") %>% 
  mutate(pct = scales::percent(weight, accuracy = 0.1)) %>% 
  arrange(desc(weight)) 

# SIM ---------------------------------------------------------------------
# --- Parameters ---------------------------------------------------------------
n_sims    <- 10000
n_days    <- 252   # 1 trading year
start_val <- sum(Tar_portfolio_full$current_worth, na.rm = TRUE)

# --- Fetch 3-year price history -----------------------------------------------
# --- Fetch 3-year price history -----------------------------------------------
sim_tickers <- Tar_portfolio_weights$ticker

prices_long <- tq_get(
  sim_tickers,
  from = "2022-05-01",
  to   = Sys.Date(),
  get  = "stock.prices"
)

# --- Fetch FX history ---------------------------------------------------------
fx_history <- tq_get(
  c("AUDUSD=X", "NZDUSD=X"),
  from = "2022-05-01",
  to   = Sys.Date(),
  get  = "stock.prices"
) %>%
  select(date, symbol, fx_close = close) %>%
  mutate(
    currency = recode(symbol,
                      "AUDUSD=X" = "AUD",
                      "NZDUSD=X" = "NZD")
  ) %>%
  select(date, currency, fx_close)

# --- Convert adjusted prices to USD properly ---------------------------------
prices_long_usd <- prices_long %>%
  left_join(Tar_data %>% select(ticker, currency),
            by = c("symbol" = "ticker")) %>%
  left_join(fx_history, by = c("date", "currency")) %>%
  group_by(symbol) %>%
  arrange(date) %>%
  tidyr::fill(fx_close, .direction = "downup") %>%
  ungroup() %>%
  mutate(
    fx_close = if_else(currency == "USD", 1, fx_close),
    adjusted_usd = adjusted * fx_close
  )

# --- Build USD returns matrix ------------------------------------------------
returns_sim <- prices_long_usd %>% 
  group_by(symbol) %>% 
  arrange(date) %>% 
  mutate(daily_return = adjusted_usd / lag(adjusted_usd) - 1) %>% 
  ungroup() %>% 
  select(date, symbol, daily_return) %>% 
  pivot_wider(names_from = symbol, values_from = daily_return) %>% 
  arrange(date)

returns_sim <- returns_sim %>%
  select(-date) %>%
  drop_na()
# --- Align weights to returns_sim columns ------------------------------------
# Build w from portfolio_weights, keeping only tickers present in returns_sim
# and re-normalising so they sum to 1.
w_df <- Tar_portfolio_weights %>%
  dplyr::filter(ticker %in% names(returns_sim)) %>%
  dplyr::slice(match(names(returns_sim), ticker))
w <- w_df$weight / sum(w_df$weight)

# --- Covariance matrix & eigendecomposition -----------------------------------
mu_long <- colMeans(returns_sim)
cov_raw <- cov(returns_sim)

# Project to nearest positive definite matrix (handles near-collinear assets)
cov_pd  <- as.matrix(nearPD(cov_raw, corr = FALSE)$mat)

eig     <- eigen(cov_pd, symmetric = TRUE)
L_eigen <- eig$vectors %*% diag(sqrt(pmax(eig$values, 0)))

# --- Run simulation -----------------------------------------------------------
sim_paths <- matrix(NA_real_, nrow = n_days + 1, ncol = n_sims)
sim_paths[1, ] <- start_val

for (sim in seq_len(n_sims)) {
  z       <- matrix(rnorm(n_days * length(mu_long)), nrow = n_days)
  r_asset <- z %*% t(L_eigen) + matrix(mu_long, nrow = n_days, ncol = length(mu_long), byrow = TRUE)
  r_port  <- as.vector(r_asset %*% w)
  sim_paths[-1, sim] <- start_val * cumprod(1 + r_port)
}

# --- Summary statistics -------------------------------------------------------
final_vals <- sim_paths[n_days + 1, ]

mc_summary_data <- data.frame(
  scenario     = c("Starting Value", "5th Percentile (VaR 95%)", "25th Percentile",
                   "Median", "Mean", "75th Percentile", "95th Percentile",
                   "Probability of Loss", "Probability of >+50%"),
  final_value  = c(start_val,
                   quantile(final_vals, 0.05), quantile(final_vals, 0.25),
                   median(final_vals), mean(final_vals),
                   quantile(final_vals, 0.75), quantile(final_vals, 0.95),
                   NA, NA),
  return_pct   = c(0,
                   (quantile(final_vals, 0.05) / start_val - 1) * 100,
                   (quantile(final_vals, 0.25) / start_val - 1) * 100,
                   (median(final_vals)         / start_val - 1) * 100,
                   (mean(final_vals)           / start_val - 1) * 100,
                   (quantile(final_vals, 0.75) / start_val - 1) * 100,
                   (quantile(final_vals, 0.95) / start_val - 1) * 100,
                   mean(final_vals < start_val) * 100,
                   mean(final_vals > start_val * 1.5) * 100)
)

mc_summary_table <- mc_summary_data %>% 
  gt() %>% 
  tab_header(
    title    = "Monte Carlo Simulation — 1-Year Outlook",
    subtitle = "1,000 simulations · Based on 3-year return history"
  ) %>% 
  cols_label(
    scenario    = "Scenario",
    final_value = "Final Value",
    return_pct  = "Return"
  ) %>% 
  fmt_currency(columns = final_value, currency = "USD", decimals = 2) %>% 
  fmt_number(columns = return_pct, decimals = 1, suffix = "%") %>% 
  tab_row_group(label = "Probabilities",  rows = 8:9) %>% 
  tab_row_group(label = "Percentiles",    rows = 2:7) %>% 
  tab_row_group(label = "Starting Point", rows = 1) %>% 
  sub_missing(columns = final_value, missing_text = "—") %>% 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>% 
  tab_style(
    style = cell_fill(color = "#EFF6FF"),
    locations = cells_body(rows = scenario == "Median")
  ) %>% 
  tab_style(
    style = cell_text(color = "#DC2626"),
    locations = cells_body(columns = return_pct, rows = return_pct < 0)
  ) %>% 
  tab_style(
    style = cell_text(color = "#16A34A"),
    locations = cells_body(columns = return_pct, rows = return_pct > 0 & !is.na(final_value))
  ) %>% 
  tab_options(table.font.size = 13)

# --- Fan chart ----------------------------------------------------------------
days_seq <- 0:n_days
pctiles  <- c(0.05, 0.25, 0.50, 0.75, 0.95)

band_df <- pctiles |>
  purrr::map_dfc(\(p) apply(sim_paths, 1, quantile, probs = p)) |>
  setNames(paste0("p", pctiles * 100)) |>
  dplyr::mutate(day = days_seq)

set.seed(4821)
path_sample <- sim_paths[, sample(n_sims, 100)] |>
  as.data.frame() |>
  dplyr::mutate(day = days_seq) |>
  tidyr::pivot_longer(-day, names_to = "sim", values_to = "value")

mc_fan_chart <- ggplot() +
  geom_line(data = path_sample, aes(x = day, y = value, group = sim),
            colour = "#3B82F6", alpha = 0.06, linewidth = 0.3) +
  geom_ribbon(data = band_df, aes(x = day, ymin = p5, ymax = p95),
              fill = "#3B82F6", alpha = 0.15) +
  geom_ribbon(data = band_df, aes(x = day, ymin = p25, ymax = p75),
              fill = "#3B82F6", alpha = 0.25) +
  geom_line(data = band_df, aes(x = day, y = p50),
            colour = "#1D4ED8", linewidth = 1) +
  geom_hline(yintercept = start_val, linetype = "dashed", colour = "grey40") +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(
    breaks = seq(0, n_days, by = 62),
    labels = c("Now", "3M", "6M", "9M", "1Y")
  ) +
  labs(
    title    = "Portfolio Monte Carlo Simulation",
    subtitle = "10,000 simulations over 252 trading days (1 year) · Based on 3-year return history",
    x        = NULL,
    y        = "Portfolio Value",
    caption  = "Shaded bands: 5th–95th pctile (light), 25th–75th pctile (dark) · Line: median"
  ) +
  theme_minimal(base_size = 13)

# --- Distribution of final values ---------------------------------------------
final_df <- data.frame(final_value = final_vals)

x_low  <- quantile(final_vals, 0.01, na.rm = TRUE)
x_high <- quantile(final_vals, 0.99, na.rm = TRUE)

mc_final_dist <- ggplot(final_df, aes(x = final_value)) +
  geom_histogram(bins = 50, fill = "#3B82F6", colour = "white", alpha = 0.85) +
  geom_vline(xintercept = start_val, linetype = "dashed", colour = "grey30", linewidth = 0.8) +
  geom_vline(xintercept = median(final_vals), linetype = "solid", colour = "#1D4ED8", linewidth = 1) +
  geom_vline(xintercept = quantile(final_vals, 0.05), linetype = "dotted", colour = "#EF4444", linewidth = 0.9) +
  coord_cartesian(xlim = c(x_low, x_high)) +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(
    title = "Distribution of Portfolio Values After 1 Year",
    subtitle = "10,000 Monte Carlo simulations, zoomed to 1st–99th percentile",
    x = "Final Portfolio Value",
    y = "Count"
  ) +
  theme_minimal(base_size = 13)


# Check -------------------------------------------------------------------

c(
  start = start_val,
  p5 = quantile(final_vals, 0.05),
  median = median(final_vals),
  mean = mean(final_vals),
  p95 = quantile(final_vals, 0.95),
  min = min(final_vals),
  max = max(final_vals)
)

