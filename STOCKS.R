library("quantmod")
library("shiny")
library("tidyverse")
library("tidyquant")
library("scales")
library("gt")
library("tidyverse")
library("ggplot2")
library("PerformanceAnalytics")
library("dplyr")
# Completed ---------------------------------------------------------------
#Portfolio Current
Connor_Sharesies_2.0_Portfolio
Connors_Sharesies_2.0_Performance

#Data
View(portfolio_full)
View(portfolio_summary)

#Realised Gains Portfolio
Realised_Gains_Portfolio
Realised_Gains_Summary

#Contribution Analysis
#Sector
Sector_performance_contribution
sector_contrib_plot
#Ticker
ticker_level_contribution

#Beta Analysis
Beta_Table
Beta_bar_chart
#By Weight
Beta_Contribution_Table
Beta_contribution_chart

#CSV File
write.csv(portfolio_full, "Connors_Sharesies_2.0_Portfolio.csv", row.names = FALSE)
write.csv(portfolio_summary, "Connors_Sharesies_2.0_Performance.csv", row.names = FALSE)

# Portfolio Source Data Done ------------------------------------------------------
#Source 2.0
portfolio_data <- tribble(
  ~ticker,   ~shares,     ~total_cost, ~sector,             ~industry,
  "AAPL",    0.02456,     7.00,        "Technology",        "Consumer Electronics",
  "GOOGL",   0.031789,    12.50,       "Technology",        "Internet Services",
  "TSM",     0.00945,     3.90,        "Technology",        "Semiconductors",
  "NVDA",    0.031846,    6.90,        "Technology",        "Semiconductors",
  "IONQ",    0.139908,    7.00,        "Technology",        "Quantum Computing",
  "TTMI",    0.044607,    7.50,        "Technology",        "Electronic Components",
  "AXTI",    0.031056,    3.50,        "Technology",        "Semiconductors",
  "QBTS",    0.407013,    9.24,        "Technology",        "Quantum Computing",
  "SAP",     0.039962,    7.00,        "Technology",        "Enterprise Software",
  "LRCX",    0.024243,    6.50,        "Technology",        "Semiconductors",
  "CAT",     0.005496,    5.00,        "Industrials",       "Construction Machinery",
  "GE",      0.021907,    7.00,        "Industrials",       "Aerospace & Defense",
  "ESE",     0.019189,    6.01,        "Industrials",       "Defense Electronics",
  "HWM",     0.06334,     15.00,       "Industrials",       "Aerospace Components",
  "RKLB",    0.116223,    8.00,        "Industrials",       "Space & Aerospace",
  "DORM",    0.045232,    5.00,        "Consumer Cyclical", "Auto Parts",
  "BK",      0.011659,    1.60,        "Financials",        "Asset Management",
  "EVR",     0.019679,    7.00,        "Financials",        "Investment Banking",
  "GS",      0.010029,    9.14,        "Financials",        "Investment Banking",
  "ANZ.NZ",  0.218176,    10.00,       "Financials",        "Banking",
  "VAS.AX",  0.090456,    10.00,       "ETF",               "AU Equities",
  "V500.AX", 0.097511,    5.00,        "ETF",               "Intl Equities",
  "IXJ.AX",  0.036901,    5.00,        "ETF",               "Healthcare",
  "IVV.AX",  0.10558684,  7.00,        "ETF",               "US Equities",
  "VRTX",    0.019094,    8.40,        "Healthcare",        "Biotechnology"
  
)

latest_prices <- portfolio_data %>%
  tq_get(get = "stock.prices", from = Sys.Date() - 5) %>%
  group_by(ticker) %>%
  slice_tail(n = 1) %>% 
  select(ticker, close)
latest_prices



# Graphs ------------------------------------------------------------------

portfolio_plot_data <- portfolio_data %>% 
  mutate(
    avg_cost = total_cost / shares,
    last_close_price = map_dbl(ticker, ~as.numeric(last(Cl(get(.x))))),
    pct_diff_price = (last_close_price - avg_cost) / avg_cost * 100
  )
#AAPL

plot_ticker <- function(t, from = "2026-04-01") {
  getSymbols(t, from = from, to = Sys.Date(), auto.assign = TRUE)
  row <- portfolio_plot_data %>%  filter(ticker == t)
  chartSeries(get(t), theme = chartTheme("white"),
              name = sprintf("%s | Avg Cost: $%.2f (%+.1f%%)", t, row$avg_cost, row$pct_diff_price))
  addLines(h = row$avg_cost, col = "red")
}

plot_ticker("GOOGL")


# Sum of Connors Sharesies 2.0 --------------------------------------------
two_day_prices <- portfolio_data %>% 
  tq_get(get = "stock.prices", from = Sys.Date() - 10) %>% 
  group_by(ticker) %>% 
  slice_tail(n = 2) %>% 
  mutate(day = c("prev_close", "last_close")) %>% 
  select(ticker, day, close)

prices_wide <- two_day_prices %>% 
  pivot_wider(names_from = day, values_from = close)

portfolio_full <- portfolio_data %>% 
  left_join(prices_wide, by = "ticker") %>% 
  mutate(
    avg_cost = total_cost / shares, 
    current_worth   = shares * last_close,
    prev_worth      = shares * prev_close,
    total_gain_loss = current_worth - total_cost,
    total_pct       = (total_gain_loss / total_cost) * 100,
    day_gain_loss   = current_worth - prev_worth,
    day_pct         = (day_gain_loss / prev_worth) * 100,
    weight = current_worth / sum(current_worth),
  )

portfolio_summary <- portfolio_full %>% 
  summarise(
    total_cost        = sum(total_cost),
    current_value     = sum(current_worth),
    day_change        = sum(day_gain_loss),
    day_pct_change    = (sum(day_gain_loss) / sum(prev_worth)) * 100,
    total_change      = sum(total_gain_loss),
    total_pct_change  = (sum(total_gain_loss) / sum(total_cost)) * 100
  ) 

#Portfolio Performances

Connors_Sharesies_2.0_Performance <- portfolio_summary %>% 
  gt() %>% 
  tab_header(
    title    = md("**Connor's Sharesies 2.0**"),
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

Connor_Sharesies_2.0_Portfolio <- portfolio_full %>% 
  select(ticker, sector, total_cost, current_worth, day_gain_loss, day_pct, total_gain_loss, total_pct) %>% 
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

Connor_Sharesies_2.0_Portfolio

# Pie Charts --------------------------------------------------------------
portfolio_by_ticker <- portfolio_full %>% 
  mutate(ticker = reorder(ticker, current_worth)) %>% 
  ggplot(aes(x = 1, y = current_worth, fill = ticker)) +
  geom_col(color = "white") + 
  coord_polar(theta = "y") +
  theme_void() + 
  labs(title = "Portfolio Weight Distribution") + 
  geom_text(aes(label = scales::percent(current_worth/sum(current_worth), 
                                        accuracy = 0.1)),
            position = position_stack(vjust = 0.5))
portfolio_by_ticker

#Tickers Rest
Ticker_by_rest <- portfolio_full %>%
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
Ticker_by_rest

#Sector
portfolio_by_sector <- portfolio_full%>%
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
portfolio_by_sector
#Industry#Isectorndustry
portfolio_by_industry <- portfolio_full %>%
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
portfolio_by_industry

portfolio_by_ticker
ggsave("portfolio_by_ticker.png", width = 8, height = 6, dpi = 300)
Ticker_by_rest
ggsave("portfolio_by_ticker_grouped.png", width = 8, height = 6, dpi = 300)
portfolio_by_sector
ggsave("portfolio_by_sector.png", width = 8, height = 6, dpi = 300)
portfolio_by_industry
ggsave("portfolio_by_industry.png", width = 8, height = 6, dpi = 300)

# Analysis ----------------------------------------------------------------
# log returns matrix
tickers <- portfolio_full$ticker

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

ggplot(cor_long, aes(x = Ticker1, y = Ticker2, fill = Correlation)) +
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
hhi_ticker <- portfolio_full %>% 
  summarise(HHI = sum(weight^2),
            `Equivalent N` = round(1 / sum(weight^2))) %>% 
  mutate(Level = case_when(
    HHI < 0.10 ~ "Diversified",
    HHI < 0.18 ~ "Moderate",
    TRUE        ~ "Concentrated"
  ))
print(hhi_ticker)

# HHI by sector
hhi_sector <- portfolio_full %>% 
  group_by(sector) %>% 
  summarise(sector_weight = sum(weight), .groups = "drop") %>% 
  summarise(HHI = sum(sector_weight^2),
            `Equivalent N` = round(1 / sum(sector_weight^2))) %>% 
  mutate(Level = case_when(
    HHI < 0.10 ~ "Diversified",
    HHI < 0.18 ~ "Moderate",
    TRUE        ~ "Concentrated"
  ))
print(hhi_sector)

# Per-sector weight breakdown
portfolio_full %>% 
  group_by(sector) %>% 
  summarise(weight = sum(weight), .groups = "drop") %>% 
  mutate(pct = scales::percent(weight, accuracy = 0.1)) %>% 
  arrange(desc(weight)) 


# Realized Gains ----------------------------------------------------------

portfolio_realised <- tibble(
  ticker = c("PL","AAOI","CRCL","IREN","SVCO","TTMI","AXTI","IONQ","YSS"),
  shares = c(0.328854,0.071638, 0.080729,0.113455,0.380119, 0.041414,0.137909,
             0.242995,0.11323318),
  total_cost = c(12,8,8,5,3.06,5.1,6.9,7,4.50),
  sector = c("Industrial","Technology","Financial Serivces","Finacial services",
             "Technology","Technology","Technology","Technology","Industrial"),
  industry = c("Aerospace & defence", "Communication equipment","Capital markets",
               "Capital markets","Software - application","Electronic components",
               "Semiconductor equipment & materials","Computer hardware","Aerospace & defence"),
  sold = c(10.795,10.304,9.120,6.330,4.329,4.730,8.666,10.419,3.79),
  return = sold - total_cost,
  pct_return = (return / total_cost) * 100
)
View(portfolio_realised)

portfolio_realised_summary <- portfolio_realised %>% 
  summarise(
    total_cost        = sum(total_cost),
    total_sold        = sum(sold),
    total_change      = sum(total_sold)-sum(total_cost),
    total_pct_change  = (sum(total_change) / sum(total_cost)) * 100
  ) 
View(portfolio_realised_summary)

#Realised_Gains_Portfolio
Realised_Gains_Portfolio <- portfolio_realised %>% 
  select(ticker, sector, total_cost, sold, return, pct_return) %>% 
  arrange(desc(pct_return)) %>% 
  gt() %>% 
  tab_header(
    title    = md("**Connor's Sharesies 2.0**"),
    subtitle = "Realised Gains — Per Stock"
  ) %>% 
  cols_label(
    ticker      = "Ticker",
    sector      = "Sector",
    total_cost  = "Cost",
    sold        = "Sold For",
    return      = "Return ($)",
    pct_return  = "Return (%)"
  ) %>% 
  fmt_currency(columns = c(total_cost, sold, return)) %>% 
  fmt_number(columns = pct_return, decimals = 1, pattern = "{x}%") %>% 
  tab_style(
    style = cell_text(color = "forestgreen", weight = "bold"),
    locations = cells_body(columns = c(return, pct_return), rows = return > 0)
  ) %>% 
  tab_style(
    style = cell_text(color = "red", weight = "bold"),
    locations = cells_body(columns = c(return, pct_return), rows = return < 0)
  ) %>% 
  tab_style(
    style = cell_text(color = "white", weight = "bold"),
    locations = cells_column_labels()
  ) %>% 
  tab_options(
    table.font.size                 = 13,
    heading.background.color        = "#084C61",
    heading.title.font.size         = 18,
    heading.subtitle.font.size      = 13,
    column_labels.background.color  = "#177E89",
    column_labels.font.weight       = "bold",
    row.striping.include_table_body = TRUE,
    row.striping.background_color   = "#f7faff"
  )

Realised_Gains_Portfolio


# Realized Gains summary
Realised_Gains_Summary <- portfolio_realised_summary %>% 
  gt() %>% 
  tab_header(
    title    = md("**Connor's Sharesies 2.0**"),
    subtitle = "Realised Gains — Summary"
  ) %>% 
  cols_label(
    total_cost       = "Total Cost",
    total_sold       = "Total Sold For",
    total_change     = "Return ($)",
    total_pct_change = "Return (%)"
  ) %>% 
  fmt_currency(columns = c(total_cost, total_sold, total_change)) %>% 
  fmt_number(columns = total_pct_change, decimals = 2, pattern = "{x}%") %>% 
  tab_style(
    style = cell_text(color = "forestgreen", weight = "bold"),
    locations = cells_body(columns = c(total_change, total_pct_change), rows = total_change > 0)
  ) %>% 
  tab_style(
    style = cell_text(color = "red", weight = "bold"),
    locations = cells_body(columns = c(total_change, total_pct_change), rows = total_change < 0)
  ) %>% 
  tab_style(
    style = cell_text(color = "white", weight = "bold"),
    locations = cells_column_labels()
  ) %>% 
  tab_options(
    table.font.size                 = 14,
    heading.background.color        = "#323031",
    heading.title.font.size         = 18,
    heading.subtitle.font.size      = 13,
    column_labels.background.color  = "#FFC857",
    column_labels.font.weight       = "bold",
    row.striping.include_table_body = TRUE,
    row.striping.background_color   = "#f0f4ff"
  )

Realised_Gains_Summary



# Sector Performance analysis ---------------------------------------------

portfolio_full %>%  head(20)

# Total portfolio value
total_worth <- sum(portfolio_full$current_worth)
total_cost  <- sum(portfolio_full$total_cost)

sector_contrib <- portfolio_full %>% 
  group_by(sector) %>% 
  summarise(
    tickers        = paste(ticker, collapse = ", "),
    n_holdings     = n(),
    sector_cost    = sum(total_cost),
    sector_worth   = sum(current_worth),
    sector_weight  = sector_worth / total_worth,
    day_gain       = sum(day_gain_loss),
    total_gain     = sum(total_gain_loss),
    .groups = "drop"
  ) %>% 
  mutate(
    sector_return_total = (sector_worth - sector_cost) / sector_cost,
    # Contribution = sector weight * sector total return
    contribution_total  = sector_weight * sector_return_total,
    # Day contribution = day gain / total portfolio worth
    contribution_day    = day_gain / total_worth
  ) %>% 
  arrange(desc(sector_weight))

sector_contrib

#Sector Performance Contribution

portfolio_total_return <- (total_worth - total_cost) / total_cost

Sector_performance_contribution <- sector_contrib %>% 
  select(sector, n_holdings, sector_weight, sector_return_total, contribution_total, day_gain, contribution_day) %>% 
  gt() %>% 
  tab_header(
    title    = md("**Sector Performance Contribution**"),
    subtitle = glue::glue("Total Portfolio Return: {scales::percent(portfolio_total_return, accuracy = 0.01)}")
  ) %>% 
  cols_label(
    sector              = "Sector",
    n_holdings          = "Holdings",
    sector_weight       = "Weight",
    sector_return_total = "Sector Return",
    contribution_total  = "Contribution to Return",
    day_gain            = "Day Gain ($)",
    contribution_day    = "Day Contribution"
  ) %>% 
  fmt_percent(columns = c(sector_weight, sector_return_total, contribution_total, contribution_day), decimals = 2) %>% 
  fmt_currency(columns = day_gain, currency = "USD", decimals = 2) %>% 
  data_color(
    columns = contribution_total,
    method  = "numeric",
    palette = c("#d73027", "#ffffbf", "#1a9850")
  ) %>% 
  data_color(
    columns = contribution_day,
    method  = "numeric",
    palette = c("#d73027", "#ffffbf", "#1a9850")
  ) %>% 
  tab_footnote(
    footnote = "Contribution = Sector Weight × Sector Return",
    locations = cells_column_labels(contribution_total)
  ) %>% 
  tab_options(table.font.size = 13)

Sector_performance_contribution

# Waterfall-style contribution chart

sector_contrib_plot <- sector_contrib %>%
  mutate(
    sector   = fct_reorder(sector, contribution_total),
    fill_col = if_else(contribution_total >= 0, "Positive", "Negative")
  )

sector_contrib_plot <- ggplot(sector_contrib_plot, aes(x = sector, y = contribution_total, fill = fill_col)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey40") +
  geom_text(
    aes(
      label = scales::percent(contribution_total, accuracy = 0.01),
      vjust = if_else(contribution_total >= 0, -0.4, 1.3)
    ),
    size = 3.5, fontface = "bold"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_fill_manual(values = c("Positive" = "#1a9850", "Negative" = "#d73027"), guide = "none") +
  labs(
    title    = "Sector Contribution to Total Portfolio Return",
    subtitle = glue::glue("Total portfolio return: {scales::percent(portfolio_total_return, accuracy = 0.01)}"),
    x = NULL,
    y = "Contribution to Return"
  ) +
  theme_minimal(base_size = 13)

sector_contrib_plot

# Ticker-level contribution to total portfolio return
ticker_contrib <- portfolio_full %>% 
  mutate(
    ticker_weight        = current_worth / total_worth,
    ticker_return        = (current_worth - total_cost) / total_cost,
    contribution_total   = ticker_weight * ticker_return
  ) %>% 
  arrange(sector, desc(contribution_total)) %>% 
  select(sector, ticker, ticker_weight, ticker_return, contribution_total, day_gain_loss, total_gain_loss)

ticker_level_contribution <- ticker_contrib %>% 
  gt(groupname_col = "sector") %>% 
  tab_header(
    title    = md("**Ticker-Level Contribution to Portfolio Return**"),
    subtitle = glue::glue("Total Portfolio Return: {scales::percent(portfolio_total_return, accuracy = 0.01)}")
  ) %>% 
  cols_label(
    ticker             = "Ticker",
    ticker_weight      = "Weight",
    ticker_return      = "Return",
    contribution_total = "Contribution",
    day_gain_loss      = "Day P&L ($)",
    total_gain_loss    = "Total P&L ($)"
  ) %>% 
  fmt_percent(columns = c(ticker_weight, ticker_return, contribution_total), decimals = 2) |>
  fmt_currency(columns = c(day_gain_loss, total_gain_loss), currency = "USD", decimals = 2) |>
  data_color(
    columns = contribution_total,
    method  = "numeric",
    palette = c("#d73027", "#ffffbf", "#1a9850")
  ) %>% 
  data_color(
    columns = ticker_return,
    method  = "numeric",
    palette = c("#d73027", "#ffffbf", "#1a9850")
  ) %>% 
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>% 
  tab_options(table.font.size = 12, row_group.background.color = "#f0f0f0")
ticker_level_contribution


# Beta Analysis -----------------------------------------------------------

# Portfolio weights (from portfolio_full in session)
portfolio_weights <- portfolio_full %>% 
  select(ticker, weight, sector)
all_tickers <- portfolio_weights$ticker
from_date <- "2025-05-01"
to_date   <- Sys.Date()
market_returns <- tq_get("SPY", get = "stock.prices", from = from_date, to = to_date) %>% 
  tq_transmute(select = adjusted, mutate_fun = periodReturn,
              period = "daily", col_rename = "mkt_return")
stock_returns <- tq_get(all_tickers, get = "stock.prices", from = from_date, to = to_date) %>% 
  group_by(symbol) %>% 
  tq_transmute(select = adjusted, mutate_fun = periodReturn,
               period = "daily", col_rename = "stk_return")
head(stock_returns)

#Individual & Portfolio Beta Calculation

# !Note: ANZ.NZ, VAS.AX, IXJ.AX, IVV.AX, V500.AX trade in NZD/AUD;
# !their betas vs SPY include currency noise — interpret with caution.

beta_data <- stock_returns %>% 
  left_join(market_returns, by = "date") %>% 
  filter(!is.na(mkt_return), !is.na(stk_return), stk_return != 0 | mkt_return != 0)

individual_betas <- beta_data %>% 
  group_by(symbol) %>% 
  summarise(
    beta      = cov(stk_return, mkt_return) / var(mkt_return),
    r_squared = cor(stk_return, mkt_return)^2,
    n_obs     = n(),
    .groups   = "drop"
  ) %>% 
  left_join(portfolio_weights %>%  rename(symbol = ticker), by = "symbol") %>% 
  arrange(desc(beta))

# Weighted portfolio beta
portfolio_beta <- sum(individual_betas$beta * individual_betas$weight, na.rm = TRUE)

#Beta Table

Beta_Table <- individual_betas %>% 
  select(symbol, sector, beta, r_squared, weight) %>% 
  gt() %>% 
  tab_header(
    title    = md("**Portfolio Beta Analysis**"),
    subtitle = glue::glue("vs S&P 500 (SPY) | Portfolio Beta: {round(portfolio_beta, 3)}")
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

Beta_Table

#Beta Bar Chart 

Beta_bar_chart <- ggplot(individual_betas, aes(x = reorder(symbol, beta), y = beta, fill = sector)) +
  geom_col() +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey40") +
  geom_hline(yintercept = portfolio_beta, linetype = "solid", colour = "black", linewidth = 0.8) +
  annotate("text", x = 1.5, y = portfolio_beta + 0.08,
           label = paste("Portfolio β =", round(portfolio_beta, 2)),
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

Beta_bar_chart
gtsave(Beta_Table, "Beta_Analysis_Table.png", expand = 10)

#Contribution to portfolio beta analysis

beta_contrib <- individual_betas %>% 
  mutate(
    beta_contribution = weight * beta,
    fill_col = if_else(beta_contribution >= 0, "Positive", "Negative")
  ) %>% 
  arrange(desc(beta_contribution))

# Table
Beta_Contribution_Table <- beta_contrib %>% 
  select(symbol, sector, weight, beta, beta_contribution) %>% 
  gt() %>% 
  tab_header(
    title    = md("**Contribution to Portfolio Beta**"),
    subtitle = glue::glue("Portfolio Beta: {round(portfolio_beta, 3)} | Benchmark: SPY")
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

Beta_Contribution_Table

# Chart
Beta_contribution_chart <- ggplot(beta_contrib, aes(x = reorder(symbol, beta_contribution), y = beta_contribution, fill = fill_col)) +
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
    subtitle = glue::glue("Portfolio Beta: {round(portfolio_beta, 3)}"),
    x = NULL, y = "Beta Contribution"
  ) +
  coord_flip() +
  theme_minimal(base_size = 13)
Beta_contribution_chart
# Specific Date Portfolios --------------------------------------------------



