if (!exists("portfolio_full")) {
  source(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "Connor_Sharesies_Source.R"))
}
# Realized Gains ----------------------------------------------------------

portfolio_realised <- tibble(
  ticker = c("PL","AAOI","CRCL","IREN","SVCO","TTMI","AXTI","IONQ","YSS","V500.AX",
             "ANZ.NZ","VAS.AX","IXJ.AX","GE","ESE","QBTS","IONQ","TSM","AXTI"),
  shares = c(0.328854,0.071638, 0.080729,0.113455,0.380119, 0.041414,0.137909,
             0.242995,0.11323318,0.097511,0.218176,0.090456, 0.036901, 0.021907,0.019189,
             0.40701328,0.13990793,0.00945007,0.03105551),
  total_cost = c(12,8,8,5,3.06,5.1,6.9,7,4.50,5.00,10.00,10.00,5.00,7.00,6.01,9.00,7.00,3.90,3.50),
  sector = c("Industrial","Technology","Financial Serivces","Finacial services",
             "Technology","Technology","Technology","Technology","Industrial","ETF",
             "Financials","ETF","ETF","Industrials","Industrials","Technology","Technology","Technology",
             "Technology"),
  industry = c("Aerospace & defence", "Communication equipment","Capital markets",
               "Capital markets","Software - application","Electronic components",
               "Semiconductor equipment & materials","Computer hardware","Aerospace & defence",
               "Intl Equities","Banking","AU Equities","Healthcare","Aerospace & Defense",
               "Defense Electronics","Quantum Computing","Quantum Computing","Semiconductors",
               "Semiconductors"),
  currency = c("USD","USD","USD","USD","USD","USD","USD","USD","USD","AUD","NZD",
               "AUD","AUD","USD","USD","USD","USD","USD","USD"),
  sold = c(10.795,10.304,9.120,6.330,4.329,4.730,8.666,10.419,3.79,5.12,9.21,9.65,
           4.71,6.22,5.59,10.83,8.17,4.16,1.93),
  return = sold - total_cost,
  pct_return = (return / total_cost) * 100
)



# Summary grouped by currency — avoids mixing USD / AUD / NZD into a single misleading total
portfolio_realised_summary <- portfolio_realised %>%
  group_by(currency) %>%
  summarise(
    total_cost        = sum(total_cost),
    total_sold        = sum(sold),
    total_change      = total_sold - total_cost,
    total_pct_change  = (total_change / total_cost) * 100,
    .groups = "drop"
  )

# Combine rows for the same ticker (e.g. sold in multiple tranches)
portfolio_realised_by_ticker <- portfolio_realised %>% 
  group_by(ticker, currency) %>% 
  summarise(
    sector     = first(sector),
    industry   = first(industry),
    shares     = sum(shares),
    total_cost = sum(total_cost),
    sold       = sum(sold),
    .groups    = "drop"
  ) |>
  mutate(
    return     = sold - total_cost,
    pct_return = (return / total_cost) * 100
  )

#Realised_Gains_Portfolio
Realised_Gains_Portfolio <- portfolio_realised_by_ticker %>% 
  dplyr::select(ticker, sector, currency, total_cost, sold, return, pct_return) %>% 
  arrange(desc(pct_return)) %>% 
  gt() %>% 
  tab_header(
    title    = md("**Connor's Sharesies 2.0**"),
    subtitle = "Realised Gains — Per Stock"
  ) %>% 
  cols_label(
    ticker      = "Ticker",
    sector      = "Sector",
    currency    = "Ccy",
    total_cost  = "Cost",
    sold        = "Sold For",
    return      = "Return ($)",
    pct_return  = "Return (%)"
  ) %>% 
  fmt_currency(columns = c(total_cost, sold, return), currency = "USD", rows = currency == "USD") %>%
  fmt_currency(columns = c(total_cost, sold, return), currency = "AUD", rows = currency == "AUD") %>%
  fmt_currency(columns = c(total_cost, sold, return), currency = "NZD", rows = currency == "NZD") %>%
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

# Realized Gains summary
Realised_Gains_Summary <- portfolio_realised_summary %>% 
  gt() %>% 
  tab_header(
    title    = md("**Connor's Sharesies 2.0**"),
    subtitle = "Realised Gains — Summary"
  ) %>% 
  cols_label(
    currency         = "Currency",
    total_cost       = "Total Cost",
    total_sold       = "Total Sold For",
    total_change     = "Return ($)",
    total_pct_change = "Return (%)"
  ) %>% 
  fmt_currency(columns = c(total_cost, total_sold, total_change), currency = "USD", rows = currency == "USD") %>%
  fmt_currency(columns = c(total_cost, total_sold, total_change), currency = "AUD", rows = currency == "AUD") %>%
  fmt_currency(columns = c(total_cost, total_sold, total_change), currency = "NZD", rows = currency == "NZD") %>%
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
