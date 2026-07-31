
if (!exists("portfolio_data") || !exists("fx_rates")) {
  source(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "Portfolio_Data_Source.R"))
}
# Sum of Connors Sharesies 2.0 --------------------------------------------
# --- Daily historical prices: used mainly for AUD tickers ----------------------
daily_prices <- portfolio_data %>%
  tq_get(get = "stock.prices", from = Sys.Date() - 10) %>%
  dplyr::filter(!is.na(close)) %>%
  dplyr::group_by(ticker) %>%
  dplyr::arrange(date) %>%
  dplyr::slice_tail(n = 2) %>%
  dplyr::mutate(day = c("prev_close_hist", "last_close_hist")) %>%
  dplyr::ungroup() %>%
  dplyr::select(ticker, day, close) %>%
  tidyr::pivot_wider(names_from = day, values_from = close)

# --- Latest quote: used mainly for USD tickers --------------------------------
latest_quote <- suppressWarnings(
  quantmod::getQuote(portfolio_data$ticker)
) %>%
  tibble::rownames_to_column("ticker") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    ticker = stringr::str_trim(ticker),
    quote_last_close = Last,
    quote_prev_close = Last - Change
  ) %>%
  dplyr::select(ticker, quote_last_close, quote_prev_close)

# --- Final price table --------------------------------------------------------
prices_wide <- daily_prices %>%
  dplyr::left_join(latest_quote, by = "ticker") %>%
  dplyr::left_join(
    portfolio_data %>% dplyr::select(ticker, currency),
    by = "ticker"
  ) %>%
  dplyr::left_join(fx_rates, by = "currency") %>%
  dplyr::mutate(
    fx_to_usd = replace_na(fx_to_usd, 1.0),
    
    prev_close_raw = dplyr::case_when(
      currency == "AUD" ~ prev_close_hist,
      TRUE              ~ quote_prev_close
    ),
    
    last_close_raw = dplyr::case_when(
      currency == "AUD" ~ last_close_hist,
      TRUE              ~ quote_last_close
    ),
    
    prev_close = prev_close_raw * fx_to_usd,
    last_close = last_close_raw * fx_to_usd
  ) %>%
  dplyr::select(ticker, prev_close, last_close)

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
    weight          = current_worth / sum(current_worth, na.rm = TRUE)
  )

COST_ADJUSTMENT <- 24.64

portfolio_summary <- portfolio_full %>% 
  summarise(
    raw_total_cost   = sum(total_cost, na.rm = TRUE),
    total_cost       = raw_total_cost - COST_ADJUSTMENT,
    current_value    = sum(current_worth, na.rm = TRUE),
    prev_value       = sum(prev_worth, na.rm = TRUE),
    
    day_change       = current_value - prev_value,
    day_pct_change   = (day_change / prev_value) * 100,
    
    total_change     = current_value - total_cost,
    total_pct_change = (total_change / total_cost) * 100
  ) %>% 
  dplyr::select(
    total_cost,
    current_value,
    day_change,
    day_pct_change,
    total_change,
    total_pct_change
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


