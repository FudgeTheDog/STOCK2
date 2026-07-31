if (!exists("portfolio_full")) {
  source(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "Connor_Sharesies_Source.R"))
}
# Note on FX: portfolio weights are computed in USD (prices converted in Connor_Sharesies_Source.R).
# However, the daily returns fed into beta calculations are still in local currency
# (NZD for ANZ.NZ, AUD for *.AX) — Yahoo Finance returns raw local prices via tq_get.
# Betas and correlations for these tickers therefore include currency noise vs SPY.
# Beta Analysis -----------------------------------------------------------

# Portfolio weights (from portfolio_full in session)
# Recalculate weights from priced holdings only (portfolio_full weights are NA if any price is missing)
portfolio_weights <- portfolio_full %>%
  filter(!is.na(current_worth)) %>%
  mutate(weight = current_worth / sum(current_worth)) %>% 
  dplyr::select(ticker, weight, sector)
all_tickers <- portfolio_weights$ticker
from_date <- "2025-05-01"
to_date   <- Sys.Date()

market_returns <- tq_get(
  "SPY",
  get = "stock.prices",
  from = from_date,
  to = to_date
) %>%
  filter(!is.na(adjusted)) %>%
  arrange(date) %>%
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "daily",
    col_rename = "mkt_return"
  )


stock_prices <- purrr::map_dfr(all_tickers, function(tk) {
  tryCatch(
    tq_get(tk, get = "stock.prices", from = from_date, to = to_date) %>%
      dplyr::mutate(symbol = tk),
    error = function(e) {
      message("Failed to fetch: ", tk, " — ", e$message)
      tibble::tibble()
    }
  )
})

stock_returns <- stock_prices %>%
  dplyr::filter(!is.na(adjusted)) %>%
  dplyr::group_by(symbol) %>%
  dplyr::arrange(date) %>%
  tq_transmute(
    select = adjusted,
    mutate_fun = periodReturn,
    period = "daily",
    col_rename = "stk_return"
  ) %>%
  dplyr::ungroup()

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
  dplyr::select(symbol, sector, beta, r_squared, weight) %>% 
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

#Contribution to portfolio beta analysis

beta_contrib <- individual_betas %>% 
  mutate(
    beta_contribution = weight * beta,
    fill_col = if_else(beta_contribution >= 0, "Positive", "Negative")
  ) %>% 
  arrange(desc(beta_contribution))

# Table
Beta_Contribution_Table <- beta_contrib %>% 
  dplyr::select(symbol, sector, weight, beta, beta_contribution) %>% 
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
