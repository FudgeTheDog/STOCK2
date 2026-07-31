#Back Up

sim_tickers <- Tar_portfolio_weights$ticker

prices_long <- tq_get(
  sim_tickers,
  from = "2022-05-01",
  to   = format(Sys.Date()),
  get  = "stock.prices"
)

# --- Build returns matrix -----------------------------------------------------
# V500.AX excluded (only ~6 weeks of history); its weight is merged into IVV.AX
returns_sim <- prices_long %>% 
  dplyr::group_by(symbol) %>% 
  dplyr::arrange(date) %>% 
  dplyr::mutate(daily_return = adjusted / lag(adjusted) - 1) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(date, symbol, daily_return) %>% 
  pivot_wider(names_from = symbol, values_from = daily_return) %>% 
  dplyr::arrange(date)

returns_sim <- returns_sim %>%
  dplyr::select(where(~ mean(!is.na(.x)) > 0.8)) %>% 
  dplyr::select(-date) %>% 
  drop_na()