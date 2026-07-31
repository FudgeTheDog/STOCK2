if (!exists("portfolio_full")) {
  source(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "Connor_Sharesies_Source.R"))
}
# Sector Performance analysis ---------------------------------------------

# Exclude tickers with no current price so cost and worth are always comparable
portfolio_priced <- portfolio_full %>% filter(!is.na(current_worth))

total_worth <- sum(portfolio_priced$current_worth)
total_cost  <- sum(portfolio_priced$total_cost)-COST_ADJUSTMENT

sector_contrib <- portfolio_priced %>% 
  group_by(sector) %>% 
  summarise(
    tickers        = paste(ticker, collapse = ", "),
    n_holdings     = n(),
    sector_cost    = sum(total_cost),
    sector_worth   = sum(current_worth),
    sector_weight  = sector_worth / total_worth,
    day_gain       = sum(day_gain_loss,   na.rm = TRUE),
    total_gain     = sum(total_gain_loss, na.rm = TRUE),
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
#Sector Performance Contribution
portfolio_total_return <- (total_worth - total_cost) / total_cost

Sector_performance_contribution <- sector_contrib %>% 
  dplyr::select(sector, n_holdings, sector_weight, sector_return_total, contribution_total, day_gain, contribution_day) %>% 
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

# Ticker-level contribution to total portfolio return
ticker_contrib <- portfolio_full %>% 
  mutate(
    ticker_weight        = current_worth / total_worth,
    ticker_return        = (current_worth - total_cost) / total_cost,
    contribution_total   = ticker_weight * ticker_return
  ) %>% 
  arrange(sector, desc(contribution_total)) %>% 
  dplyr::select(sector, ticker, ticker_weight, ticker_return, contribution_total, day_gain_loss, total_gain_loss)

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
