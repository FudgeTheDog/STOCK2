
# Risk Analysis ---------------------------------------------------------------
# Covers: VaR/CVaR, Drawdown, Sharpe/Sortino, Volatility Decomposition
# Reuses: stock_returns, portfolio_weights, portfolio_full from Beta_Analysis_Source

if (!exists("stock_returns") || !exists("portfolio_weights")) {
  source(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "Beta_Analysis_Source.R"))
}

# Risk-free rate — adjust to current 3-month T-bill rate if needed
RISK_FREE_ANNUAL <- 0.043
RISK_FREE_DAILY  <- RISK_FREE_ANNUAL / 252

# ── Portfolio daily returns (weighted) ───────────────────────────────────────
portfolio_daily_returns <- stock_returns %>%
  left_join(portfolio_weights %>% rename(symbol = ticker), by = "symbol") %>%
  filter(!is.na(weight)) %>%
  group_by(date) %>%
  summarise(port_return = sum(stk_return * weight, na.rm = TRUE), .groups = "drop") %>%
  arrange(date)

# ── 1. VaR & CVaR ─────────────────────────────────────────────────────────────
port_value <- sum(portfolio_full$current_worth, na.rm = TRUE)

var_95  <- quantile(portfolio_daily_returns$port_return, 0.05)
var_99  <- quantile(portfolio_daily_returns$port_return, 0.01)
cvar_95 <- mean(portfolio_daily_returns$port_return[portfolio_daily_returns$port_return <= var_95])
cvar_99 <- mean(portfolio_daily_returns$port_return[portfolio_daily_returns$port_return <= var_99])

VaR_Table <- tibble(
  Metric     = c("VaR (95%)", "VaR (99%)", "CVaR (95%)", "CVaR (99%)"),
  Daily_pct  = c(var_95, var_99, cvar_95, cvar_99),
  Daily_usd  = Daily_pct * port_value,
  Annual_pct = Daily_pct * sqrt(252)
) %>%
  gt() %>%
  tab_header(
    title    = md("**Portfolio Value at Risk**"),
    subtitle = glue::glue("Historical simulation | Portfolio value: ${round(port_value, 2)}")
  ) %>%
  cols_label(
    Metric     = "Metric",
    Daily_pct  = "Daily (%)",
    Daily_usd  = "Daily ($)",
    Annual_pct = "Annualised (%)"
  ) %>%
  fmt_percent(columns = c(Daily_pct, Annual_pct), decimals = 2) %>%
  fmt_currency(columns = Daily_usd, decimals = 2) %>%
  tab_style(
    style     = cell_text(color = "#c0392b", weight = "bold"),
    locations = cells_body()
  ) %>%
  tab_footnote(
    footnote  = "VaR: worst expected daily loss at confidence level. CVaR: average loss on days beyond VaR (tail risk).",
    locations = cells_column_labels(Metric)
  ) %>%
  tab_options(
    table.font.size                = 13,
    heading.background.color       = "#1a1a2e",
    heading.title.font.size        = 18,
    column_labels.background.color = "#16213e",
    column_labels.font.weight      = "bold",
    row.striping.include_table_body = TRUE,
    row.striping.background_color  = "#fff0f0"
  )

# ── 2. Drawdown Chart ─────────────────────────────────────────────────────────
drawdown_df <- portfolio_daily_returns %>%
  mutate(
    cum_return  = cumprod(1 + port_return),
    running_max = cummax(cum_return),
    drawdown    = (cum_return - running_max) / running_max
  )

max_drawdown <- min(drawdown_df$drawdown)
max_dd_date  <- drawdown_df$date[which.min(drawdown_df$drawdown)]

drawdown_chart <- ggplot(drawdown_df, aes(x = date, y = drawdown)) +
  geom_area(fill = "#e74c3c", alpha = 0.25) +
  geom_line(colour = "#e74c3c", linewidth = 0.9) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey40") +
  annotate("point", x = max_dd_date, y = max_drawdown, colour = "#c0392b", size = 3) +
  annotate("label",
           x = max_dd_date, y = max_drawdown,
           label = paste0("Max DD: ", scales::percent(max_drawdown, accuracy = 0.1)),
           colour = "#c0392b", fill = "white", fontface = "bold",
           size = 3.5, vjust = 1.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b '%y") +
  labs(
    title    = "Portfolio Drawdown Over Time",
    subtitle = paste0("Max drawdown: ",
                      scales::percent(max_drawdown, accuracy = 0.1),
                      " (", max_dd_date, ")"),
    x = NULL, y = "Drawdown from Peak"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold"),
    axis.text.x      = element_text(angle = 30, hjust = 1),
    panel.grid.minor = element_blank()
  )

# ── 3. Sharpe & Sortino per stock ─────────────────────────────────────────────
risk_metrics <- stock_returns %>%
  group_by(symbol) %>%
  summarise(
    mean_daily  = mean(stk_return, na.rm = TRUE),
    sd_daily    = sd(stk_return, na.rm = TRUE),
    # Downside deviation: std dev of returns below the risk-free rate
    down_sd     = sd(pmin(stk_return - RISK_FREE_DAILY, 0), na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  mutate(
    ann_return  = mean_daily * 252,
    ann_vol     = sd_daily * sqrt(252),
    sharpe      = (ann_return - RISK_FREE_ANNUAL) / ann_vol,
    sortino     = (ann_return - RISK_FREE_ANNUAL) / (down_sd * sqrt(252))
  ) %>%
  left_join(portfolio_weights %>% rename(symbol = ticker), by = "symbol") %>%
  filter(!is.na(weight)) %>%
  arrange(desc(sharpe))

# Portfolio-level Sharpe & Sortino
port_mean  <- mean(portfolio_daily_returns$port_return)
port_sd    <- sd(portfolio_daily_returns$port_return)
port_down  <- sd(pmin(portfolio_daily_returns$port_return - RISK_FREE_DAILY, 0))
port_sharpe  <- (port_mean * 252 - RISK_FREE_ANNUAL) / (port_sd * sqrt(252))
port_sortino <- (port_mean * 252 - RISK_FREE_ANNUAL) / (port_down * sqrt(252))

Risk_Table <- risk_metrics %>%
  dplyr::select(symbol, sector, ann_return, ann_vol, sharpe, sortino) %>%
  gt() %>%
  tab_header(
    title    = md("**Risk-Adjusted Return Metrics**"),
    subtitle = glue::glue(
      "Portfolio — Sharpe: {round(port_sharpe, 2)} | Sortino: {round(port_sortino, 2)} | Rf: {RISK_FREE_ANNUAL * 100}%"
    )
  ) %>%
  cols_label(
    symbol     = "Ticker",
    sector     = "Sector",
    ann_return = "Ann. Return",
    ann_vol    = "Ann. Volatility",
    sharpe     = "Sharpe",
    sortino    = "Sortino"
  ) %>%
  fmt_percent(columns = c(ann_return, ann_vol), decimals = 1) %>%
  fmt_number(columns = c(sharpe, sortino), decimals = 2) %>%
  data_color(
    columns = sharpe,
    method  = "numeric",
    palette = c("#d73027", "#ffffbf", "#1a9850")
  ) %>%
  tab_footnote(
    footnote  = "Sharpe = excess return / total volatility. Sortino = excess return / downside deviation only.",
    locations = cells_column_labels(sharpe)
  ) %>%
  tab_options(
    table.font.size                = 13,
    heading.background.color       = "#1a1a2e",
    heading.title.font.size        = 18,
    column_labels.background.color = "#16213e",
    column_labels.font.weight      = "bold",
    row.striping.include_table_body = TRUE,
    row.striping.background_color  = "#f0f4ff"
  )

# ── 4. Volatility Decomposition ───────────────────────────────────────────────
# Component contribution to risk via Euler decomposition:
# CC_i = w_i * (Σw)_i / port_vol  →  sums to port_vol
# As a % of total: CC_i / port_vol  →  sums to 1

returns_wide <- stock_returns %>%
  filter(symbol %in% portfolio_weights$ticker) %>%
  pivot_wider(names_from = symbol, values_from = stk_return) %>%
  arrange(date) %>%
  dplyr::select(-date)

# Drop tickers with too little data
returns_wide <- returns_wide[, colSums(!is.na(returns_wide)) > 20]

cov_mat <- cov(returns_wide, use = "pairwise.complete.obs")

w_aligned <- portfolio_weights %>%
  filter(ticker %in% colnames(cov_mat)) %>%
  arrange(match(ticker, colnames(cov_mat)))

w_vec    <- w_aligned$weight / sum(w_aligned$weight)
port_var <- as.numeric(t(w_vec) %*% cov_mat %*% w_vec)
port_vol <- sqrt(port_var)

marg_contrib <- as.numeric(cov_mat %*% w_vec) / port_vol
comp_contrib <- w_vec * marg_contrib          # sums to port_vol
pct_contrib  <- comp_contrib / port_vol       # sums to 1

vol_decomp <- tibble(
  symbol       = w_aligned$ticker,
  sector       = w_aligned$sector,
  weight       = w_vec,
  vol_contrib  = pct_contrib
) %>%
  arrange(desc(vol_contrib))

vol_decomp_chart <- ggplot(vol_decomp,
                           aes(x = reorder(symbol, vol_contrib),
                               y = vol_contrib,
                               fill = sector)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey40") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title    = "Volatility Contribution by Stock",
    subtitle = glue::glue(
      "Annualised portfolio volatility: {scales::percent(port_vol * sqrt(252), accuracy = 0.1)}"
    ),
    x = NULL, y = "% of Portfolio Volatility", fill = "Sector"
  ) +
  coord_flip() +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
