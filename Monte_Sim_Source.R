# Monte Carlo Simulation -------------------------------------------------------
# Simulates portfolio value over 1 trading year (252 days) across 1,000 paths.
# Uses 3-year return history from Yahoo Finance via tidyquant.
# Correlations between assets are preserved via eigendecomposition of the
# covariance matrix. V500.AX (launched Mar 2026) is proxied by IVV.AX.
#
# Note on FX: start_val and portfolio weights are correctly in USD (via
# Connor_Sharesies_Source.R conversion). Historical returns for ANZ.NZ and
# *.AX tickers are still in local currency (NZD/AUD), so their simulated
# return distributions include currency noise rather than pure USD returns.

set.seed(4823)

# --- Parameters ---------------------------------------------------------------
n_sims    <- 10000
n_days    <- 252   # 1 trading year
start_val <- total_worth

# --- Fetch 3-year price history -----------------------------------------------
# --- Fetch 3-year price history -----------------------------------------------
sim_tickers <- portfolio_weights$ticker

prices_long <- tq_get(
  sim_tickers,
  from = "2022-05-01",
  to   = format(Sys.Date()),
  get  = "stock.prices"
)

# --- Fetch FX rates ------------------------------------------------------------
fx_wide <- tq_get(
  c("AUDUSD=X", "NZDUSD=X"),
  from = "2022-05-01",
  to   = format(Sys.Date()),
  get  = "stock.prices"
) %>%
  dplyr::select(date, symbol, fx_rate = adjusted) %>%
  tidyr::pivot_wider(names_from = symbol, values_from = fx_rate)

# --- Convert non-USD prices into USD ------------------------------------------
prices_long_usd <- prices_long %>%
  dplyr::left_join(fx_wide, by = "date") %>%
  dplyr::mutate(
    adjusted_usd = dplyr::case_when(
      stringr::str_detect(symbol, "\\.AX$") ~ adjusted * `AUDUSD=X`,
      stringr::str_detect(symbol, "\\.NZ$") ~ adjusted * `NZDUSD=X`,
      TRUE ~ adjusted
    )
  ) %>%
  dplyr::select(date, symbol, adjusted_usd)

# --- Build returns matrix -----------------------------------------------------
returns_sim <- prices_long_usd %>% 
  dplyr::group_by(symbol) %>% 
  dplyr::arrange(date) %>% 
  dplyr::mutate(daily_return = adjusted_usd / dplyr::lag(adjusted_usd) - 1) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(date, symbol, daily_return) %>% 
  tidyr::pivot_wider(names_from = symbol, values_from = daily_return) %>% 
  dplyr::arrange(date) %>% 
  dplyr::select(-date) %>% 
  dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) %>%
  na.omit()

# --- Align weights to returns_sim columns ------------------------------------
# Build w from portfolio_weights, keeping only tickers present in returns_sim
# and re-normalising so they sum to 1.
w_df <- portfolio_weights %>%
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
    subtitle = "1,000 simulations over 252 trading days (1 year) · Based on 3-year return history",
    x        = NULL,
    y        = "Portfolio Value",
    caption  = "Shaded bands: 5th–95th pctile (light), 25th–75th pctile (dark) · Line: median"
  ) +
  theme_minimal(base_size = 13)

# --- Distribution of final values ---------------------------------------------
final_df <- data.frame(final_value = final_vals)

mc_final_dist <- ggplot(final_df, aes(x = final_value)) +
  geom_histogram(binwidth = 20, fill = "#3B82F6", colour = "white", alpha = 0.85) +
  geom_vline(xintercept = start_val,                    linetype = "dashed", colour = "grey30",  linewidth = 0.8) +
  geom_vline(xintercept = median(final_vals),           linetype = "solid",  colour = "#1D4ED8", linewidth = 1) +
  geom_vline(xintercept = quantile(final_vals, 0.05),   linetype = "dotted", colour = "#EF4444", linewidth = 0.9) +
  annotate("text", x = start_val + 5,                       y = Inf, label = "Start",   vjust = 2, hjust = 0, size = 3.5, colour = "grey30") +
  annotate("text", x = median(final_vals) + 5,              y = Inf, label = "Median",  vjust = 2, hjust = 0, size = 3.5, colour = "#1D4ED8") +
  annotate("text", x = quantile(final_vals, 0.05) - 5,      y = Inf, label = "5th %ile", vjust = 2, hjust = 1, size = 3.5, colour = "#EF4444") +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(
    title    = "Distribution of Portfolio Values After 1 Year",
    subtitle = "10,000 Monte Carlo simulations",
    x        = "Final Portfolio Value",
    y        = "Count"
  ) +
  theme_minimal(base_size = 13)


###########################################################################
# --- 5 Year sim ---------------------------------------------------------------
n_days5 <- 1260

sim_paths5 <- matrix(NA_real_, nrow = n_days5 + 1, ncol = n_sims)
sim_paths5[1, ] <- start_val

for (sim in seq_len(n_sims)) {
  z       <- matrix(rnorm(n_days5 * length(mu_long)), nrow = n_days5)
  r_asset <- z %*% t(L_eigen) + matrix(mu_long, nrow = n_days5, ncol = length(mu_long), byrow = TRUE)
  r_port  <- as.vector(r_asset %*% w)
  sim_paths5[-1, sim] <- start_val * cumprod(1 + r_port)
}

# --- Summary statistics 5 -------------------------------------------------------
final_vals5 <- sim_paths5[n_days5 + 1, ]

mc_summary_data5 <- data.frame(
  scenario     = c("Starting Value", "5th Percentile (VaR 95%)", "25th Percentile",
                   "Median", "Mean", "75th Percentile", "95th Percentile",
                   "Probability of Loss", "Probability of >+50%"),
  final_value  = c(start_val,
                   quantile(final_vals5, 0.05), quantile(final_vals5, 0.25),
                   median(final_vals5), mean(final_vals5),
                   quantile(final_vals5, 0.75), quantile(final_vals5, 0.95),
                   NA, NA),
  return_pct   = c(0,
                   (quantile(final_vals5, 0.05) / start_val - 1) * 100,
                   (quantile(final_vals5, 0.25) / start_val - 1) * 100,
                   (median(final_vals5)         / start_val - 1) * 100,
                   (mean(final_vals5)           / start_val - 1) * 100,
                   (quantile(final_vals5, 0.75) / start_val - 1) * 100,
                   (quantile(final_vals5, 0.95) / start_val - 1) * 100,
                   mean(final_vals5 < start_val) * 100,
                   mean(final_vals5 > start_val * 1.5) * 100)
)

mc_summary_table5 <- mc_summary_data5 %>% 
  gt() %>% 
  tab_header(
    title    = "Monte Carlo Simulation — 5-Year Outlook",
    subtitle = "10,000 simulations · Based on 3-year return history"
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

# --- Fan chart 5 ----------------------------------------------------------------
days_seq5 <- 0:n_days5
pctiles5  <- c(0.05, 0.25, 0.50, 0.75, 0.95)

band_df5 <- pctiles5 %>% 
  purrr::map_dfc(\(p) apply(sim_paths5, 1, quantile, probs = p)) %>% 
  setNames(paste0("p", pctiles5 * 100)) %>% 
  dplyr::mutate(day = days_seq5)

set.seed(4821)
path_sample5 <- sim_paths5[, sample(n_sims, 100)] %>% 
  as.data.frame() %>% 
  dplyr::mutate(day = days_seq5) %>% 
  tidyr::pivot_longer(-day, names_to = "sim", values_to = "value")

mc_fan_chart5 <- ggplot() +
  geom_line(data = path_sample5, aes(x = day, y = value, group = sim),
            colour = "#3B82F6", alpha = 0.06, linewidth = 0.3) +
  geom_ribbon(data = band_df5, aes(x = day, ymin = p5, ymax = p95),
              fill = "#3B82F6", alpha = 0.15) +
  geom_ribbon(data = band_df5, aes(x = day, ymin = p25, ymax = p75),
              fill = "#3B82F6", alpha = 0.25) +
  geom_line(data = band_df5, aes(x = day, y = p50),
            colour = "#1D4ED8", linewidth = 1) +
  geom_hline(yintercept = start_val, linetype = "dashed", colour = "grey40") +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(
    breaks = seq(0, n_days5, by = 252),
    labels = c("Now", "1Y", "2Y", "3Y", "4Y", "5Y")
  ) +
  labs(
    title    = "Portfolio Monte Carlo Simulation",
    subtitle = "10,000 simulations over 1,260 trading days (5 years) · Based on 3-year return history",
    x        = NULL,
    y        = "Portfolio Value",
    caption  = "Shaded bands: 5th–95th pctile (light), 25th–75th pctile (dark) · Line: median"
  ) +
  theme_minimal(base_size = 13)

# --- Distribution of final values 5 ---------------------------------------------
final_df5 <- data.frame(final_value5 = final_vals5)

mc_final_dist5 <- ggplot(final_df5, aes(x = final_value5)) +
  geom_histogram(binwidth = 500, fill = "#3B82F6", colour = "white", alpha = 0.85) +
  geom_vline(xintercept = start_val,                    linetype = "dashed", colour = "grey30",  linewidth = 0.8) +
  geom_vline(xintercept = median(final_vals5),           linetype = "solid",  colour = "#1D4ED8", linewidth = 1) +
  geom_vline(xintercept = quantile(final_vals5, 0.05),   linetype = "dotted", colour = "#EF4444", linewidth = 0.9) +
  annotate("text", x = start_val + 5,                       y = Inf, label = "Start",   vjust = 2, hjust = 0, size = 3.5, colour = "grey30") +
  annotate("text", x = median(final_vals5) + 5,              y = Inf, label = "Median",  vjust = 2, hjust = 0, size = 3.5, colour = "#1D4ED8") +
  annotate("text", x = quantile(final_vals5, 0.05) - 5,      y = Inf, label = "5th %ile", vjust = 2, hjust = 1, size = 3.5, colour = "#EF4444") +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(
    title    = "Distribution of Portfolio Values After 5 Years",
    subtitle = "10,000 Monte Carlo simulations",
    x        = "Final Portfolio Value",
    y        = "Count"
  ) +
  theme_minimal(base_size = 13)


# --- Scenario / Stress Test Simulations ---------------------------------------
# Reuses mu_long, L_eigen, w, n_days5, n_sims, start_val from above.
# Each scenario applies a per-asset drift adjustment scaled by beta,
# representing a sustained market tailwind or headwind over 5 years.
#
# Scenarios:
#   Bear  — S&P -20 % / year  (severe sustained downturn)
#   Base  — historical mu_long (no adjustment)
#   Bull  — S&P +25 % / year  (strong sustained rally)

# Build a named beta vector aligned to mu_long
beta_lookup <- individual_betas %>% 
  dplyr::select(symbol, beta) %>% 
  dplyr::filter(symbol %in% names(mu_long))

beta_vec <- beta_lookup$beta[match(names(mu_long), beta_lookup$symbol)]
beta_vec[is.na(beta_vec)] <- 1  # fallback for any unmatched tickers

run_scenario <- function(annual_market_shock, seed = 4821) {
  daily_shock <- (1 + annual_market_shock)^(1 / 252) - 1
  mu_scenario <- mu_long + beta_vec * daily_shock

  set.seed(seed)
  paths <- matrix(NA_real_, nrow = n_days5 + 1, ncol = n_sims)
  paths[1, ] <- start_val

  for (sim in seq_len(n_sims)) {
    z       <- matrix(rnorm(n_days5 * length(mu_scenario)), nrow = n_days5)
    r_asset <- z %*% t(L_eigen) + matrix(mu_scenario, nrow = n_days5,
                                         ncol = length(mu_scenario), byrow = TRUE)
    r_port  <- as.vector(r_asset %*% w)
    paths[-1, sim] <- start_val * cumprod(1 + r_port)
  }
  paths
}

scenario_paths <- list(
  Bear = run_scenario(-0.20),
  Base = sim_paths5,          # already computed above
  Bull = run_scenario(0.25)
)

# --- Scenario fan chart (median paths + IQR bands) ----------------------------
scenario_bands <- purrr::imap_dfr(scenario_paths, \(paths, label) {
  apply(paths, 1, quantile, probs = c(0.25, 0.50, 0.75)) %>% 
    t() %>% 
    as.data.frame() %>% 
    setNames(c("p25", "p50", "p75")) %>% 
    dplyr::mutate(day = days_seq5, scenario = label)
})

scenario_colours <- c(Bear = "#EF4444", Base = "#3B82F6", Bull = "#16A34A")

mc_scenario_chart <- ggplot(scenario_bands, aes(x = day, colour = scenario, fill = scenario)) +
  geom_ribbon(aes(ymin = p25, ymax = p75), alpha = 0.12, colour = NA) +
  geom_line(aes(y = p50), linewidth = 1) +
  geom_hline(yintercept = start_val, linetype = "dashed", colour = "grey40") +
  scale_colour_manual(values = scenario_colours, name = "Scenario") +
  scale_fill_manual(values = scenario_colours, name = "Scenario") +
  scale_y_continuous(labels = scales::dollar_format()) +
  scale_x_continuous(
    breaks = seq(0, n_days5, by = 252),
    labels = c("Now", "1Y", "2Y", "3Y", "4Y", "5Y")
  ) +
  labs(
    title    = "Portfolio Monte Carlo — Scenario Comparison (5 Years)",
    subtitle = "Median path ± IQR band · Bear: S&P −20%/yr · Bull: S&P +25%/yr · 10,000 sims each",
    x        = NULL,
    y        = "Portfolio Value",
    caption  = "Per-asset drift adjusted by individual beta"
  ) +
  theme_minimal(base_size = 13)

# --- Scenario summary table ---------------------------------------------------
scenario_summary <- purrr::imap_dfr(scenario_paths, \(paths, label) {
  fv <- paths[n_days5 + 1, ]
  data.frame(
    Scenario       = label,
    `5th Pctile`   = quantile(fv, 0.05),
    Median         = median(fv),
    Mean           = mean(fv),
    `95th Pctile`  = quantile(fv, 0.95),
    `P(Loss)`      = mean(fv < start_val) * 100,
    `P(>+50%)`     = mean(fv > start_val * 1.5) * 100,
    check.names    = FALSE
  )
}) %>% 
  dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x, 2)))

mc_scenario_table <- scenario_summary %>% 
  gt() %>% 
  tab_header(
    title    = "Scenario Stress Test — 5-Year Outlook",
    subtitle = "10,000 simulations per scenario · Drift adjusted by asset beta"
  ) %>% 
  fmt_currency(columns = c(`5th Pctile`, Median, Mean, `95th Pctile`),
               currency = "USD", decimals = 0) %>% 
  fmt_number(columns = c(`P(Loss)`, `P(>+50%)`), decimals = 1, suffix = "%") %>% 
  tab_style(
    style     = cell_fill(color = "#FEF2F2"),
    locations = cells_body(rows = Scenario == "Bear")
  ) %>% 
  tab_style(
    style     = cell_fill(color = "#F0FDF4"),
    locations = cells_body(rows = Scenario == "Bull")
  ) %>% 
  tab_style(
    style     = cell_fill(color = "#EFF6FF"),
    locations = cells_body(rows = Scenario == "Base")
  ) %>% 
  tab_options(table.font.size = 13)



