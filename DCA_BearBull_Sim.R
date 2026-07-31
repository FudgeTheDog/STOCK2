# DCA Bear-then-Bull Monte Carlo — v2 (Realistic Constraints)
# ══════════════════════════════════════════════════════════════════════════════
# $300/week for 30 weeks:
#   45% IVV.AX | 20% VEU | 15% VHY.AX
#   20% equally split across holdings with > 3% portfolio weight
#
# Regime: 2-year bear (S&P –20%/yr) → 3-year bull (S&P +25%/yr)
#
# v2 improvements over v1:
#   1. Drift caps by asset class — replaces unrealistic historical means
#      (e.g. AXTI 1364%/yr → 30% base, RKLB 319% → 30%)
#   2. Bear-phase correlation spike — blends historical correlations 50%
#      toward a uniform crash level (0.65), mimicking synchronized sell-offs
#   3. Bear-phase volatility scaling — 1.5× vol during crash
#      (models VIX doubling in real drawdowns)
#   4. Bull total drift also capped at class ceiling — no 200%/yr scenarios
#
# Requires from session: mu_long, cov_pd, L_eigen, w,
#                        portfolio_weights, start_val, individual_betas
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(Matrix)
library(gt)
library(glue)

# ── Parameters ─────────────────────────────────────────────────────────────────
weekly_contrib_dca  <- 175.65
n_contrib_weeks_dca <- 30
n_days_bear_dca     <- round(2 * 252)   # 504 trading days (~2 years)
n_days_bull_dca     <- round(3 * 252)   # 756 trading days (~3 years)
n_days_dca          <- n_days_bear_dca + n_days_bull_dca   # 1260
n_sims_dca          <- 3000
total_invested_dca  <- start_val + weekly_contrib_dca * n_contrib_weeks_dca

# ── Contribution allocation ─────────────────────────────────────────────────────
fixed_etfs  <- c("IVV.AX", "VEU", "VHY.AX")
fixed_alloc <- c("IVV.AX" = 0.40, "VEU" = 0.20, "VHY.AX" = 0.10)

top3_tickers_dca <- portfolio_weights %>% 
  dplyr::filter(weight > 0.03, !ticker %in% fixed_etfs) %>% 
  dplyr::pull(ticker)

rest_alloc_each <- 0.30 / length(top3_tickers_dca)

contrib_vec_dca <- setNames(rep(0, length(mu_long)), names(mu_long))
for (tk in names(fixed_alloc)) {
  if (tk %in% names(contrib_vec_dca))
    contrib_vec_dca[tk] <- weekly_contrib_dca * fixed_alloc[tk]
}
for (tk in top3_tickers_dca) {
  if (tk %in% names(contrib_vec_dca))
    contrib_vec_dca[tk] <- weekly_contrib_dca * rest_alloc_each
}

# ── Rebuild beta_vec ────────────────────────────────────────────────────────────
beta_lookup_dca <- individual_betas %>% 
  dplyr::select(symbol, beta) %>% 
  dplyr::filter(symbol %in% names(mu_long))
beta_vec <- beta_lookup_dca$beta[match(names(mu_long), beta_lookup_dca$symbol)]
beta_vec[is.na(beta_vec)] <- 1

# ── Asset classification ─────────────────────────────────────────────────────────
# Used to assign forward-looking drift caps rather than relying on recent
# historical returns (which can be extreme for speculative/high-beta assets)
asset_class <- dplyr::case_when(
  names(mu_long) %in% c("IVV.AX", "VEU", "VHY.AX", "VAS.AX", "IXJ.AX")
    ~ "ETF",
  names(mu_long) %in% c("AAPL", "GOOGL", "AMZN", "SAP", "TSM",
                         "GS", "VRTX", "REGN", "BK", "CAT", "ANZ.NZ", "NVDA")
    ~ "Quality",
  names(mu_long) %in% c("HWM", "DORM", "LRCX", "GE", "GEV",
                         "EVR", "ESE", "TTMI")
    ~ "Growth",
  names(mu_long) %in% c("RKLB", "IONQ", "QBTS", "AXTI")
    ~ "Speculative",
  TRUE ~ "Quality"   # safe fallback
)

# ── Drift caps ──────────────────────────────────────────────────────────────────
# Base cap:  ceiling on the raw historical mean  (prevents recent-run artifacts)
# Bull cap:  ceiling on total drift incl. market shock  (prevents explosive compounding)
#
# Rationale:
#   ETF:         10–12% realistic equity premium; bull follows market (28%)
#   Quality:     15%/yr for great businesses; bull +35% in a raging market
#   Growth:      20%/yr base; bull +45% with positive market regime
#   Speculative: 30%/yr base (not 300–1400% historical); bull +60% maximum
#
cap_base_annual <- dplyr::case_when(
  asset_class == "ETF"         ~ 0.12,
  asset_class == "Quality"     ~ 0.20,
  asset_class == "Growth"      ~ 0.15,
  asset_class == "Speculative" ~ 0.40
)
cap_bull_annual <- dplyr::case_when(
  asset_class == "ETF"         ~ 0.28,
  asset_class == "Quality"     ~ 0.35,
  asset_class == "Growth"      ~ 0.45,
  asset_class == "Speculative" ~ 0.60
)

cap_base_daily <- (1 + cap_base_annual)^(1 / 252) - 1
cap_bull_daily <- (1 + cap_bull_annual)^(1 / 252) - 1

# Apply base cap  (ceiling; preserves any historically negative means)
mu_reg <- pmin(mu_long, cap_base_daily)

# Regime shocks
daily_drift_bear_dca <- (1 - 0.20)^(1 / 252) - 1
daily_drift_bull_dca <- (1 + 0.25)^(1 / 252) - 1

# Bear drift: uncapped downside — the pain is the point
mu_bear_dca <- mu_reg + beta_vec * daily_drift_bear_dca

# Bull drift: cap total at asset-class ceiling
mu_bull_uncap <- mu_reg + beta_vec * daily_drift_bull_dca
mu_bull_dca   <- pmin(mu_bull_uncap, cap_bull_daily)

# ── Print drift assumption comparison ──────────────────────────────────────────
drift_check <- data.frame(
  ticker        = names(mu_long),
  class         = asset_class,
  hist_annual   = round(((1 + mu_long)^252 - 1) * 100, 1),
  base_cap      = round(cap_base_annual * 100, 0),
  capped_annual = round(((1 + mu_reg)^252 - 1) * 100, 1),
  bull_total    = round(((1 + mu_bull_dca)^252 - 1) * 100, 1),
  bear_total    = round(((1 + mu_bear_dca)^252 - 1) * 100, 1)
) %>%  dplyr::arrange(class, desc(hist_annual))

cat("=== Drift Assumption Comparison ===\n")
cat(sprintf("%-10s %-12s %8s %8s %8s %8s %8s\n",
            "Ticker", "Class", "Hist%", "BaseCap%", "Capped%", "Bull%", "Bear%"))
cat(strrep("-", 68), "\n")
for (i in seq_len(nrow(drift_check))) {
  with(drift_check[i, ], cat(sprintf(
    "%-10s %-12s %8.1f %8.0f %8.1f %8.1f %8.1f\n",
    ticker, class, hist_annual, base_cap, capped_annual, bull_total, bear_total
  )))
}

# ── Bear-phase stress covariance ────────────────────────────────────────────────
# In real crashes:
#   - Correlations spike as indiscriminate selling dominates (blend → 0.65)
#   - Realised volatility roughly doubles (VIX 15 → 30+)  →  scale 1.5×
n_assets_dca <- length(mu_long)
cor_hist     <- cov2cor(cov_pd)
vol_hist     <- sqrt(diag(cov_pd))

# Crash correlation template: uniform 0.65 off-diagonal
cor_crash <- matrix(0.65, n_assets_dca, n_assets_dca)
diag(cor_crash) <- 1

# 50/50 blend: keeps asset-specific correlations but adds systematic component
alpha_stress <- 0.5
cor_stress   <- (1 - alpha_stress) * cor_hist + alpha_stress * cor_crash
cor_stress   <- (cor_stress + t(cor_stress)) / 2   # enforce symmetry
diag(cor_stress) <- 1

# Scale volatility
vol_stress     <- vol_hist * 1.5
cov_stress_raw <- outer(vol_stress, vol_stress) * cor_stress
cov_stress_pd  <- as.matrix(nearPD(cov_stress_raw, corr = FALSE)$mat)

eig_stress   <- eigen(cov_stress_pd, symmetric = TRUE)
L_eigen_bear <- eig_stress$vectors %*% diag(sqrt(pmax(eig_stress$values, 0)))

cat("\nBear-phase stress parameters:\n")
cat(sprintf("  Average off-diagonal correlation: %.3f (hist) → %.3f (stressed)\n",
            mean(cor_hist[lower.tri(cor_hist)]),
            mean(cor_stress[lower.tri(cor_stress)])))
cat(sprintf("  Volatility scaling: 1.5× (avg daily vol: %.4f → %.4f)\n",
            mean(vol_hist), mean(vol_stress)))

# ── Contribution schedule ───────────────────────────────────────────────────────
contrib_days_dca <- seq(5, n_contrib_weeks_dca * 5, by = 5)
init_holdings    <- start_val * w

# ── Simulation ──────────────────────────────────────────────────────────────────
# Vectorised within each sim via present-value identity:
#   V(t) = Σ_i  cumret_i(t) × [init_i + Σ_{c≤t} C_i / cumret_i(c)]
# Bear uses L_eigen_bear (stressed); bull uses L_eigen (normal)

set.seed(7391)
sim_paths_dca <- matrix(NA_real_, nrow = n_days_dca + 1, ncol = n_sims_dca)
sim_paths_dca[1, ] <- start_val

cat("\nRunning", n_sims_dca, "simulations...\n")

for (sim in seq_len(n_sims_dca)) {
  # Bear phase: stressed correlations + higher vol
  z_bear <- matrix(rnorm(n_days_bear_dca * n_assets_dca), nrow = n_days_bear_dca)
  r_bear <- z_bear %*% t(L_eigen_bear) +
    matrix(mu_bear_dca, nrow = n_days_bear_dca, ncol = n_assets_dca, byrow = TRUE)

  # Bull phase: normal correlations + vol
  z_bull <- matrix(rnorm(n_days_bull_dca * n_assets_dca), nrow = n_days_bull_dca)
  r_bull <- z_bull %*% t(L_eigen) +
    matrix(mu_bull_dca, nrow = n_days_bull_dca, ncol = n_assets_dca, byrow = TRUE)

  r_all  <- rbind(r_bear, r_bull)
  cumret <- apply(1 + r_all, 2, cumprod)

  # Present-value of each weekly contribution
  contrib_pv <- matrix(0, nrow = n_days_dca, ncol = n_assets_dca)
  for (c in contrib_days_dca) {
    contrib_pv[c, ] <- contrib_vec_dca / cumret[c, ]
  }
  cum_contrib_pv <- apply(contrib_pv, 2, cumsum)

  adj_init <- matrix(init_holdings, nrow = n_days_dca, ncol = n_assets_dca,
                     byrow = TRUE) + cum_contrib_pv

  sim_paths_dca[-1, sim] <- rowSums(cumret * adj_init)
}

# ── Summary statistics ──────────────────────────────────────────────────────────
final_vals_dca <- sim_paths_dca[n_days_dca + 1, ]

cat("=== Final Value Summary (5 years, bear→bull + DCA) ===\n")
cat(sprintf("  Starting value:    %s\n", scales::dollar(start_val)))
cat(sprintf("  Total invested:    %s\n", scales::dollar(total_invested_dca)))
cat(sprintf("  5th  percentile:   %s\n", scales::dollar(quantile(final_vals_dca, 0.05))))
cat(sprintf("  25th percentile:   %s\n", scales::dollar(quantile(final_vals_dca, 0.25))))
cat(sprintf("  Median:            %s\n", scales::dollar(median(final_vals_dca))))
cat(sprintf("  Mean:              %s\n", scales::dollar(mean(final_vals_dca))))
cat(sprintf("  75th percentile:   %s\n", scales::dollar(quantile(final_vals_dca, 0.75))))
cat(sprintf("  95th percentile:   %s\n", scales::dollar(quantile(final_vals_dca, 0.95))))
cat(sprintf("  P(loss vs invested): %.1f%%\n",
            mean(final_vals_dca < total_invested_dca) * 100))

# ── Summary table ───────────────────────────────────────────────────────────────
dca_summary_data <- data.frame(
  scenario    = c(
    "Starting Value",
    "Total Invested (incl. DCA)",
    "5th Percentile (Downside)",
    "25th Percentile",
    "Median",
    "Mean",
    "75th Percentile",
    "95th Percentile",
    "Probability of loss (vs. total invested)",
    "Probability of >50% gain on invested"
  ),
  final_value = c(
    start_val, total_invested_dca,
    quantile(final_vals_dca, 0.05), quantile(final_vals_dca, 0.25),
    median(final_vals_dca),  mean(final_vals_dca),
    quantile(final_vals_dca, 0.75), quantile(final_vals_dca, 0.95),
    NA, NA
  ),
  return_pct = c(
    0, 0,
    (quantile(final_vals_dca, 0.05)  / total_invested_dca - 1) * 100,
    (quantile(final_vals_dca, 0.25)  / total_invested_dca - 1) * 100,
    (median(final_vals_dca)          / total_invested_dca - 1) * 100,
    (mean(final_vals_dca)            / total_invested_dca - 1) * 100,
    (quantile(final_vals_dca, 0.75)  / total_invested_dca - 1) * 100,
    (quantile(final_vals_dca, 0.95)  / total_invested_dca - 1) * 100,
    mean(final_vals_dca < total_invested_dca)      * 100,
    mean(final_vals_dca > total_invested_dca * 1.5) * 100
  )
)

dca_summary_table <- dca_summary_data %>% 
  gt() %>% 
  tab_header(
    title    = "DCA Bear→Bull Simulation — 5-Year Outlook (v2)",
    subtitle = glue(
      "3,000 sims · $300/wk × 30 wks · ",
      "2Y bear (S&P –20%/yr) → 3Y bull (S&P +25%/yr)\n",
      "Drift caps: ETF ≤28% · Quality ≤35% · Growth ≤45% · Spec ≤60% | ",
      "Bear: +1.5× vol, corr spike to 0.65"
    )
  ) %>% 
  cols_label(
    scenario    = "Scenario",
    final_value = "Final Value (USD)",
    return_pct  = "Return vs. Invested"
  ) %>% 
  fmt_currency(columns = final_value, currency = "USD", decimals = 0) %>% 
  fmt_number(columns  = return_pct,  decimals = 1, suffix = "%") %>% 
  tab_row_group(label = "Probabilities",   rows = 9:10) %>% 
  tab_row_group(label = "Percentiles",     rows = 3:8)  %>% 
  tab_row_group(label = "Capital Summary", rows = 1:2)  %>% 
  sub_missing(columns = final_value, missing_text = "—") %>% 
  tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups()) %>% 
  tab_style(
    style     = cell_fill(color = "#EFF6FF"),
    locations = cells_body(rows = scenario == "Median")
  ) %>% 
  tab_style(
    style     = cell_fill(color = "#F0FDF4"),
    locations = cells_body(rows = scenario == "Total Invested (incl. DCA)")
  ) %>% 
  tab_style(
    style     = cell_text(color = "#DC2626"),
    locations = cells_body(columns = return_pct, rows = return_pct < 0)
  ) %>% 
  tab_style(
    style     = cell_text(color = "#16A34A"),
    locations = cells_body(columns = return_pct, rows = return_pct > 0 & !is.na(final_value))
  ) %>% 
  tab_options(table.font.size = 13)

# ── Drift assumptions table ─────────────────────────────────────────────────────
drift_gt_data <- drift_check %>% 
  dplyr::select(ticker, class, hist_annual, capped_annual, bull_total, bear_total) %>% 
  dplyr::arrange(class, ticker)

drift_assumptions_table <- drift_gt_data %>% 
  gt(groupname_col = "class") %>% 
  tab_header(
    title    = "Drift Assumptions — Historical vs. Capped",
    subtitle = "All values are expected annualised returns (%)"
  ) %>% 
  cols_label(
    ticker        = "Ticker",
    hist_annual   = "Historical",
    capped_annual = "Capped Base",
    bull_total    = "Bull Total",
    bear_total    = "Bear Total"
  ) %>% 
  fmt_number(
    columns = c(hist_annual, capped_annual, bull_total, bear_total),
    decimals = 1, suffix = "%"
  ) %>% 
  tab_style(
    style     = cell_text(color = "#DC2626", weight = "bold"),
    locations = cells_body(columns = hist_annual,
                           rows    = hist_annual > capped_annual + 5)
  ) %>% 
  tab_style(
    style     = cell_text(color = "#DC2626"),
    locations = cells_body(columns = bear_total, rows = bear_total < 0)
  ) %>% 
  tab_style(
    style     = cell_text(color = "#16A34A"),
    locations = cells_body(columns = bull_total, rows = bull_total > 0)
  ) %>% 
  tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups()) %>% 
  tab_options(table.font.size = 12)

# ── Fan chart ───────────────────────────────────────────────────────────────────
days_seq_dca <- 0:n_days_dca
pctiles_dca  <- c(0.05, 0.25, 0.50, 0.75, 0.95)

band_df_dca <- pctiles_dca %>% 
  purrr::map_dfc(\(p) apply(sim_paths_dca, 1, quantile, probs = p)) %>% 
  setNames(paste0("p", pctiles_dca * 100)) %>% 
  dplyr::mutate(day = days_seq_dca)

set.seed(7391)
path_sample_dca <- sim_paths_dca[, sample(n_sims_dca, 80)] %>% 
  as.data.frame() %>% 
  dplyr::mutate(day = days_seq_dca) %>% 
  tidyr::pivot_longer(-day, names_to = "sim", values_to = "value")

# Decide scale: check if log scale is needed
p95_final <- quantile(final_vals_dca, 0.95)
p05_final <- quantile(final_vals_dca, 0.05)
use_log   <- (p95_final / p05_final) > 20   # log if 95th is >20× the 5th

dca_fan_chart <- ggplot() +
  annotate("rect", xmin = 0, xmax = n_days_bear_dca, ymin = -Inf, ymax = Inf,
           fill = "#FEF2F2", alpha = 0.6) +
  annotate("rect", xmin = n_days_bear_dca, xmax = n_days_dca, ymin = -Inf, ymax = Inf,
           fill = "#F0FDF4", alpha = 0.6) +
  annotate("text", x = n_days_bear_dca / 2,                    y = Inf,
           label = "Bear Market (–20%/yr)", vjust = 2,
           size = 3.8, colour = "#DC2626", fontface = "bold") +
  annotate("text", x = n_days_bear_dca + n_days_bull_dca / 2,  y = Inf,
           label = "Bull Market (+25%/yr)", vjust = 2,
           size = 3.8, colour = "#16A34A", fontface = "bold") +
  geom_vline(xintercept = n_days_bear_dca,
             linetype = "dashed", colour = "grey50", linewidth = 0.6) +
  geom_vline(xintercept = max(contrib_days_dca),
             linetype = "dotted", colour = "#7C3AED", linewidth = 0.7) +
  annotate("text", x = max(contrib_days_dca) + 6,
           y = median(final_vals_dca) * 0.4,
           label = "DCA ends\n(wk 30)", hjust = 0,
           size = 3.1, colour = "#7C3AED") +
  geom_line(data = path_sample_dca, aes(x = day, y = value, group = sim),
            colour = "#3B82F6", alpha = 0.05, linewidth = 0.3) +
  geom_ribbon(data = band_df_dca, aes(x = day, ymin = p5,  ymax = p95),
              fill = "#3B82F6", alpha = 0.15) +
  geom_ribbon(data = band_df_dca, aes(x = day, ymin = p25, ymax = p75),
              fill = "#3B82F6", alpha = 0.25) +
  geom_line(data = band_df_dca, aes(x = day, y = p50),
            colour = "#1D4ED8", linewidth = 1) +
  geom_hline(yintercept = start_val,
             linetype = "dashed", colour = "grey40", linewidth = 0.7) +
  geom_hline(yintercept = total_invested_dca,
             linetype = "dotted", colour = "#7C3AED", linewidth = 0.7) +
  {if (use_log) scale_y_log10(labels = scales::dollar_format())
   else scale_y_continuous(labels = scales::dollar_format())} +
  scale_x_continuous(
    breaks = c(0, 252, 504, 756, 1008, 1260),
    labels = c("Now", "1Y", "2Y\n(Bull starts)", "3Y", "4Y", "5Y")
  ) +
  labs(
    title    = "DCA Bear→Bull Simulation (v2 — Realistic Constraints)",
    subtitle = "$300/wk for 30 weeks · 3,000 sims · 5-year horizon",
    x        = NULL,
    y        = if (use_log) "Portfolio Value (log scale)" else "Portfolio Value",
    caption  = paste0(
      "Drift capped by class: ETF ≤12% base/28% bull · Quality ≤15%/35% · Growth ≤20%/45% · Spec ≤30%/60%\n",
      "Bear phase: correlations blended 50% toward 0.65 + 1.5× volatility · Bands: 5th–95th (light), 25th–75th (dark)"
    )
  ) +
  theme_minimal(base_size = 13)

# ── Final distribution histogram ────────────────────────────────────────────────
final_df_dca <- data.frame(final_value = final_vals_dca)
hist_clip    <- quantile(final_vals_dca, 0.99)   # clip top 1% for readability

dca_final_dist <- ggplot(
  final_df_dca %>%  dplyr::filter(final_value <= hist_clip),
  aes(x = final_value)
) +
  geom_histogram(bins = 60, fill = "#3B82F6", colour = "white", alpha = 0.85) +
  geom_vline(xintercept = pmin(total_invested_dca, hist_clip), linetype = "dotted",
             colour = "#7C3AED", linewidth = 0.9) +
  geom_vline(xintercept = pmin(start_val, hist_clip), linetype = "dashed",
             colour = "grey30",  linewidth = 0.8) +
  geom_vline(xintercept = pmin(median(final_vals_dca), hist_clip), linetype = "solid",
             colour = "#1D4ED8", linewidth = 1.0) +
  geom_vline(xintercept = quantile(final_vals_dca, 0.05), linetype = "dotted",
             colour = "#EF4444", linewidth = 0.9) +
  annotate("text", x = total_invested_dca + hist_clip * 0.01, y = Inf,
           label = glue("Invested\n({scales::dollar(total_invested_dca, accuracy=1)})"),
           vjust = 1.5, hjust = 0, size = 3.0, colour = "#7C3AED") +
  annotate("text", x = median(final_vals_dca) + hist_clip * 0.01, y = Inf,
           label = glue("Median\n({scales::dollar(median(final_vals_dca), accuracy=1)})"),
           vjust = 1.5, hjust = 0, size = 3.0, colour = "#1D4ED8") +
  annotate("text", x = quantile(final_vals_dca, 0.05) - hist_clip * 0.01, y = Inf,
           label = glue("5th %ile\n({scales::dollar(quantile(final_vals_dca, 0.05), accuracy=1)})"),
           vjust = 1.5, hjust = 1, size = 3.0, colour = "#EF4444") +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(
    title    = "Distribution of Final Portfolio Values — 5 Years (v2)",
    subtitle = glue("3,000 Monte Carlo sims · Bear→Bull + DCA · Bottom 99% shown"),
    x        = "Final Portfolio Value",
    y        = "Count",
    caption  = glue("Top 1% (> {scales::dollar(hist_clip, accuracy=1)}) not shown")
  ) +
  theme_minimal(base_size = 13)

# ── Allocation table ────────────────────────────────────────────────────────────
alloc_show <- contrib_vec_dca[contrib_vec_dca > 0]
dca_alloc_df <- data.frame(
  Ticker     = names(alloc_show),
  Class      = asset_class[names(alloc_show)],
  Weekly_USD = round(alloc_show, 2),
  Pct        = round(alloc_show / weekly_contrib_dca * 100, 2),
  Total_30wk = round(alloc_show * 30, 2),
  row.names  = NULL
)

dca_alloc_table <- dca_alloc_df %>% 
  dplyr::arrange(desc(Weekly_USD)) %>% 
  gt() %>% 
  tab_header(
    title    = "DCA Contribution Allocation",
    subtitle = "$300/week · 30 weeks · $9,000 total"
  ) %>% 
  cols_label(
    Ticker = "Ticker", Class = "Class",
    Weekly_USD = "Per Week", Pct = "Alloc %", Total_30wk = "30-Wk Total"
  ) %>% 
  fmt_currency(columns = c(Weekly_USD, Total_30wk), currency = "USD", decimals = 2) %>% 
  fmt_number(columns = Pct, decimals = 2, suffix = "%") %>% 
  grand_summary_rows(
    columns = c(Weekly_USD, Total_30wk),
    fns     = list(Total = ~sum(.)),
    fmt     = ~fmt_currency(., currency = "USD", decimals = 2)
  ) %>% 
  grand_summary_rows(
    columns = Pct,
    fns     = list(Total = ~sum(.)),
    fmt     = ~fmt_number(., decimals = 2, suffix = "%")
  ) %>% 
  tab_options(table.font.size = 13)

# ── Sector gain attribution ────────────────────────────────────────────────────
# Uses deterministic drift (mean GBM expectation under bear→bull regime) to
# attribute expected portfolio gain to each GICS sector.
# All 30 DCA contributions fall inside the 2-year bear phase.

# Pull sector mapping from existing portfolio_full table
sector_lookup <- portfolio_full %>% 
  dplyr::select(ticker, sector) %>% 
  dplyr::distinct()

# Expected final value per asset via deterministic drift
bear_growth_asset  <- (1 + mu_bear_dca)^n_days_bear_dca   # per-asset bear growth
bull_growth_asset  <- (1 + mu_bull_dca)^n_days_bull_dca   # per-asset bull growth
total_growth_asset <- bear_growth_asset * bull_growth_asset

# Initial holdings grow through the full 5 years
initial_final_asset <- init_holdings * total_growth_asset

# Each weekly DCA contribution grows from its contribution day to the end
#   contrib day c (bear phase) → grows bear*(504-c) days then bull*756 days
dca_final_asset <- rep(0, n_assets_dca)
names(dca_final_asset) <- names(mu_long)
for (c_day in contrib_days_dca) {
  remaining_bear  <- n_days_bear_dca - c_day
  growth_from_c   <- (1 + mu_bear_dca)^remaining_bear * bull_growth_asset
  dca_final_asset <- dca_final_asset + contrib_vec_dca * growth_from_c
}

expected_final_asset   <- initial_final_asset + dca_final_asset
total_invested_asset   <- init_holdings + contrib_vec_dca * n_contrib_weeks_dca
expected_gain_asset    <- expected_final_asset - total_invested_asset

# Asset-level frame
asset_gain_df <- data.frame(
  ticker         = names(mu_long),
  total_invested = total_invested_asset,
  expected_final = expected_final_asset,
  expected_gain  = expected_gain_asset,
  row.names      = NULL
) %>% 
  dplyr::left_join(sector_lookup, by = "ticker") %>% 
  dplyr::mutate(
    # Tickers not in portfolio_full (shouldn't happen) fall back to asset_class
    sector = dplyr::coalesce(sector, asset_class[match(ticker, names(mu_long))])
  )

# Sector-level summary
sector_gain_df <- asset_gain_df %>% 
  dplyr::group_by(sector) %>% 
  dplyr::summarise(
    holdings       = dplyr::n(),
    total_invested = sum(total_invested),
    expected_final = sum(expected_final),
    expected_gain  = sum(expected_gain),
    .groups        = "drop"
  ) %>% 
  dplyr::mutate(return_pct = expected_gain / total_invested * 100) %>% 
  dplyr::arrange(desc(expected_gain))

# Sector colour palette (consistent with existing portfolio charts)
sector_colours_dca <- c(
  "Technology"       = "#3B82F6",
  "Industrials"      = "#F59E0B",
  "Financials"       = "#10B981",
  "Healthcare"       = "#EF4444",
  "Consumer Cyclical"= "#8B5CF6",
  "ETF"              = "#6B7280"
)

# ── Sector bar chart ───────────────────────────────────────────────────────────
dca_sector_chart <- ggplot(
  sector_gain_df,
  aes(y = reorder(sector, expected_gain), x = expected_gain, fill = sector)
) +
  geom_bar(stat = "identity", width = 0.65) +
  geom_text(
    aes(label = paste0(
      scales::dollar(expected_gain, accuracy = 1),
      "  (", sprintf("%.1f%%", return_pct), ")"
    )),
    hjust = -0.06, size = 3.5
  ) +
  scale_x_continuous(
    labels = scales::dollar_format(),
    expand = expansion(mult = c(0, 0.45))
  ) +
  scale_fill_manual(values = sector_colours_dca, guide = "none") +
  labs(
    title    = "Expected Gain by Sector — DCA Bear→Bull Scenario",
    subtitle = "Deterministic drift (mean expectation) · 2Y bear (–20%/yr) → 3Y bull (+25%/yr)",
    x        = "Expected Gain (USD)",
    y        = NULL,
    caption  = "Return % shown relative to capital invested in that sector"
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank())

# ── Sector gt table ────────────────────────────────────────────────────────────
dca_sector_table <- sector_gain_df %>% 
  gt() %>% 
  tab_header(
    title    = "Portfolio Gain by Sector — DCA Bear→Bull",
    subtitle = "Expected values under mean drift · 2Y bear (S&P –20%/yr) → 3Y bull (+25%/yr)"
  ) %>% 
  cols_label(
    sector         = "Sector",
    holdings       = "Holdings",
    total_invested = "Total Invested",
    expected_final = "Expected Final",
    expected_gain  = "Expected Gain",
    return_pct     = "Return"
  ) %>% 
  fmt_currency(
    columns  = c(total_invested, expected_final, expected_gain),
    currency = "USD", decimals = 0
  ) %>% 
  fmt_number(columns = return_pct, decimals = 1, suffix = "%") %>% 
  tab_style(
    style     = cell_text(color = "#16A34A", weight = "bold"),
    locations = cells_body(columns = expected_gain)
  ) %>% 
  tab_style(
    style     = cell_text(color = "#16A34A"),
    locations = cells_body(columns = return_pct)
  ) %>% 
  # Colour-code sector rows
  tab_style(
    style     = cell_fill(color = "#EFF6FF"),
    locations = cells_body(rows = sector == "ETF")
  ) %>% 
  tab_style(
    style     = cell_fill(color = "#F0FDF4"),
    locations = cells_body(rows = sector == "Technology")
  ) %>% 
  grand_summary_rows(
    columns = c(total_invested, expected_final, expected_gain),
    fns     = list(Total = ~sum(.)),
    fmt     = ~fmt_currency(., currency = "USD", decimals = 0)
  ) %>% 
  grand_summary_rows(
    columns = holdings,
    fns     = list(Total = ~sum(.)),
    fmt     = ~fmt_number(., decimals = 0)
  ) %>% 
  tab_options(table.font.size = 13)

