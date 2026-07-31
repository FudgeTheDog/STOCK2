if (!exists("portfolio_full")) {
  source(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "Connor_Sharesies_Source.R"))
}

# Correlation/HHI ----------------------------------------------------------------
if (!all(sapply(portfolio_full$ticker, exists))) {
  getSymbols(portfolio_full$ticker, from = "2026-04-01", to = Sys.Date())
}

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
hhi_ticker <- portfolio_full %>% 
  summarise(HHI = sum(weight^2),
            `Equivalent N` = round(1 / sum(weight^2))) %>% 
  mutate(Level = case_when(
    HHI < 0.10 ~ "Diversified",
    HHI < 0.18 ~ "Moderate",
    TRUE        ~ "Concentrated"
  ))

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

# Per-sector weight breakdown
weight_by_sector <- portfolio_full %>% 
  group_by(sector) %>% 
  summarise(weight = sum(weight), .groups = "drop") %>% 
  mutate(pct = scales::percent(weight, accuracy = 0.1)) %>% 
  arrange(desc(weight)) 

