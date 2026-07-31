Connors_Sharsies_2.0 <- portfolio_data %>%
  left_join(latest_prices, by = "ticker") %>%
  mutate(current_worth = shares * close,
         avg_cost = total_cost / shares, 
         gain_loss = current_worth - total_cost,
         percent_change = (gain_loss / total_cost) * 100
  ) %>%
  mutate(weight = current_worth / sum(current_worth))
print(Connors_Sharsies_2.0)
View(Connors_Sharsies_2.0)
write.csv(Connors_Sharsies_2.0, "Connors_Sharsies_2.0.csv", row.names = FALSE)


portfolio_data <- tibble(
  ticker = c("AAPL", "GOOGL", "TSM", "CAT", "NVDA","IONQ","TTMI","AXTI", "BK",
             "QBTS","GE","ESE","YSS","HWM","SAP","RKLB","DORM","LRCX","EVR",
             "GS","ANZ.NZ","VAS.AX","V500.AX","IXJ.AX","IVV.AX"),
  shares = c(0.02456,0.031789,0.00945,0.005496,0.031846,0.139908,0.044607,
             0.031056,0.011659, 0.407013,0.021907,0.019189,0.113464,0.06334,
             0.039962,0.116223,0.045232,0.024243,0.019679,0.010029,
             0.218176,0.090456,0.097511,0.036901,0.10558684),
  total_cost = c(7, 12.50,3.90,5,6.90,7,7.50,3.50,1.60,9.24,7.00,6.01,4.58,
                 15.00,7.00,8.00,5.00,6.50,7.00,9.14,10,10,5,
                 5,7),
  sector = c("Technology","Technology","Technology","Industrials","Technology",
             "Technology","Technology","Technology","Financials","Technology",
             "Industrials","Industrials","Industrials","Industrials",
             "Technology","Industrials","Consumer cyclical","Technology",
             "Financials","Financials","Financials","ETF","ETF",
             "ETF","ETF"),
  industry = c("Consumer Electronics","Internet Services","Semiconductors",
               "Construction Machinery","Semiconductors","Quantum Computing",
               "Electronic Components","Semiconductors","Asset Management",
               "Quantum Computing","Aerospace & Defense","Defense Electronics",
               "Aerospace & Defense","Aerospace Components",
               "Enterprise Software","Space & Aerospace","Auto Parts",
               "Semiconductors","Investment Banking","Investment Banking",
               "Banking","AU Equities","Intl Equities","Healthcare","US Equities")
)


#GOOGL
avg_cost_googl <- 393.12
googl_close <- Cl(GOOGL)
last_close_googl <- as.numeric(last(googl_close))
pct_diff_googl <- (last_close_googl-avg_cost_googl)/avg_cost_googl*100
chartSeries(GOOGL, theme = chartTheme('white'),
            name = sprintf("GOOGL | Avg Cost: $%.2f (%+.1f%%)",
                           avg_cost_googl,pct_diff_googl))
addLines(h=avg_cost_googl, col = "red")
#CAT
avg_cost_cat <- 909.12
cat_close <- Cl(CAT)
last_close_cat <- as.numeric(last(cat_close))
pct_diff_cat <- (last_close_cat-avg_cost_cat)/avg_cost_cat*100
chartSeries(CAT, theme = chartTheme('white'),
            name = sprintf("CAT | Avg Cost: $%.2f (%+.1f%%)",
                           avg_cost_cat,pct_diff_cat))
addLines(h=avg_cost_cat, col = "red")
#TSM
avg_cost_tsm <- 412.41
tsm_close <- Cl(TSM)
last_close_tsm <- as.numeric(last(tsm_close))
pct_diff_tsm <- (last_close_tsm-avg_cost_tsm)/avg_cost_tsm*100
chartSeries(TSM, theme = chartTheme('white'),
            name = sprintf("TSM | Avg Cost: $%.2f (%+.1f%%)",
                           avg_cost_tsm,pct_diff_tsm))
addLines(h=avg_cost_tsm, col = "red")
#NVDA
avg_cost_nvda <- 216.71
nvda_close <- Cl(NVDA)
last_close_nvda <- as.numeric(last(nvda_close))
pct_diff_nvda <- (last_close_nvda-avg_cost_nvda)/avg_cost_nvda*100
chartSeries(NVDA, theme = chartTheme('white'),
            name = sprintf("NVAD | Avg Cost: $%.2f (%+.1f%%)",
                           avg_cost_nvda,pct_diff_nvda))
addLines(h=avg_cost_nvda, col = "red")
#IONQ
avg_cost_ionq <- 50.03
ionq_close <- Cl(IONQ)
last_close_ionq <- as.numeric(last(ionq_close))
pct_diff_ionq <- (last_close_ionq-avg_cost_ionq)/avg_cost_ionq*100
chartSeries(IONQ, theme = chartTheme('white'),
            name = sprintf("IONQ | Avg Cost: $%.2f (%+.1f%%)",
                           avg_cost_ionq,pct_diff_ionq))
addLines(h=avg_cost_ionq, col = "red")
#TTMI
avg_cost_ttmi <- 168.14
ttmi_close <- Cl(TTMI)
last_close_ttmi <- as.numeric(last(ttmi_close))
pct_diff_ttmi <- (last_close_ttmi-avg_cost_ttmi)/avg_cost_ttmi*100
chartSeries(TTMI, theme = chartTheme('white'),
            name = sprintf("TTMI | Avg Cost: $%.2f (%+.1f%%)",
                           avg_cost_ttmi,pct_diff_ttmi))
addLines(h=avg_cost_ttmi, col = "red")
#AXTI
avg_cost_axti <- 112.85
axti_close <- Cl(AXTI)
last_close_axti <- as.numeric(last(axti_close))
pct_diff_axti <- (last_close_axti-avg_cost_axti)/avg_cost_axti*100
chartSeries(AXTI, theme = chartTheme('white'),
            name = sprintf("AXTI | Avg Cost: $%.2f (%+.1f%%)",
                           avg_cost_axti,pct_diff_axti))
addLines(h=avg_cost_axti, col = "red")
#BK
avg_cost_bk <- 137.21
bk_close <- Cl(BK)
last_close_bk <- as.numeric(last(bk_close))
pct_diff_bk <- (last_close_bk-avg_cost_bk)/avg_cost_bk*100
chartSeries(BK, theme = chartTheme('white'),
            name = sprintf("BK | Avg Cost: $%.2f (%+.1f%%)",
                           avg_cost_bk,pct_diff_bk))
addLines(h=avg_cost_bk, col = "red")
#RKLB
avg_cost_rklb <- 68.83
rklb_close <- Cl(RKLB)
last_close_rklb <- as.numeric(last(rklb_close))
pct_diff_rklb <- (last_close_rklb-avg_cost_rklb)/avg_cost_rklb*100
chartSeries(RKLB, theme = chartTheme('white'),
            name = sprintf("RKLB | Avg Cost: $%.2f (%+.1f%%)",
                           avg_cost_rklb,pct_diff_rklb))
addLines(h=avg_cost_rklb, col = "red")
#GE
avg_cost_ge <- 319.50
ge_close <- Cl(GE)
last_close_ge <- as.numeric(last(ge_close))
pct_diff_ge <- (last_close_ge-avg_cost_ge)/avg_cost_ge*100
chartSeries(GE, theme = chartTheme('white'),
            name = sprintf("GE | Avg Cost: $%.2f (%+.1f%%)",
                           avg_cost_ge,pct_diff_ge))
addLines(h=avg_cost_ge, col = "red")
