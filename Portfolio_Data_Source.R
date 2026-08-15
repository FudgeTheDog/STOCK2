
# Portfolio Source Data ------------------------------------------------------

portfolio_data <- tribble(
  ~ticker,    ~shares,       ~total_cost, ~sector,              ~industry,                          ~currency,
  "AAPL",     0.14402841,    43.55,       "Technology",         "Consumer Electronics",             "USD",
  "GOOGL",    0.13408317,    49.05,       "Technology",         "Internet Services",                "USD",
  "NVDA",     0.28384439,    64.33,       "Technology",         "Semiconductors",                   "USD",
  "TTMI",     0.04460699,    7.50,        "Technology",         "Electronic Components",            "USD",
  "LRCX",     0.13514633,    41.06,       "Technology",         "Semiconductors",                   "USD",
  "CAT",      0.00549568,    5.00,        "Industrials",        "Construction Machinery",           "USD",
  "HWM",      0.23645467,    62.51,       "Industrials",        "Aerospace Components",             "USD",
  "RKLB",     0.8699045,     93.61,       "Industrials",        "Space & Aerospace",                "USD",
  "DORM",     0.32807291,    40.18,       "Consumer Cyclical",  "Auto Parts",                       "USD",
  "BNY",      0.38747149,    57.27,       "Financials",         "Asset Management",                 "USD",
  "EVR",      0.01967888,    7.00,        "Financials",         "Investment Banking",               "USD",
  "GS",       0.0499979,     50.32,       "Financials",         "Investment Banking",               "USD",
  "IVV.AX",   14.31372828,   1027.99,      "ETF",                "US Equities",                      "AUD",
  "VRTX",     0.13189165,    61.47,       "Healthcare",         "Biotechnology",                    "USD",
  "REGN",     0.05745529,    43.67,       "Healthcare",         "Biotechnology",                    "USD",
  "GEV",      0.01609577,    17.79,       "Industrials",        "Speciality industrial machinery",  "USD",
  "VEU",      0.71493771,    60.08,       "ETF",                "World Equities",                   "USD",
  "AMZN",     0.1667313,     44.43,       "Consumer Cyclical",  "Internet Retail",                  "USD",
  "VHY.AX",   1.21201604,    101.92,      "ETF",                "World Equities",                   "AUD",
  "MELI",     0.01785641,    29.14,       "Consumer Cyclical",  "Internet Retail",                  "USD",
  "APO",      0.09212112,    12.23,       "Financials",         "Asset Management",                 "USD",
  "XEL",      0.31531540,    25.00,       "Utilitis",           "Utilities - Regulated Electricity","USD",
  "CVX",      0.03869370,    7.50,        "Engery",             "Oil & Gas Integrated",             "USD",
  "MU",       0.06911837,    64.65,       "Technology",         "Semiconductors",                   "USD",
  "MRVL",     0.36571317,    88.41,       "Technology",         "Semiconductors",                   "USD",
  "AMD",      0.07717587,    38.20,       "Technology",         "Semiconductors",                   "USD",
  "NOK",      1.69339099,    26.55,       "Technology",         "Communication Equipment",          "USD",
  "PICK",     1.14287357,    72.77,       "ETF",                "Natural Resources",                "USD",
  "FHLC",     1.39885814,    108.85,       "ETF",                "Health",                           "USD",
  "UNP",      0.06437752,    17.38,       "Industrials",        "Railways",                      "USD"
)

# FX rates: convert AUD and NZD prices/costs to USD --------------------------
# Fetches the latest available rate from Yahoo Finance
fx_rates <- tq_get(c("NZDUSD=X", "AUDUSD=X"), get = "stock.prices", from = Sys.Date() - 5) %>%
  group_by(symbol) %>%
  slice_tail(n = 1) %>%
  transmute(
    currency  = dplyr::recode(symbol, "NZDUSD=X" = "NZD", "AUDUSD=X" = "AUD"),
    fx_to_usd = close
  )

# Convert total_cost to USD in-place
portfolio_data <- portfolio_data %>%
  left_join(fx_rates, by = "currency") %>%
  mutate(
    fx_to_usd  = replace_na(fx_to_usd, 1.0),
    total_cost = total_cost * fx_to_usd
  ) %>%
  dplyr::select(-fx_to_usd)

# latest_prices: convert close to USD for display
latest_prices <- purrr::map_dfr(portfolio_data$ticker, function(tk) {
  tryCatch(
    tq_get(tk, get = "stock.prices", from = Sys.Date() - 5) %>%
      dplyr::mutate(ticker = tk),
    error = function(e) {
      message("Failed to fetch latest price: ", tk)
      tibble::tibble()
    }
  )
}) %>%
  dplyr::group_by(ticker) %>%
  dplyr::filter(!is.na(close)) %>%
  dplyr::slice_tail(n = 1) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    portfolio_data %>% dplyr::select(ticker, currency),
    by = "ticker"
  ) %>%
  dplyr::left_join(fx_rates, by = "currency") %>%
  dplyr::mutate(
    fx_to_usd = replace_na(fx_to_usd, 1.0),
    close     = close * fx_to_usd
  ) %>%
  dplyr::select(ticker, close)