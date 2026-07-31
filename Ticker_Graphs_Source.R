if (!exists("portfolio_data")) {
  source(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "Portfolio_Data_Source.R"))
}

# Graphs ------------------------------------------------------------------
safe_get_symbol <- function(tk, from = "2026-04-01") {
  tryCatch(
    {
      quantmod::getSymbols(tk, from = from, to = Sys.Date(), auto.assign = TRUE)
      TRUE
    },
    error = function(e) {
      message("Failed to fetch chart data: ", tk)
      FALSE
    }
  )
}
purrr::walk(portfolio_data$ticker, safe_get_symbol)

missing_tickers <- portfolio_data$ticker[
  !portfolio_data$ticker %in% ls(.GlobalEnv)
]

if(length(missing_tickers) > 0) {
  message(
    "Missing price data for: ",
    paste(missing_tickers, collapse = ", ")
  )
}

portfolio_plot_data <- portfolio_data %>% 
  mutate(
    avg_cost = total_cost / shares,
    last_close_price = purrr::map_dbl(
      ticker,
      ~{
        if (!exists(.x)) {
          return(NA_real_)
        }
        
        tryCatch(
          as.numeric(last(Cl(get(.x)))),
          error = function(e) NA_real_
        )
      }
    ),
    pct_diff_price = (last_close_price - avg_cost) / avg_cost * 100
  )
portfolio_plot_data <- portfolio_plot_data %>%
  filter(!is.na(last_close_price))
#Graph
plot_ticker <- function(t, from = "2026-04-01") {
  ok <- safe_get_symbol(t, from)
  
  if (!ok || !exists(t)) {
    stop("No chart data available for ", t, ". Try again later.")
  }
  
  row <- portfolio_data %>% 
    dplyr::filter(ticker == t) %>%
    dplyr::mutate(avg_cost = total_cost / shares)
  
  avg_cost <- row$avg_cost[1]
  
  chartSeries(
    get(t),
    theme = chartTheme("white"),
    name = sprintf("%s | Avg Cost: $%.2f", t, avg_cost)
  )
  
  addLines(h = avg_cost, col = "red")
}