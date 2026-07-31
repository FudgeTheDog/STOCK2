
if (!exists("portfolio_full")) {
  source(file.path(dirname(rstudioapi::getSourceEditorContext()$path), "Connor_Sharesies_Source.R"))
}
# Pie Charts --------------------------------------------------------------

portfolio_by_ticker <- portfolio_full %>% 
  mutate(ticker = reorder(ticker, current_worth)) %>% 
  ggplot(aes(x = 1, y = current_worth, fill = ticker)) +
  geom_col(color = "white") + 
  coord_polar(theta = "y") +
  theme_void() + 
  labs(title = "Portfolio Weight Distribution") + 
  geom_text(aes(label = scales::percent(current_worth/sum(current_worth), 
                                        accuracy = 0.1)),
            position = position_stack(vjust = 0.5))

#Tickers Rest
Ticker_by_rest <- portfolio_full %>%
  mutate(ticker_grouped = ifelse(weight < 0.03, "Rest", ticker)) %>%
  group_by(ticker_grouped) %>%
  summarise(worth = sum(current_worth), .groups = "drop") %>%
  mutate(pct = worth / sum(worth),
         ticker_grouped = reorder(ticker_grouped, worth)) %>%
  mutate(ticker = reorder(ticker_grouped, worth)) %>% 
  ggplot(aes(x = 1, y = worth, fill = ticker_grouped)) +
  geom_col(color = "white") + 
  coord_polar(theta = "y") +
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)), 
            position = position_stack(vjust = 0.5), size = 3) +
  theme_void() +
  labs(title = "Portfolio Weight by Ticker", fill = "Ticker")

#Sector
portfolio_by_sector <- portfolio_full%>%
  group_by(sector) %>%
  summarise(worth = sum(current_worth), .groups = "drop") %>%
  mutate(pct = worth / sum(worth),
         sector = reorder(sector, worth),
         label = paste0(sector, "\n", 
                        scales::percent(pct, accuracy = 0.1))) %>%
  ggplot(aes(x = 1, y = worth, fill = sector)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), 
            position = position_stack(vjust = 0.5), size = 3) +
  theme_void() +
  labs(title = "Portfolio Weight by Sector") +
  theme(legend.position = "none")

#Industry#Isectorndustry
portfolio_by_industry <- portfolio_full %>%
  group_by(industry) %>%
  summarise(worth = sum(current_worth), .groups = "drop") %>%
  mutate(pct = worth / sum(worth)) %>%
  mutate(industry = reorder(industry, worth)) %>% 
  ggplot(aes(x = 1, y = worth, fill = industry)) +
  geom_col(color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = scales::percent(pct, accuracy = 0.1)), 
            position = position_stack(vjust = 0.5), size = 3) +
  theme_void() +
  labs(title = "Portfolio Weight by Industry")


