# These are a couple of simple exploratory trading strategies utilizing tidyquant to quickly 
# investigate a strategy before building out in a more robust package like quantstrat.
# They do not take into consideration tradidng costs, or the fact that returns may not be achieved
# due to lag between signal and transaction. 

# Charts are plotted in base R, and using ggplot
# Interactive charts are produced in highcharter

library(tidyverse)
library(tidyquant)
library(timetk)
library(tibbletime)
library(scales)
library(highcharter)
library(broom)
library(PerformanceAnalytics)

# Retreive S&P 500 index values as well as treasurty yields
symbols <- c("^GSPC", "^IRX")
prices <- tq_get(symbols, get = "stock.prices", from = "1990-01-01")

head(prices)
tail(prices)

# Format dates
prices <- mutate(prices, date = ymd(date))

# Javascript interactive chart of returns with highcharter
prices %>% filter(symbol == "^GSPC") %>%
  hchart(., 
         hcaes(x = date, y = adjusted),
         type = "line") %>%
  hc_title(text = "GSPC prices")

# Spread the data from a single symbols column to individual columns for each price series,
# then mutate the price series into returns
returns <- prices %>%
  select(symbol, date, adjusted) %>%
  spread(symbol, adjusted) %>%
  rename(sp500 = "^GSPC", treas = "^IRX") %>%
  mutate(sp500_returns = log(sp500) - lag(log(sp500)), daily_treas = (1 + (treas / 100))^(1 / 252) - 1)

# Add SMA indicators, drop na's, and add signals based on indicators
# Buy S&P 500 if the trading signal was triggered on the prior day, 
# otherwise stay in treasuries.
# Also added a simply buy and hold strategy for comparison.
returns <- 
  mutate(returns, 
         sma200 = SMA(sp500, n = 200), 
         sma50 = SMA(sp500, n = 50)) %>%
  na.omit %>%
  mutate(signal = if_else(sma50 > sma200, 1, 0),
         trend_returns = if_else(lag(signal) == 1, sp500_returns, daily_treas),
         buy_hold_returns = 0.9 * sp500_returns + 0.1 * daily_treas)

# Inspect the data
head(returns, 10)

# Check annualized return table
returns %>% 
  tq_performance(Ra = sp500_returns, performance_fun = table.AnnualizedReturns, Rf = mean(returns$treas / 100) / 252)

# Check stats
returns %>% 
  tq_performance(sp500_returns, performance_fun = table.Stats) %>% t()

# Plot the daily returns
returns %>% ggplot(aes(x = date, y = sp500_returns)) + 
  geom_point(color = "cornflowerblue") + 
  scale_x_date(breaks = pretty_breaks(n = 30)) + 
  labs(title = "S&P 500 Daily Returns",
      y = "Daily % Change") + 
  theme(axis.text.x = element_text(angle = 90))

# Investigate the proportion of buy signals
returns %>%
  select(date, signal, sma200, sma50)%>%  
  summarise(buy_proportion = mean(signal))

# Chart the performance summary
charts.PerformanceSummary(xts(returns$trend_returns, order.by = returns$date))

# This is informative
# Plot the strategy returns vs. a buy and hold strategy 90% invested in the market, in base R
plot(x = returns$date[-1], y = cumprod(1 + returns$trend_returns[-1]) - 1, type = "l", col = "blue", 
     main = "Trend Strategy vs. S&P 500", xlab = "date", ylab = "Cumulative Returns")
lines(x = returns$date[-1], y = cumprod(1 + returns$buy_hold_returns[-1]) - 1, type = "l", col = "black")

# Create a table of risk metrics
risks <- returns %>% tq_performance(Ra = trend_returns, performance_fun = table.DownsideRisk)
risks[2, ] <- returns %>% tq_performance(Ra = sp500_returns, performance_fun = table.DownsideRisk)
rownames(risks) <- c("Trend", "SP500")
t(risks)

# Recreate the base R plot in a javascript interactive format using hicharter
# First calculate the cumulative change in $1
returns <- returns %>% na.omit %>%
  mutate(trend_growth = cumprod(1 + trend_returns),
         buy_hold_growth = cumprod(1 + buy_hold_returns))

# Then plot
returns %>% 
  select(date, trend_growth, buy_hold_growth) %>%
  gather(strategy, growth, -date) %>%
  hchart(., hcaes(x = date, y = growth, group = strategy), 
         type = "line") %>%
  hc_title(text = "Growth of $1, Strategy Returns vs. Buy & Hold") %>%
  hc_tooltip(pointFormat = "{point.strategy}: ${point.growth: .2f}")

# Redo the strategy incorporating a z-score
trend_z_results <- prices %>% 
  na.locf() %>%
  select(date, symbol, adjusted) %>%
  spread(symbol, adjusted) %>%
  rename(sp500 = "^GSPC", treas = "^IRX") %>%
  mutate(sma_200 = SMA(sp500, n = 200),
         sma_50 = SMA(sp500, n = 50),
         sp500_returns = log(sp500) - lag(log(sp500)),
         daily_treas = (1 + (treas / 100))^(1/252) - 1) %>%
  na.omit() %>%
  mutate(trend_signal = if_else(sma_50 > sma_200, 1, 0),
         z_spread = sp500 - sma_200,
         z_score = z_spread / sd(z_spread),
         z_signal = if_else(lag(z_score, 1) > 2 & 
                              lag(z_score, 2) > 2, 
                            0, 1),
         trend_z_returns = if_else(lag(trend_signal) == 1 & z_signal == 1, 
                                 sp500_returns, daily_treas),
         trend_returns = if_else(lag(trend_signal) == 1, 
                                 sp500_returns, daily_treas)) %>%
  na.omit() %>% 
  mutate(
    sp500_growth = cumprod(1 + sp500_returns),
    trend_z_growth = cumprod(1 + trend_z_returns),
    trend_growth = cumprod(1 + trend_returns))
  
# Then plot
trend_z_results %>% 
  select(date, sp500_growth, trend_growth, trend_z_growth) %>%
  gather(strategy, growth, -date) %>%
  hchart(., hcaes(x = date, y = growth, group = strategy), 
         type = "line") %>%
  hc_title(text = "Growth of $1, Strategy vs. S&P 500") %>%
  hc_tooltip(pointFormat = "{point.strategy}: ${point.growth: .2f}")

# Chart performance summary
charts.PerformanceSummary(xts(trend_z_results$trend_z_returns, order.by = trend_z_results$date))


# Create a table of downside risks
risks_z <- trend_z_results %>% 
  tq_performance(Ra = trend_z_returns, performance_fun = table.DownsideRisk)

risks_z <- rbind(risks_z, tq_performance(trend_z_results, trend_returns, performance_fun = table.DownsideRisk))
risks_z <- rbind(risks_z, tq_performance(trend_z_results, sp500_returns, performance_fun = table.DownsideRisk))

rownames(risks_z) <- c("trend_z", "trend", "sp500")
t(risks_z)
