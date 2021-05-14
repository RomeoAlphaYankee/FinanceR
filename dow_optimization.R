library(tidyverse)
library(tidyquant)
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)
library(DEoptim)

# Download the symbols for the Dow Jones Index
DJI <- tq_index("DOW")
DJI

DJI %>% group_by(sector) %>% count()

DJI %>% group_by(sector) %>% count() %>%
  ggplot(aes(x = "", y = n, fill = sector)) + 
  geom_bar(stat = "identity", width = 1, color = "white") + 
  coord_polar("y", start = 0) +
  theme_void() +
  ggtitle("Dow Jones Industrial Average Sectors")

DJI %>% group_by(sector) %>% 
  summarize(tot_weight = sum(weight)) %>% 
  arrange(desc(tot_weight))

dow_tickers <- DJI$symbol

from <- "2015-01-01"
to <- Sys.Date()

# Get price data
# Note that GE is added back to index and the recently formed DOW Chemical is removed due to lack  
# of historical data for DOW.
dow <- tq_get(c(dow_tickers, "GE", "RTX", "XOM", ""), get = "stock.prices", from = from, to = to)

# Extract DJIA weights
dow.weights <- DJI %>% select(symbol, weight)

# dow.weights[dow.weights$symbol == "DOW", 1] <- "GE"
rbind(dow.weights, c("GE", 0))

head(dow.weights)
tail(dow.weights)

dow.weights <- arrange(dow.weights, symbol) %>% column_to_rownames(var = "symbol")

# Retreive a risk free rate
treas <- tq_get("^IRX", get = "stock.prices", from = from, to = to)
daily.treas <- treas %>% select(date, adjusted) %>% 
  mutate(return = (1 + (adjusted / 100))^(1/252) - 1) %>%
  select(date, return)

dow.prices <- dow %>% select(date, symbol, adjusted) %>% 
  spread(symbol, adjusted) %>%
  column_to_rownames(var = "date")

# Calculate returns, convert to xts
dow.returns <- Return.calculate(as.xts(dow.prices, order.by = as.Date(rownames(dow.prices))))

dow.returns <- dow.returns[complete.cases(dow.returns)]

# Remove any prior portfolio specifications
rm(port_spec)

# Creat a new portfolio specification
port_spec <- portfolio.spec(colnames(dow.returns))

port_spec <- add.constraint(portfolio = port_spec,
                            type = "full_investment")

port_spec <- add.constraint(portfolio = port_spec,
                            type = "long_only")

port_spec <- add.objective(portfolio = port_spec,
                           type = "return", 
                           name = "mean")

port_spec <- add.objective(portfolio = port_spec,
                           type = "risk",
                           name = "ETL")

port_spec <- add.constraint(portfolio = port_spec, type = "box", 
                            min = 0,
                            max = 0.066)

opt <- optimize.portfolio.rebalancing(R = dow.returns, portfolio = port_spec,
                                      rebalance_on = "weeks",
                                      training_period = 126, rolling_window = 126,
                                      optimize_method = "ROI")

# Plot weights of optimized portfolio
chart.Weights(opt)
tail(extractWeights(opt))

# Calculate portfolio returns
dow.port.returns <- Return.rebalancing(R = dow.returns, weights = extractWeights(opt), rebalance_on = "weeks")

# Chart results
chart.CumReturns(dow.port.returns)

# Download DJIA returns
djia <- getSymbols("^DJI", from = from, to = to, auto.assign = FALSE)
djia.returns <- na.trim(Return.calculate(Ad(djia)))

# Plot comparison
plot(cumprod(1 + dow.port.returns) - 1, col = "blue", main = "Optimized Portfolio vs. DJIA")
lines(cumprod(1 + djia.returns["2019-09-23/"]) -1, col = "red", lwd = 2)

# Chart performance summary, including drawdowns
charts.PerformanceSummary(na.omit(merge(dow.port.returns, djia.returns)))

# Chart performance vs. benchmark
SharpeRatio.annualized(na.omit(merge(dow.port.returns, djia.returns)), Rf = 0.005/252, scale = 252)