library(quantmod)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(lubridate)
library(TTR)

# Establish a timeframe
from <- ymd("2018-06-19") # first day of XLC introduction
to <- Sys.Date()

# Benchmark ETF
getSymbols("SPY", from = from, to = to)

SPY <- Return.calculate((Ad(SPY)))
SPY <- SPY[-1, ]
colnames(SPY) <- "SPY"

head(SPY)
tail(SPY)

# Ticker list for sector spyders
tickers <- c("XLK", "XLV", "XLF", "XLC", "XLY", "XLI", "XLP", "XLU", "XLE", "XLRE", "XLB", "TLT")

# create a list of prices
prices <- xts(order.by = index(SPY))

for(ticker in tickers){
  temp <- getSymbols(ticker, auto.assign = FALSE, from = from, to = to)
  prices <- merge(prices, Ad(temp))
}

names(prices) <- tickers

# Empty xts object to hold prices and returns
returns <- xts(order.by = (index(SPY)))

# For loop to iterate over vector of tickers
for(ticker in tickers){
  temp <- Return.calculate(prices[ , ticker])
  returns <- merge(returns, temp)
}

# fix portfolio names
names(returns) <- tickers
returns <- returns[-1, ]

head(returns)
tail(returns)

# Create a table of Sector RSI
sector.rsi <- xts(order.by = index(SPY))

for(ticker in tickers){
  rsi <- RSI(prices[ , ticker])
  sector.rsi <- merge(sector.rsi, rsi)
}

names(sector.rsi) <- tickers

tail(sector.rsi)

# set up a chart
par(mfrow = c(3, 4))

plot.zoo(sector.rsi$XLK["2021"], ylim = c(1, 100), col = "blue", lwd = 2, main = "Technology RSI")
abline(h = 70, col = "red", lty = 2)
abline(h = 30, col = "green", lty = 2)

plot.zoo(sector.rsi$XLV["2021"], ylim = c(1, 100), col = "blue", lwd = 2, main = "Healthcare RSI")
abline(h = 70, col = "red", lty = 2)
abline(h = 30, col = "green", lty = 2)

plot.zoo(sector.rsi$XLF["2021"], ylim = c(1, 100), col = "blue", lwd = 2, main = "Financials RSI")
abline(h = 70, col = "red", lty = 2)
abline(h = 30, col = "green", lty = 2)

plot.zoo(sector.rsi$XLC["2021"], ylim = c(1, 100), col = "blue", lwd = 2, main = "Communications RSI")
abline(h = 70, col = "red", lty = 2)
abline(h = 30, col = "green", lty = 2)

plot.zoo(sector.rsi$XLY["2021"], ylim = c(1, 100), col = "blue", lwd = 2, main = "Discrationary RSI")
abline(h = 70, col = "red", lty = 2)
abline(h = 30, col = "green", lty = 2)

plot.zoo(sector.rsi$XLI["2021"], ylim = c(1, 100), col = "blue", lwd = 2, main = "Industrials RSI")
abline(h = 70, col = "red", lty = 2)
abline(h = 30, col = "green", lty = 2)

plot.zoo(sector.rsi$XLP["2021"], ylim = c(1, 100), col = "blue", lwd = 2, main = "Staples RSI")
abline(h = 70, col = "red", lty = 2)
abline(h = 30, col = "green", lty = 2)

plot.zoo(sector.rsi$XLU["2021"], ylim = c(1, 100), col = "blue", lwd = 2, main = "Utilities RSI")
abline(h = 70, col = "red", lty = 2)
abline(h = 30, col = "green", lty = 2)

plot.zoo(sector.rsi$XLE["2021"], ylim = c(1, 100), col = "blue", lwd = 2, main = "Energy RSI")
abline(h = 70, col = "red", lty = 2)
abline(h = 30, col = "green", lty = 2)

plot.zoo(sector.rsi$XLRE["2021"], ylim = c(1, 100), col = "blue", lwd = 2, main = "Real Estate RSI")
abline(h = 70, col = "red", lty = 2)
abline(h = 30, col = "green", lty = 2)

plot.zoo(sector.rsi$XLB["2021"], ylim = c(1, 100), col = "blue", lwd = 2, main = "Materials RSI")
abline(h = 70, col = "red", lty = 2)
abline(h = 30, col = "green", lty = 2)

plot.zoo(sector.rsi$TLT["2021"], ylim = c(1, 100), col = "blue", lwd = 2, main = "Treasury RSI")
abline(h = 70, col = "red", lty = 2)
abline(h = 30, col = "green", lty = 2)
