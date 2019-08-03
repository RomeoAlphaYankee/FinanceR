install.packages("remotes")
install.packages("devtools")
install.packages("FinancialInstrument")

devtools::install_github("braverock/blotter")
devtools::install_github("braverock/quantstrat")

library(quantmod)
library(TTR)
library(quantstrat)

# S&P 500 ETF data
getSymbols("SPY", from = "2000-01-01", src = "yahoo")

# Plot
plot(Cl(SPY["2018/"]))

# Plot moving averages
sma200 <- SMA(x = Cl(SPY), n = 200)
lines(sma200["2018/"], col = "red")
sma50 <- SMA(Cl(SPY), n = 50)
lines(sma50["2018/"], col = "blue")

# Plot bollinger bands
bands <- BBands(SPY["2018/", 2:4])
plot(Cl(SPY["2018/"]))
lines(bands$d, col = "red")
lines(bands$up, col = "red")
lines(bands$mavg, col = "blue")


# It takes fewer lines of code to do it the other way around
plot(bands["2018/", 1:3], col = c("green", "blue", "red"))
lines(SPY["2018/", 4], col = "black")

# Relative Strength Indicator plot
spy.rsi <- RSI(Cl(SPY["2018/"]))
plot(x = index(SPY["2018/"]), y = as.numeric(spy.rsi), type = "l")
abline(h = c(30, 70), lty = 2, col = "red")

# Initialize quantstrat parameters
# Set initial dates for backtest
initdate <- "1999-01-01"
from <- "2003-01-01"
to <- "2015-12-31"

# Initialize environment
Sys.setenv(TZ = "UTC")
currency("USD")

stock("SPY", currency = "USD")

# Name account, portfolio, strategy
rm.strat(strategy.st)
strategy.st <- portfolio.st <- account.st <- "firststrat"

tradesize <- 100000
initeq <- 100000

# Initialize account and portfolios
initPortf(portfolio.st, symbols = "SPY", initDate = initdate, currency = "USD")
initAcct(account.st, portfolios = portfolio.st, initDate = initdate, currency = "USD", initEq = initeq)
initOrders(portfolio.st, initDate = initdate)
strategy(strategy.st, store = TRUE)

# Add trading indicators
add.indicator(strategy = strategy.st, name = "SMA",
              arguments = list(x = quote(Cl(mktdata)), n = 200),
              label = "SMA200")

# Add a 50-day SMA indicator to strategy.st
add.indicator(strategy = strategy.st, name = "SMA", 
              arguments = list(x = quote(Cl(mktdata)), n = 50), 
              label = "SMA50")

# Add a short-term relative strength indicator
add.indicator(strategy = strategy.st, name = "RSI",
              arguments = list(price = quote(Cl(mktdata)), n = 3),
              label = "RSI_3")

# Write a custom function for a short term RSI indicator
calc_RSI_avg <- function(price, n1, n2){
  RSI_1 <- RSI(price = price, n = n1)
  RSI_2 <- RSI(price = price, n = n2)
  RSI_avg <- (RSI_1 + RSI_2) / 2
  colnames(RSI_avg) <- "RSI_avg"
  return(RSI_avg)
}

# Add the custom function as an indicator
add.indicator(strategy = strategy.st, name = "calc_RSI_avg", 
              arguments = list(price = quote(Cl(mktdata)), n1 = 3, n2 = 4),
              label = "RSI_3_4")

# Write a function to calculate the David Varadi Oscillator
DVO <- function(HLC, navg = 2, percentlookback = 126) {
  ratio <- Cl(HLC)/((Hi(HLC) + Lo(HLC))/2)
  avgratio <- SMA(ratio, n = navg)
  out <- runPercentRank(avgratio, n = percentlookback, exact.multiplier = 1) * 100
  colnames(out) <- "DVO"
  return(out)
}

# Add the DVO indicator to strategy
add.indicator(strategy = strategy.st, name = "DVO", 
              arguments = list(HLC = quote(HLC(mktdata)), navg = 2, percentlookback = 126), 
              label = "DVO_2_126")

# Test the indicators
test <- applyIndicators(strategy = strategy.st, mktdata = OHLC(SPY))
head(test)

# Add a signal comparison to filter potential triggers for transactions
add.signal(strategy = strategy.st, name = "sigComparison",
           arguments = list(columns = c("SMA.SMA50", "SMA.SMA200"), relationship = "gt"),
           label = "longfilter")

# Add a crossover signal indicator
add.signal(strategy = strategy.st, name = "sigCrossover",
           arguments = list(columns = c("SMA.SMA50", "SMA.SMA200"), 
                            relationship = "lt"), 
           label = "filterexit")

# Implement a sigThreshold which specifies that DVO_2_126 must be under 20
add.signal(strategy = strategy.st, name = "sigThreshold",
           arguments = list(column = "DVO_2_126", 
                             threshold = 20,
                             relationship = "lt",
                             cross = FALSE),
           label = "longthreshold")

# Add a sigThreshold to create a signal when the DVO 2 126 goes above 80
add.signal(strategy.st, name = "sigThreshold", 
           arguments = list(column = "DVO_2_126", threshold = 80, 
                            relationship = "gt", cross = TRUE), 
           label = "thresholdexit")

# Test indicators and signals
test_init <- applyIndicators(strategy = strategy.st, mktdata = OHLC(SPY))
test <- applySignals(strategy = strategy.st, mktdata = test_init)

tail(test)

# Add a compound signal using sigFormula
add.signal(strategy = strategy.st, name = "sigFormula",
           arguments = list(formula = "longfilter & longthreshold", cross = TRUE),
           label = "longentry")

# Add rules to execute trades based on signals
add.rule(strategy = strategy.st, name = "ruleSignal", 
         arguments = list(sigcol = "filterexit", sigval = TRUE, orderqty = "all", 
                          ordertype = "market", orderside = "long",
                          replace = FALSE, prefer = "Open"),
         type = "exit")

add.rule(strategy = strategy.st, name = "ruleSignal", 
         arguments = list(sigcol = "thresholdexit", sigval = TRUE, orderqty = "all", 
                          ordertype = "market", orderside = "long",
                          replace = FALSE, prefer = "Open"),
         type = "exit")

# Establish an order sizing function
osMaxDollar <- function(data, timestamp, orderqty, ordertype, orderside,
                        portfolio, symbol, prefer = "Open", tradeSize,
                        maxSize, integerQty = TRUE,
                        ...) {
  pos <- getPosQty(portfolio, symbol, timestamp)
  if(prefer == "Close") {
    price <- as.numeric(Cl(mktdata[timestamp,]))
  } else {
    price <- as.numeric(Op(mktdata[timestamp,]))
  }
  posVal <- pos*price
  if (orderside=="short") {
    dollarsToTransact <- max(tradeSize, maxSize-posVal)
    #If our position is profitable, we don't want to cover needlessly.
    if(dollarsToTransact > 0) {dollarsToTransact = 0}
  } else {
    dollarsToTransact <- min(tradeSize, maxSize-posVal)
    #If our position is profitable, we don't want to sell needlessly.
    if(dollarsToTransact < 0) {dollarsToTransact = 0}
  }
  qty <- dollarsToTransact/price
  if(integerQty) {
    qty <- trunc(qty)
  }
  return(qty)
}


# Add an entry rule using the MaxDollar function
add.rule(strategy = strategy.st, name = "ruleSignal", 
         arguments = list(sigcol = "longentry", sigval = TRUE, 
                          ordertype = "market", orderside = "long",
                          replace = FALSE, prefer = "Open", 
                          osFUN = osMaxDollar,
                          tradeSize = tradesize,
                          maxSize = tradesize),
         type = "enter")

