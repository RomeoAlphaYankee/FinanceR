library(quantmod)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(lubridate)
library(ROI)
library(DEoptim)

# Establish a timeframe
from <- ymd("2018-06-19") # first day of XLC introduction
to <- Sys.Date()

# Benchmark ETF
getSymbols("SPY", from = from, to = to)

SPY <- Return.calculate(Ad(SPY[endpoints(SPY, on = "months")]))
colnames(SPY) <- "SPY"

head(SPY)
tail(SPY)

# Ticker list for sector spyders
tickers <- c("XLK", "XLV", "XLF", "XLC", "XLY", "XLI", "XLP", "XLU", "XLE", "XLRE", "XLB", "BIL")

# Empty xts object to hold prices and returns
returns <- xts(order.by = (index(SPY)))

# For loop to iterate over vector of tickers
for(ticker in tickers){
  temp <- getSymbols(ticker, auto.assign = FALSE, from = from, to = to)
  temp <- Return.calculate(Ad(temp[endpoints(temp, on = "months")]))
  returns <- merge(returns, temp)
}

# fix portfolio names
names(returns) <- tickers
returns <- returns[-1, ]

head(returns)

# Assign initial weights from State Street to match that of the SPY
weights <- c(.2449, .1446, .1182, .1045, .0973, .0864, .0757, .0369, .0342, .032, .0253, 0)
names(weights) <- tickers
sum(weights)
weights

# Calculate portfolio expected returns, variance, covariance, and correlation
expected.rtn <- colMeans(returns, na.rm = TRUE) # Monthly expected returns
expected.rtn

((1 + expected.rtn)^12 - 1) # Annualized
((1 + expected.rtn)^12 - 1) * weights # Weighted annualized returns

sum(((1 + expected.rtn)^12 - 1) * weights) # Annualized and weighted expected portfolio total return

apply(returns, 2, FUN = sd, na.rm = TRUE) # Monthly standard deviation
apply(returns, 2, FUN = sd, na.rm = TRUE) * sqrt(12) # Annualized standard deviation

# Calculate Sharpe ratio for each sector
((1 + expected.rtn)^12 - 1) / (apply(returns, 2, FUN = sd, na.rm = TRUE) * sqrt(12))

# Examine correlation
cor(returns)

corrplot::corrplot.mixed(cor(returns))

#####

## Generate a long / short optimized portfolio that shorts specific sectors
# First, remove any residual portfolio specifications
rm(ls.pspec)

# Create a new base portfolio specification
ls.pspec <- portfolio.spec(assets = tickers, 
                           weight_seq = weights)

print.default(ls.pspec)
ls.pspec

# Add objective for return
ls.pspec <- add.objective(portfolio = ls.pspec, 
                       type = 'return', 
                       name = 'mean')

# Add weight constraints
# ls.pspec <- add.constraint(portfolio = ls.pspec, 
#                        type = "dollar_neutral")


# Weight constraint of 0% to 30% short
ls.pspec <- add.constraint(portfolio = ls.pspec, 
                           type = "weight_sum",
                           min_sum = .7,
                           max_sum = 1)

# Add box constraint for each sector from negative index weight to index weight + 5%
ls.pspec <- add.constraint(portfolio = ls.pspec, type = 'box', 
                           min = -weights, 
                           max = weights + 0.05)

# Create a new portfolio specification from the base specification
# Add objective to minimize variance
ls.var.pspec <- add.objective(portfolio = ls.pspec, 
                          type = 'risk',
                          name = 'StdDev')


# Inspect the portfolio object
print.default(ls.var.pspec)

# Run the optimization
ls.var.opt <- optimize.portfolio.rebalancing(R = returns, 
                                             portfolio = ls.var.pspec, 
                                             optimize_method = "DEoptim", 
                                             rebalance_on = 'months', 
                                             training_period = 1,
                                             rolling_window = 6)

# Chart weights
chart.Weights(ls.var.opt,
              c("black", "red", "green", "cornflowerblue", "darkkhaki", "yellow", "grey", "navyblue", "orange", "firebrick", "darkmagenta", "darkolivegreen"))

# Inspect Weights
extractWeights(ls.var.opt)
rowSums(extractWeights(ls.var.opt))
last(extractWeights(ls.var.opt), 3)

# Plot the net long/short weighting
plot(index(extractWeights(ls.var.opt)), 
     rowSums(extractWeights(ls.var.opt)), 
     type = "l", lty = 2, lwd = 2, col = 'red',
     main = "Net Long/Short Weight",
     xlab = "Date", ylab = "Net Long Percentage")

# Calculate long / short portfolio returns
ls.var.prt.rtn <- Return.portfolio(R = returns, weights = extractWeights(ls.var.opt), 
                               rebalance_on = "months")

# Inspect long/short portfolio returns
ls.var.prt.rtn

# Plot vs. S&P 500
chart.CumReturns(ls.var.prt.rtn, 
                 ylim = c(-0.25, .45),
                 main = "Min-Variance Portfolio 30% Short")
lines(cumprod(1 + SPY["2018-08/"]) - 1, col = "green")

# Plot after training period
chart.CumReturns(ls.var.prt.rtn["2019/"], 
                 ylim = c(-0.25, .45),
                 main = "Min-Variance Portfolio 30% Short")
lines(cumprod(1 + SPY["2019/"]) - 1, col = "green")

# Table of annualized returns
table.AnnualizedReturns(ls.var.prt.rtn, Rf = 0.003 / 12)


#####

# Change the portfolio specification to minimize expected tail loss instead of variance
ls.etl.pspec <- add.objective(portfolio = ls.pspec, 
                            type = 'risk',
                            name = 'ETL')

print.default(ls.etl.pspec)

# Run the optimization
ls.etl.opt <- optimize.portfolio.rebalancing(R = returns, 
                                             portfolio = ls.etl.pspec, 
                                             optimize_method = "DEoptim", 
                                             rebalance_on = 'months', 
                                             training_period = 1,
                                             rolling_window = 6)

# Chart weights
chart.Weights(ls.etl.opt,
              c("black", "red", "green", "cornflowerblue", "darkkhaki", "yellow", "grey", "navyblue", "orange", "firebrick", "darkmagenta", "darkolivegreen"))

# Inspect Weights
extractWeights(ls.etl.opt)
rowSums(extractWeights(ls.etl.opt))
last(extractWeights(ls.etl.opt), 3)

# Plot the net long/short weighting
plot(index(extractWeights(ls.etl.opt)), 
     rowSums(extractWeights(ls.etl.opt)), 
     type = "l", lty = 2, lwd = 2, col = 'red',
     main = "Net Long/Short Weight",
     xlab = "Date", ylab = "Net Long Percentage")

# Calculate long / short portfolio returns
ls.etl.prt.rtn <- Return.portfolio(R = portfolio, weights = extractWeights(ls.etl.opt), 
                                   rebalance_on = "months")

# Inspect long/short portfolio returns
ls.etl.prt.rtn

# Plot vs. S&P 500
chart.CumReturns(ls.etl.prt.rtn, 
                 ylim = c(-0.25, .45),
                 main = "Min-Tail Loss Portfolio 30% Short")
lines(cumprod(1 + SPY["2018-08/"]) - 1, col = "green")

# Plot vs. S&P 500 after training period
chart.CumReturns(ls.etl.prt.rtn["2019/"], 
                 ylim = c(-0.25, .45),
                 main = "Min-Tail Loss Portfolio 30% Short")
lines(cumprod(1 + SPY["2019/"]) - 1, col = "green")

# Table returns
table.AnnualizedReturns(ls.var.prt.rtn["2019/"], Rf = 0.003 / 12)
table.AnnualizedReturns(SPY["2019/"], Rf = 0.003 / 12)

#####

# Create an account that owns thelong/short portfolio and keeps collateral in T-Bills
prt.rtn <- merge.xts(ls.var.prt.rtn, as.xts(rowSums(extractWeights(ls.var.opt)[-1]), order.by = index(ls.var.prt.rtn)), portfolio$BIL[-1])
names(prt.rtn) <- c("Port", "Weight", "TBill")

# Calculate the portfolio returns plus T-Bill returns times 1 - net portfolio weight
prt.rtn$Acct <- prt.rtn$Port + prt.rtn$TBill * (1 - prt.rtn$Weight)

# Take a look
prt.rtn

# Chart
chart.CumReturns(SPY["2018-08/"], ylim = c(-0.1, 0.35))
lines(cumprod(1 + prt.rtn$Port) - 1, col = "red", lwd = 2)
lines(cumprod(1 + prt.rtn$Acct) - 1, col = "green", lwd = 2)
addLegend("topleft", legend.names = c("S&P 500", "L/S Port", "Account"),
          lty = 1, lwd = 2, col = c("black", "red", "green"))

# Examine performance
acct.returns <- merge.xts(SPY, prt.rtn[ , c(1, 4)])
acct.returns <- acct.returns[-1, ]
acct.returns[1, ] <- 0

# Chart drawdowns
charts.PerformanceSummary(acct.returns)
table.Drawdowns(acct.returns$SPY)
table.Drawdowns(acct.returns$Acct)

# Performance tables
table.AnnualizedReturns(dn.returns[-1, ], Rf = mean(portfolio$BIL) / 12)

# Calculate the total returns
tot.ret <- function(x){
  tmp <- cumprod(1 + x) - 1
  last(tmp)
}

apply(acct.returns[-1, ], MARGIN = 2, FUN = tot.ret)

chart.RollingPerformance(acct.returns$Acct)

# Histogram of returns
chart.Histogram(R = SPY, breaks = 10, methods = c("add.normal", "add.density"))
chart.Histogram(R = acct.returns$Acct, breaks = 10, methods = c("add.normal", "add.density"))