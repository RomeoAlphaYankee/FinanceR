library(quantmod)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(lubridate)

# Establish a timeframe
from <- ymd("2018-06-19") # first day of XLC introduction
to <- Sys.Date()

# Benchmark ETF
getSymbols("SPY", from = from, to = to)

SPY <- Return.calculate((Ad(SPY[endpoints(SPY, on = "months")])))
SPY <- SPY[-1, ]
colnames(SPY) <- "SPY"

head(SPY)
tail(SPY)

# Ticker list for sector spyders
tickers <- c("XLK", "XLV", "XLF", "XLC", "XLY", "XLI", "XLP", "XLU", "XLE", "XLRE", "XLB", "BIL")

# Empty xts object to hold prices and returns
portfolio <- xts(order.by = (index(SPY)))

# For loop to iterate over vector of tickers
for(ticker in tickers){
  temp <- getSymbols(ticker, auto.assign = FALSE, from = from, to = to)
  temp <- Return.calculate(Ad(temp[endpoints(temp, on = "months")]))
  portfolio <- merge(portfolio, temp)
}

# fix portfolio names
names(portfolio) <- tickers
portfolio <- portfolio[-1, ]

head(portfolio)
tail(portfolio)

# Assign initial weights from State Street to match that of the SPY
weights <- c(.2449, .1446, .1182, .1045, .0973, .0864, .0757, .0369, .0342, .032, .0253, 0)
names(weights) <- tickers
sum(weights)
weights

# Calculate portfolio expected returns, variance, covariance, and correlation
expected.rtn <- colMeans(portfolio, na.rm = TRUE) # Monthly
((1 + expected.rtn)^12 - 1) # Annualized
sum(((1 + expected.rtn)^12 - 1) * weights) # Annualized and weighted

apply(portfolio, 2, FUN = sd, na.rm = TRUE) # Monthly standard deviation
apply(portfolio, 2, FUN = sd, na.rm = TRUE) * sqrt(12) # Annual standard deviation

cor(portfolio["2018-07/"])
corrplot::corrplot.mixed(cor(portfolio["2018-07/2019-12"]))
corrplot::corrplot.mixed(cor(portfolio["2020/"]))


corr.change <- cor(portfolio["2018-07/2019-12"]) - cor(portfolio["2020/"])
max(corr.change)
min(corr.change)


# Calculate returns without reballancing
prt.rtn <- Return.portfolio(R = portfolio, weights = weights)
chart.CumReturns(prt.rtn)

# Plot weighted sector SPDR returns vs. regular SPY ETF
plot((cumprod(1 + prt.rtn) - 1), col = "blue")
lines((cumprod(1 + SPY) - 1), col = "red")

# Cumulative performance difference
plot((cumprod(1 + (prt.rtn - SPY)) - 1), 
     col = "red", main = "Long Sector SPDRS / Short SPY")

# Rebalance on months
prt.rebal <- Return.portfolio(R = portfolio, weights = weights, rebalance_on = "months")

# Plot monthly rebalanced sector SPDR returns vs. regular SPY ETF
plot((cumprod(1 + prt.rebal) - 1), col = "blue")
lines((cumprod(1 + SPY) - 1), col = "red")

# Cumulative performance difference
plot((cumprod(1 + (prt.rebal - SPY)) - 1), 
     col = "red", main = "Long Sector SPDRS / Short SPY")

# Plot returns of the basic rebalancing portfooio
chart.CumReturns(prt.rebal)
charts.PerformanceSummary(prt.rebal)
charts.RollingPerformance(prt.rebal, Rf = 0.005 / 12)
table.DownsideRisk(prt.rebal)
table.Stats(prt.rebal)
chart.Histogram(R = prt.rebal, breaks = 10, methods = c("add.normal", "add.density"))
skewness(prt.rebal)
kurtosis(prt.rebal)


chart.Drawdown(prt.rebal)
table.Drawdowns(prt.rebal)

# Compare returns and Sharpe ratios
table.AnnualizedReturns(SPY, Rf = 0.003 / 12)
table.AnnualizedReturns(prt.rtn, Rf = 0.003 / 12)
table.AnnualizedReturns(prt.rebal, Rf = 0.003 / 12)

# Return calendar
table.CalendarReturns(SPY)
table.CalendarReturns(prt.rtn)
table.CalendarReturns(prt.rebal)

# Historical returns are better for the non-rebalanced portfolio which let the winners run, and
# underweighted the underperformers. 

# Perform a Markowitz mean / variance portfolio optimization on sectors
pspec <- portfolio.spec(assets = tickers, weight_seq = weights)
print.default(pspec)

# Add weight constraints
pspec <- add.constraint(portfolio = pspec, 
                        type = "full_investment")

pspec <- add.constraint(portfolio = pspec, 
                        type = "long_only")

# Add box constraint for each sector between 0 and 1.5X market weight
pspec <- add.constraint(portfolio = pspec, type = 'box', 
                        min = rep(0, length(tickers)), 
                        max = (weights + 0.05))

# Add objective to minimize risk of expected tail loss at 95% confidence
pspec <- add.objective(portfolio = pspec, 
                       type = 'risk',
                       name = 'ETL')

# Add objective for return
pspec <- add.objective(portfolio = pspec, 
                       type = 'return', 
                       name = 'mean')

# Add objective for risk budget of max 40% per asset
# pspec <- add.objective(portfolio = pspec,
#                       type = 'risk_budget', 
#                       name = 'ES',
#                       arguments = list(p = 0.95), max_prisk = 0.4)

# Inspect the portfolio object
print.default(pspec)
pspec

# Load the optimization tools
library(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)

# Run the optimization
prt.opt <- optimize.portfolio.rebalancing(R = portfolio, 
                                          portfolio = pspec, 
                                          optimize_method = "ROI", 
                                          rebalance_on = 'months', 
                                          training_period = 1,
                                          rolling_window = 6)

# Check the results
prt.opt

# Inspect the extracted weights
last(extractWeights(prt.opt))
rowSums(extractWeights(prt.opt)) # Interestingly staying ih cash

chart.Weights(prt.opt,
              c("black", "red", "green", "cornflowerblue", "darkkhaki", "yellow", "grey", "navyblue", "orange", "firebrick", "darkmagenta", "darkolivegreen"))

# Calculate returns with optimized weights
prt.rtn.opt <- Return.portfolio(R = portfolio, weights = extractWeights(prt.opt), 
                                rebalance_on = "months")

chart.CumReturns(prt.rtn.opt)

# Plot returns vs. SPY
plot((cumprod(1 + SPY["2018-08/"]) - 1), col = "blue",
     ylim = c(-0.5, 0.5),
     main = "SPY vs. Optimized Sector Portfolio")
lines((cumprod(1 + prt.rtn.opt) - 1), col = "red")

# Plot optimized portfolio vs. Spirit of America Value Fund 
# First get SOAVX adjusted returns.
getSymbols("SOAVX", from = from, to = to)
SOAVX <- Return.calculate(Ad(SOAVX[endpoints(SOAVX, on = "months")]))[-1, ]
names(SOAVX) <- "SOAVX"
SOAVX

# Create the plot
plot((cumprod(1 + SPY["2018-08/"]) - 1), col = "blue", 
     ylim = c(-0.2, 0.35),
     main = "Optimized Portfolio vs. S&P 500 vs. SOAVX")
lines((cumprod(1 + prt.rtn.opt) - 1), col = "green", lwd = 2)
lines((cumprod(1 + SOAVX["2018-08/"]) - 1), col = "red")
addLegend("topleft", legend.names = c("Optimized", "S&P 500", "SOAVX"),
          lty = 1, lwd = 2, col = c("green", "blue", "red"))

# Check the annualized returns and Sharpe ratios
table.AnnualizedReturns(prt.rtn.opt, Rf = 0.003 / 12)
table.AnnualizedReturns(SPY, Rf = 0.003 / 12)
table.AnnualizedReturns(SOAVX, Rf = 0.003 / 12)

# Calculate the total returns
tot.ret <- function(x){
  tmp <- cumprod(1 + x) - 1
  last(tmp)
}

returns <- merge(prt.rtn.opt, SPY, SOAVX)

returns[1, ] <- 0

apply(returns, 2, FUN = tot.ret)

# Recreate the plot with 0 in first month
plot((cumprod(1 + returns$SPY) - 1), col = "blue", 
     ylim = c(-0.2, 0.35),
     main = "Optimized Portfolio vs. S&P 500 vs. SOAVX")
lines((cumprod(1 + returns$portfolio.returns) - 1), col = "green", lwd = 2)
lines((cumprod(1 + returns$SOAVX) - 1), col = "red")
addLegend("topleft", legend.names = c("Optimized", "S&P 500", "SOAVX"),
          lty = 1, lwd = 2, col = c("green", "blue", "red"))

# Check the Sharpe ratios
apply(returns[-1, ], 2, table.AnnualizedReturns)

# Chart the performance summaries
charts.PerformanceSummary(returns)

# Drawdowns
table.Drawdowns(prt.rtn.opt)
table.Drawdowns(SOAVX)

# Cumulative performance difference between optimized portfolio and SPY
plot((cumprod(1 + (prt.rtn.opt - SPY * 0.3)) - 1), 
     col = "red", main = "Long Optimized Sector SPDRS / Short SPY")

# Run a long short portfolio that is 30% short. On the date of the travel ban shift to 100% 
# short, otherwise known as market neutral

ls.prt.rtn <- merge(prt.rtn.opt, SPY)
ls.prt.rtn$weights["2018/2019"] <- 0.3
ls.prt.rtn$weights["2020/"] <- 1.0
ls.prt.rtn$sp.wgt <- ls.prt.rtn$SPY * ls.prt.rtn$weights
ls.prt.rtn$active.rtn <- ls.prt.rtn$portfolio.returns - ls.prt.rtn$sp.wgt

chart.CumReturns(ls.prt.rtn$active.rtn, main = "Long Optimized / 30% Short / 100% Short")

# plot(cumprod(1 + ls.prt.rtn$active.rtn) - 1, col = "green")
lines(cumprod(1 + SPY) - 1, col = "red")
addLegend("topleft", legend.names = c("Optimized L/S", "S&P 500"),
          lty = 1, lwd = 2, col = c("black", "red"))


table.AnnualizedReturns(SPY, Rf = 0.004 / 12)
table.AnnualizedReturns(ls.prt.rtn$active.rtn, Rf = 0.004 / 12)


## Generate a long / short optimized portfolio that shorts specific sectors
# First remove the portfolio specification object
rm(pspec)

# Load DEoptim in the event we use a risk budget, StdDev, or another constraint not supported by ROI
library(DEoptim)

# Reset the portfolio specification
rm(ls.pspec)

ls.pspec <- portfolio.spec(assets = tickers[-length(tickers)], 
                           weight_seq = weights[-length(weights)])

print.default(ls.pspec)
ls.pspec

# Add weight constraints
# ls.pspec <- add.constraint(portfolio = ls.pspec, 
#                        type = "dollar_neutral")

ls.pspec <- add.constraint(portfolio = ls.pspec, 
                          type = "weight_sum",
                          min_sum = -.1,
                          max_sum = .1)

# Add box constraint for each sector between 1.5X and -1.5X market weight
ls.pspec <- add.constraint(portfolio = ls.pspec, type = 'box', 
                         min = -.25, 
                         max = .25)

# Add objective to minimize risk of expected tail loss at 95% confidence
ls.pspec <- add.objective(portfolio = ls.pspec, 
                       type = 'risk',
                       name = 'StdDev')

# Add objective for return
ls.pspec <- add.objective(portfolio = ls.pspec, 
                       type = 'return', 
                       name = 'mean')

# Inspect the portfolio object
print.default(ls.pspec)

# Run the optimization
ls.prt.opt <- optimize.portfolio.rebalancing(R = portfolio, 
                                          portfolio = ls.pspec, 
                                          optimize_method = "DEoptim", 
                                          rebalance_on = 'months', 
                                          training_period = 1,
                                          rolling_window = 6)

# Chart weights
chart.Weights(ls.prt.opt,
              c("black", "red", "green", "cornflowerblue", "darkkhaki", "yellow", "grey", "navyblue", "orange", "firebrick", "darkmagenta", "darkolivegreen"))

# Inspect Weights
extractWeights(ls.prt.opt)
rowSums(extractWeights(ls.prt.opt))
last(extractWeights(ls.prt.opt), 3)
plot(index(extractWeights(ls.prt.opt)), rowSums(extractWeights(ls.prt.opt)), type = "l")

# Calculate long / short portfolio returns
ls.prt.rtn <- Return.portfolio(R = portfolio, weights = extractWeights(ls.prt.opt), 
                               rebalance_on = "months")

chart.CumReturns(ls.prt.rtn, 
                 ylim = c(-0.15, .35))
lines(cumprod(1 + SPY) - 1, col = "green")

table.AnnualizedReturns(ls.prt.rtn, Rf = 0.004 / 12)

# Create an account that owns the dollar neutral long/short portfolio and keeps collateral in T-Bills
dn.prt.rtn <- merge.xts(ls.prt.rtn, as.xts(rowSums(extractWeights(ls.prt.opt)[-1]), order.by = index(ls.prt.rtn)), portfolio$BIL[-1])
names(dn.prt.rtn) <- c("Port", "Weight", "TBill")

# Calculate the portfolio returns plus T-Bill returns times 1 - net portfolio weight
dn.prt.rtn$Acct <- dn.prt.rtn$Port + dn.prt.rtn$TBill * (1 - dn.prt.rtn$Weight)

# Take a look
dn.prt.rtn

# Chart
chart.CumReturns(SPY[-1], ylim = c(-0.1, 0.35))
lines(cumprod(1 + dn.prt.rtn$Port) - 1, col = "red", lwd = 2)
lines(cumprod(1 + dn.prt.rtn$Acct) - 1, col = "green", lwd = 2)
addLegend("topleft", legend.names = c("S&P 500", "L/S Port", "Account"),
          lty = 1, lwd = 2, col = c("black", "red", "green"))


# Examine performance
dn.returns <- merge(SPY, dn.prt.rtn[ , c(1, 4)])
dn.returns[1, ] <- 0

# Chart drawdowns
charts.PerformanceSummary(dn.returns)
table.Drawdowns(dn.returns$SPY)
table.Drawdowns(dn.returns$Acct)

# Performance tables
table.AnnualizedReturns(dn.returns[-1, ], Rf = mean(portfolio$BIL) / 12)

apply(dn.returns[-1, ], MARGIN = 2, FUN = tot.ret)