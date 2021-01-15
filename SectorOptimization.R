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

SPY <- Return.calculate((Ad(SPY[endpoints(SPY, on = "months")])))
SPY <- SPY[-1, ]
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
tail(returns)

# Assign initial weights from State Street to match that of the SPY
weights <- c(.2449, .1446, .1182, .1045, .0973, .0864, .0757, .0369, .0342, .032, .0253, 0)
names(weights) <- tickers
sum(weights)
weights

# Calculate portfolio expected returns, variance, covariance, and correlation
expected.rtn <- colMeans(returns, na.rm = TRUE) # Monthly
((1 + expected.rtn)^12 - 1) # Annualized
sum(((1 + expected.rtn)^12 - 1) * weights) # Annualized and weighted

# Check volatility
apply(returns, 2, FUN = sd, na.rm = TRUE) # Monthly standard deviation
apply(returns, 2, FUN = sd, na.rm = TRUE) * sqrt(12) # Annual standard deviation

# Calculate Sharpe ratio for each sector
((1 + expected.rtn)^12 - 1) / (apply(returns, 2, FUN = sd, na.rm = TRUE) * sqrt(12))

# Examine correlation
returns.named <- returns
colnames(returns.named) <- c("Tech", "Health", "Finance", "Comms", "Cyclical", "Industry", "Staples", "Utils", "Energy", "Real Est",  "Material", "Cash")
round(cor(returns.named["2018-07/", -12]), 4)
corrplot::corrplot.mixed(cor(returns.named["2018-07/2019-12", -12]))
corrplot::corrplot.mixed(cor(returns.named["2020/", -12]))

# Examine Sector Returns
chart.CumReturns(returns.named[ , -12])
chart.CumReturns(returns.named["2019-12/2020-03", -12], main = "Sector Returns",
                 legend.loc = "bottomleft", 
                 col =  c("black", "red", "green", "cornflowerblue", "darkkhaki", "yellow", "grey", "navyblue", "orange", "firebrick", "darkmagenta", "darkolivegreen"))

chart.CumReturns(returns.named["2020-04/", -12], main = "Sector Returns",
                 legend.loc = "topleft",
                 col =  c("black", "red", "green", "cornflowerblue", "darkkhaki", "yellow", "grey", "navyblue", "orange", "firebrick", "darkmagenta", "darkolivegreen"))

tail(returns.named[ , -12])

# Check changes in correllation
corr.change <- as.matrix(cor(returns["/2019-12"])) -as.matrix( cor(returns["2020/"]))

round(corr.change, 3)
max(corr.change) == corr.change
min(corr.change) == corr.change

# Calculate returns without rebalancing
prt.rtn <- Return.portfolio(R = returns, weights = weights)
chart.CumReturns(prt.rtn)

# Plot weighted sector SPDR returns vs. regular SPY ETF

plot((cumprod(1 + prt.rtn) - 1), col = "blue", main = "Portfolio w/o Rebalancing vs. S&P 500")
lines((cumprod(1 + SPY) - 1), col = "red")

# Cumulative performance difference
plot((cumprod(1 + (prt.rtn - SPY)) - 1), 
     col = "red", main = "Long Sector SPDRS / Short SPY")

# Rebalance on months
prt.rebal <- Return.portfolio(R = returns, weights = weights, rebalance_on = "months")

# Plot monthly rebalanced sector SPDR returns vs. regular SPY ETF
plot((cumprod(1 + prt.rebal) - 1), col = "blue", main = "Portfolio Rebalanced vs. S&P 500")
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
# First, remove any residual portfolio specifications
rm(pspec)

# Create a new base portfolio specification
pspec <- portfolio.spec(assets = tickers, weight_seq = weights)
print.default(pspec)

# Add weight constraints
pspec <- add.constraint(portfolio = pspec, 
                        type = "full_investment")

pspec <- add.constraint(portfolio = pspec, 
                        type = "long_only")

# Add box constraint for each sector between 0 and 5% + market weight
pspec <- add.constraint(portfolio = pspec, type = 'box', 
                        min = rep(0, length(tickers)), 
                        max = (weights + 0.05))

# Add objective for return
pspec <- add.objective(portfolio = pspec, 
                       type = 'return', 
                       name = 'mean')

# Create a specialized portfolio specification
# Add objective to minimize risk of expected tail loss at 95% confidence
pspec.tail <- add.objective(portfolio = pspec, 
                       type = 'risk',
                       name = 'ETL')

print.default(pspec.tail)

# Add objective for risk budget of max 40% per asset
# pspec <- add.objective(portfolio = pspec,
#                       type = 'risk_budget', 
#                       name = 'ES',
#                       arguments = list(p = 0.95), max_prisk = 0.4)

# Inspect the portfolio object
print.default(pspec.tail)
pspec

# Load the optimization tools
library(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)

# Run the optimization
prt.tail.opt <- optimize.portfolio.rebalancing(R = returns, 
                                          portfolio = pspec.tail, 
                                          optimize_method = "ROI", 
                                          rebalance_on = 'months', 
                                          training_period = 1,
                                          rolling_window = 6)

# Check the results
prt.tail.opt

# Inspect the extracted weights
last(extractWeights(prt.tail.opt))
rowSums(extractWeights(prt.tail.opt))

chart.Weights(prt.tail.opt,
              c("black", "red", "green", "cornflowerblue", "darkkhaki", "yellow", "grey", "navyblue", "orange", "firebrick", "darkmagenta", "darkolivegreen"))

# Calculate returns with optimized weights
prt.tail.rtn <- Return.portfolio(R = returns, weights = extractWeights(prt.tail.opt), 
                                rebalance_on = "months")

chart.CumReturns(prt.tail.rtn, main = "Optimized Portfolio Returns")

# Plot returns vs. SPY
plot((cumprod(1 + SPY["2018-08/"]) - 1), col = "blue",
     ylim = c(-0.5, 0.5),
     main = "SPY vs. Optimized Sector Portfolio")
lines((cumprod(1 + prt.tail.rtn) - 1), col = "red")
addLegend("topleft", legend.names = c("Portfolio", "S&P 500"),
          lty = 1, lwd = 2, col = c("red", "blue"))

# Plot optimized portfolio vs. Spirit of America Value Fund 
# First get SOAVX adjusted returns.
getSymbols("SOAVX", from = from, to = to)
SOAVX <- Return.calculate(Ad(SOAVX[endpoints(SOAVX, on = "months")]))[-1, ]
names(SOAVX) <- "SOAVX"
SOAVX

# Create the plot
plot((cumprod(1 + SPY["2018-08/"]) - 1), col = "blue", 
     ylim = c(-0.2, 0.45),
     main = "Optimized Portfolio vs. S&P 500 vs. SOAVX")
lines((cumprod(1 + prt.tail.rtn) - 1), col = "green", lwd = 2)
lines((cumprod(1 + SOAVX["2018-08/"]) - 1), col = "red")
addLegend("topleft", legend.names = c("Optimized", "S&P 500", "SOAVX"),
          lty = 1, lwd = 2, col = c("green", "blue", "red"))

# Check the annualized returns and Sharpe ratios
compare.returns <- merge(prt.tail.rtn, SPY, SOAVX)
head(compare.returns)

names(compare.returns) <- c("Optimized", "SPY", "SOAVX")


table.AnnualizedReturns(compare.returns[-1, ], Rf = (mean(returns$BIL)*sqrt(12))/12)

# Calculate the total returns
tot.ret <- function(x){
  tmp <- cumprod(1 + x) - 1
  last(tmp)
}

compare.returns[1, ] <- 0

apply(compare.returns, 2, FUN = tot.ret)

# Recreate the plot with investment of $1,000
plot((cumprod(1 + compare.returns$SPY)) * 1000, col = "blue", 
     ylim = c(800, 1500),
     main = "Change in $1,000 Investment")
lines((cumprod(1 + compare.returns$Optimized)) * 1000, col = "green", lwd = 2)
lines((cumprod(1 + compare.returns$SOAVX)) * 1000, col = "red")
addLegend("topleft", legend.names = c("Optimized", "S&P 500", "SOAVX"),
          lty = 1, lwd = 2, col = c("green", "blue", "red"))

# Chart the performance summaries with drawdowns
charts.PerformanceSummary(compare.returns)

# Drawdowns
table.Drawdowns(prt.tail.rtn)
table.Drawdowns(SOAVX)


#####

# Cumulative performance difference between optimized portfolio and SPY
plot((cumprod(1 + (prt.tail.rtn - SPY)) - 1), 
     col = "red", main = "Long Optimized Sector SPDRS / Short SPY")

# Run a long short portfolio that is 30% short. On the date of the travel ban shift to 100% 
# short, otherwise known as market neutral

ls.prt.rtn <- merge(prt.tail.rtn, SPY)
ls.prt.rtn$weights["2018/2019"] <- 0.3
ls.prt.rtn$weights["2020-01/2020-06"] <- 1.0
ls.prt.rtn$weights["2020-07/"] <- 0.3

ls.prt.rtn$sp.wgt <- ls.prt.rtn$SPY * ls.prt.rtn$weights
ls.prt.rtn$active.rtn <- ls.prt.rtn$portfolio.returns - ls.prt.rtn$sp.wgt

chart.CumReturns(ls.prt.rtn$active.rtn, main = "Long Optimized / 30% Short / 100% Short")
lines(cumprod(1 + SPY) - 1)

# plot(cumprod(1 + ls.prt.rtn$active.rtn) - 1, col = "green")
lines(cumprod(1 + SPY) - 1, col = "red")
addLegend("topleft", legend.names = c("Optimized L/S", "S&P 500"),
          lty = 1, lwd = 2, col = c("black", "red"))


table.AnnualizedReturns(SPY, Rf = 0.004 / 12)
table.AnnualizedReturns(ls.prt.rtn$active.rtn, Rf = 0.004 / 12)

