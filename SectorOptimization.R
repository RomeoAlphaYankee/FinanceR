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
corrplot::corrplot.mixed(cor(portfolio["2018-07/"]))


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
chart.Histogram(R = prt.rebal, methods = c("add.normal", "add.density"))
skewness(prt.rebal)
kurtosis(prt.rebal)


chart.Drawdown(prt.rebal)
table.Drawdowns(prt.rebal)

# Compare returns and Sharpe ratios
table.AnnualizedReturns(SPY)
table.AnnualizedReturns(prt.rtn)
table.AnnualizedReturns(prt.rebal)

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
prt.opt <- optimize.portfolio.rebalancing(R = portfolio, portfolio = pspec, 
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
              c("black", "red", "green", "cornflowerblue", "darkkhaki", "yellow", "grey", "blue", "orange", "firebrick", "darkmagenta", "darkolivegreen"))

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

table.AnnualizedReturns(prt.rtn.opt)
table.AnnualizedReturns(SPY)
table.AnnualizedReturns(SOAVX)

charts.PerformanceSummary(prt.rtn.opt)
charts.PerformanceSummary(SOAVX)

table.Drawdowns(prt.rtn.opt)
table.Drawdowns(SOAVX)
