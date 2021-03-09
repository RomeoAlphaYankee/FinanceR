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

SPY <- Return.calculate((Ad(to.weekly(SPY))))
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
  temp <- Return.calculate(Ad(to.weekly(temp)))
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

# Perform a Markowitz mean / variance portfolio optimization on sectors
# First, remove any residual portfolio specifications
rm(pspec)

# Create a new base portfolio specification
pspec <- portfolio.spec(assets = tickers, weight_seq = weights)
print.default(pspec)

# Add weight constraints
pspec <- add.constraint(portfolio = pspec, 
                        type = "full_investment")

#pspec <- add.constraint(portfolio = pspec, 
#                        type = "long_only")

# Add box constraint for each sector between 0 and 5% + market weight
pspec <- add.constraint(portfolio = pspec, type = 'box', 
                        min = rep(-0.05, length(tickers)), 
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

# Inspect the portfolio object
print.default(pspec.tail)


# Load the optimization tools
library(ROI)
require(ROI.plugin.glpk)
require(ROI.plugin.quadprog)

# Run the optimization
prt.tail.opt <- optimize.portfolio.rebalancing(R = returns, 
                                               portfolio = pspec.tail, 
                                               optimize_method = "ROI", 
                                               rebalance_on = 'weeks', 
                                               training_period = 1,
                                               rolling_window = 26)

# Check the results
prt.tail.opt

# Inspect the extracted weights
last(extractWeights(prt.tail.opt))
rowSums(extractWeights(prt.tail.opt))

chart.Weights(prt.tail.opt,
              c("black", "red", "green", "cornflowerblue", "darkkhaki", "yellow", "grey", "navyblue", "orange", "firebrick", "darkmagenta", "darkolivegreen"))

# Calculate returns with optimized weights
prt.tail.rtn <- Return.portfolio(R = returns, weights = extractWeights(prt.tail.opt), 
                                 rebalance_on = "weeks")

chart.CumReturns(prt.tail.rtn, main = "Optimized Portfolio Returns")

# Plot returns vs. SPY
plot((cumprod(1 + SPY["2018-08/"]) - 1), col = "blue",
     ylim = c(-0.2, 0.75),
     main = "S&P 500 vs. Optimized Sector Portfolio")
lines((cumprod(1 + prt.tail.rtn) - 1), lwd = 2, col = "red")
addLegend("topleft", legend.names = c("Portfolio", "S&P 500"),
          lty = 1, lwd = 2, col = c("red", "blue"))

# Check returns
table.AnnualizedReturns(prt.tail.rtn, scale = 52,Rf = 0.005/52)
