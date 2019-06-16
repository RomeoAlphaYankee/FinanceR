library(quantmod)
library(PerformanceAnalytics)

data(edhec)
charts.PerformanceSummary(edhec[ , 5:6])
charts.RollingPerformance(edhec[ , 5], Rf = 0.0025)
table.AnnualizedReturns(edhec[ , 5], Rf = 0.0025)

getSymbols("AMZN")
getSymbols("IBM") 
getSymbols("AAPL")
getSymbols("MSFT")

prices <- cbind(AMZN$AMZN.Adjusted, IBM$IBM.Adjusted, AAPL$AAPL.Adjusted, MSFT$MSFT.Adjusted)
head(prices)
colnames(prices) <- c("AMZN", "IBM", "AAPL", "MSFT")
head(prices)

plot(prices$AMZN["2018"] / first(prices$AMZN["2018"], "1 day"))
lines(prices$AAPL["2018"], col = "red", lwd = 2)

returns <- Return.calculate(prices)

returns <- returns[-1, ]
charts.PerformanceSummary(returns)

prev <- matrix(rep(last(prices["2017"], "1 day"), 251), ncol = 4, byrow = TRUE)
plot(prices["2018"] / prev, main = "Relative Performance 2018")

# Generate portfolio returns
eq_weights <- c(.25, .25, .25, .25)
port_return <- Return.portfolio(returns, eq_weights, rebalance_on = ("months"))
port_return2 <- Return.portfolio(returns, eq_weights)
charts.PerformanceSummary(port_return)
charts.PerformanceSummary(port_return2)

plot(port_return)
plot(port_return2)

chart.RollingPerformance(R = port_return, width = 251, FUN = "Return.annualized")
chart.RollingPerformance(R = port_return, width = 251, FUN = "StdDev.annualized")
chart.RollingPerformance(R = port_return, width = 251, FUN = "SharpeRatio.annualized", Rf = .000118)

# import latest 3-month T-bill rate
rfr <- getSymbols("DGS3MO", src = "FRED", from = "2019", auto.assign = FALSE)
rfr <- na.locf(rfr)
rfr_last <- last(rfr, "1 day")
rfr_mo <- mean(last(rfr, "251 days")) / 12

#calculate spot risk return ratios
# annual returns on a rolling period
port_rtn <- cumprod(1 + last(port_return, "251 days")) -1
port_rtn2 <- cumprod(1 + last(port_return2, "251 days")) - 1

port_rtn_ttm <- last(port_rtn, "1 day")
port_rtn2_ttm <- last(port_rtn, "1 day")

# annualized standard deviation calculated on a rolling window
port_sd <- sd(last(port_return, "251 days")) * sqrt(251)
port_sd2 <- sd(last(port_return2, "251 days")) * sqrt(251)

# spot sharpe ratio calculation over TTM as of last
(port_rtn_ttm - (rfr_last / 100)) / port_sd
(port_rtn2_ttm - (rfr_last / 100)) / port_sd2

# import S&P 500 returns
SP500 <- getSymbols("^GSPC", auto.assign = FALSE)

SP500_mo <- to.monthly(SP500)
plot(SP500_mo$SP500.Close)
SP500_returns <- Return.calculate(SP500_mo$SP500.Close)
SP500_returns <- SP500_returns["2009-06/2019-05"]
plot.xts(SP500_returns)

(1 + mean(SP500_returns))^12 - 1
last(cumprod(1+SP500_returns))^(1 / length(SP500_returns)) - 1
mean.geometric(SP500_returns)
(Return.annualized(SP500_returns, scale = 12) - (rfr_last / 100)) / (sd(SP500_returns) * sqrt(12))
table.AnnualizedReturns(SP500_returns, scale = 12, Rf = rfr_mo / 100)

chart.RollingPerformance(R = SP500_returns, width = 12, FUN = "Return.annualized")                        
chart.RollingPerformance(R = SP500_returns, width = 12, FUN = "StdDev.annualized")
chart.RollingPerformance(R = SP500_returns, width = 12, FUN = "SharpeRatio.annualized")

chart.Histogram(SP500_returns, method = c("add.density", "add.normal"))
charts.RollingPerformance(R = SP500_returns)

# construct a balanced portfolio
SPY <- getSymbols("SPY", auto.assign = FALSE)
AGG <- getSymbols("AGG", auto.assign = FALSE)
returns_6040 <- Return.calculate(cbind(SPY$SPY.Adjusted, AGG$AGG.Adjusted))
returns_6040 <- returns_6040[-1, ]

returns_6040 <- cbind(returns_6040, Return.portfolio(R = returns_6040, weights = c(.60, .40), rebalance_on = ("months")))
names(returns_6040) <- c("SPY", "AGG", "portfolio")

plot(cumprod(1 + returns_6040$SPY), col = "red")
lines(cumprod(1 + returns_6040$portfolio), col = "black")
lines(cumprod(1 + returns_6040$AGG), col = "blue")

charts.PerformanceSummary(returns_6040)
charts.RollingPerformance(returns_6040)

# would a different ratio of stocks to bonds result in a higher Sharpe ratio
grid <- seq(0, 1, 0.01)
vsharpe <- rep(NA, length(grid))
for(i in 1:length(grid)){
  weight <- grid[i]
  preturns <- weight * returns_6040$SPY + (1 - weight) * returns_6040$AGG
  vsharpe[i] <- SharpeRatio.annualized(preturns)
}

which(vsharpe == max(vsharpe))

plot(grid, vsharpe, xlab = "Weights", ylab = "Ann. Sharpe Ratio")
abline(v = grid[vsharpe == max(vsharpe)], lty = 3)

# plot it
returns_6040$optimized <- Return.portfolio(R = c(returns_6040$SPY, returns_6040$AGG), weights = c(.14, (1 - .14)), rebalance_on = ("months"))
charts.PerformanceSummary(returns_6040$optimized)
chart.CumReturns(returns_6040)

# look at the correlations
chart.Correlation(returns_6040[ , 1:2])
chart.RollingCorrelation(returns_6040$AGG, returns_6040$SPY, width = 30)

# would different time frame result in different weights
returns_6040.5yr <- last(returns_6040, "5 years")

grid <- seq(0, 1, 0.01)
vsharpe <- rep(NA, length(grid))
for(i in 1:length(grid)){
  weight <- grid[i]
  preturns <- weight * returns_6040.5yr$SPY + (1 - weight) * returns_6040.5yr$AGG
  vsharpe[i] <- SharpeRatio.annualized(preturns)
}

which(vsharpe == max(vsharpe))

plot(grid, vsharpe, xlab = "Weights", ylab = "Ann. Sharpe Ratio")
abline(v = grid[vsharpe == max(vsharpe)], lty = 3)

# plot it
returns_6040.5yr$optimized <- Return.portfolio(R = c(returns_6040.5yr$SPY, returns_6040.5yr$AGG), 
                              weights = c(which(vsharpe == max(vsharpe)), (1 - which(vsharpe == max(vsharpe)))), 
                              rebalance_on = ("months"))
charts.PerformanceSummary(returns_6040.5yr$optimized)
chart.CumReturns(returns_6040.5yr)
addLegend("topleft", c("SPY", "AGG", "60/40", "Highest Sharpe"), lwd = 1, col = 1:4)

# calculate portfolio variance
# first get data for equity, bond, real estate, and commodity returns
SPY <- getSymbols("SPY", auto.assign = FALSE)
AGG <- getSymbols("AGG", auto.assign = FALSE)
VNQ <- getSymbols("VNQ", auto.assign = FALSE)
GSG <- getSymbols("GSG", auto.assign = FALSE)

SPY <- to.monthly(SPY)
AGG <- to.monthly(AGG)
VNQ <- to.monthly(VNQ)
GSG <- to.monthly(GSG)

prt.rtn <- cbind(SPY$SPY.Adjusted, AGG$AGG.Adjusted, VNQ$VNQ.Adjusted, GSG$GSG.Adjusted)
prt.rtn <- Return.calculate(prt.rtn)
prt.rtn <- last(prt.rtn, "120 months")
names(prt.rtn) <- c("Equities", "Bonds", "Real Estate", "Commodities")

# calculate expected returns, covariance, and portfolio returns given weights w
w <- as.matrix(c(.4, .4, .1, .1))
mu <- as.matrix(colMeans(prt.rtn))
sigma <- cov(prt.rtn)

# expected returns of individual assets given weights w
mu * w

# expected portfolio return
t(w) %*% mu

# expected portfolio volatility
sqrt(t(w) %*% sigma %*% w)

