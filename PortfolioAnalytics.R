# utilize the PortfolioAnalytics package to optimize portfolios

library(PortfolioAnalytics)
library(ROI)

data("edhec")

# Use historical hedge fund data
ret <- edhec
names <- colnames(ret)

# Investigate hedge fund return data
# First look at the correlation
hf.cor <- cor(data)
library(corrplot)
corrplot.mixed(hf.cor, upper = "color", tl.col = "black")

# calculate expected returns, covariance, and portfolio returns given weights w
w <- as.matrix(c(rep(1 / ncol(ret), ncol(ret))))
mu <- as.matrix(colMeans(ret))
sigma <- cov(ret)

# expected returns of individual assets given weights w
mu * w

# Expected equal weight benchmark portfolio return
t(w) %*% mu

# Expected benchmark portfolio volatility
sqrt(t(w) %*% sigma %*% w)

# Create portfolio specification object
prt.spc <- portfolio.spec(names)

# Begin adding constraints to specifications
prt.spc <- add.constraint(portfolio = prt.spc, type = "long_only")
prt.spc <- add.constraint(portfolio = prt.spc, type = "full_investment")

# Add objectives to specification
prt.spc <- add.objective(prt.spc, type = "return", name = "mean")
prt.spc <- add.objective(prt.spc, type = "risk", name = "StdDev")

# View the specifications
prt.spc

# Run portfolio optimization using quadratic solution
opt <- optimize.portfolio(ret, portfolio = prt.spc, optimize_method = "ROI")
opt

# Plot individual assets, and optimized solution
chart.RiskReward(opt, risk.col = "StdDev", return.col = "mean", chart.assets = TRUE)

# Examine optimized portfolio weights
# No diversification
extractWeights(opt)

# Extract objective measures
om <- extractObjectiveMeasures(opt)

# Compare to benchmark returns
om$mean
t(w) %*% mu

# Compare to benchmark standard deviation
sqrt(t(w) %*% sigma %*% w)
om$StdDev[1]

# Examine the performance
opt.prt.rtn <- Return.portfolio(ret, weights = opt$weights)
prt.rtn <- Return.portfolio(ret, weights = as.numeric(w))

chart.CumReturns(opt.prt.rtn, col = "blue")
lines(cumprod(1 + prt.rtn) - 1, col = "red")
addLegend("topleft", legend.names = c("Optimized", "Benchmark"), col = c("blue", "red"), lwd = 2)

# It would appear the optimized portfolio outperformed the equal weighted portfolio
# However, it failed to reduce standard devivation or achieve diversification
sd(opt.prt.rtn)
sd(prt.rtn)

# Check weights for diversification, or lack thereof
barplot(opt$weights)

# Check risk budget
vol_budget <- StdDev(R = ret, portfolio_method = "component", weights = opt$weights)
barplot(vol_budget)
vol_budget

# The optimized portfolio suffered a larger max drawdown
maxDrawdown(opt.prt.rtn)
maxDrawdown(prt.rtn)

# Summarize results of optimized portfolio
charts.PerformanceSummary(opt.prt.rtn)
charts.PerformanceSummary(prt.rtn)

# Value at risk also increased
VaR(opt.prt.rtn)
VaR(prt.rtn)

# Add a risk budget constraint to increase diversification and reduce volatility
prt.spc2 <- add.objective(prt.spc, type = "risk_budget", name = "StdDev", min_prisk = 0, max_prisk = .15)
prt.spc2

# Run optimization using a random optimization solution
opt2 <- optimize.portfolio(R = ret, portfolio = prt.spc2, 
                           optimize_method = "random",  trace = TRUE)
opt2

# Make it easier to access benchmark
benchmark <-  list(mean = t(w) %*% mu, StdDev = (sqrt(t(w) %*% sigma %*% w)))

# Check objective measures
om2 <- extractObjectiveMeasures(opt2)

om2$mean - benchmark$mean
om2$StdDev$StdDev - benchmark$StdDev

barplot(c(benchmark$mean, om$mean, om2$mean), ylim = c(0, 0.01))
barplot(c(benchmark$StdDev, om$StdDev, om2$StdDev$StdDev), ylim = c(0, 0.02))

# Chart the results of the generated portfolios and the efficient frontier
chart.RiskReward(opt2, risk.col = "StdDev", return.col = "mean", chart.assets = TRUE)

opt2.prt.rtn <- Return.portfolio(ret, weights = opt2$weights)

chart.CumReturns(opt2.prt.rtn, lwd = 2, ylim = range(cumprod(1 + opt.prt.rtn) - 1))
lines(cumprod(1 + opt.prt.rtn) - 1, col = "blue")
lines(cumprod(1 + prt.rtn) - 1, col = "red")
addLegend(legend.loc = "topleft", legend.names = c("Risk Budget", "Optimized", "Equal Weight"), 
          lty = 1, lwd = 2, col = c("black", "blue", "red"))

# Try a different non-quadratic optimization method
library(DEoptim)
opt3 <- optimize.portfolio(R = ret, portfolio = prt.spc2, 
                           optimize_method = "DEoptim", trace = TRUE)

om3 <- extractObjectiveMeasures(opt3)

om3$mean - om2$mean
om3$mean - benchmark$mean

om3$StdDev$StdDev - om2$StdDev$StdDev
om3$StdDev$StdDev - benchmark$StdDev

barplot(c(benchmark$mean, om$mean, om2$mean, om3$mean), ylim = c(0, 0.01))
barplot(c(benchmark$StdDev, om$StdDev, om2$StdDev$StdDev, om3$StdDev$StdDev), ylim = c(0, 0.02))

# It would appear the Differential Evolution optimization found a higher mean than the 
# random portfolio method, while also lowering standard deviation
chart.RiskReward(opt3, risk.col = "StdDev", return.col = "mean", chart.assets = TRUE)

# Compare metrics
VaR(Return.portfolio(R = ret, weights = as.numeric(w)))
VaR(Return.portfolio(R = ret, weights = opt$weights))
VaR(Return.portfolio(R = ret, weights = opt2$weights))
VaR(Return.portfolio(R = ret, weights = opt3$weights))

opt3.prt.rtn <- Return.portfolio(R = ret, weights = opt3$weights)

# Plot the returns of all portfolio solutions
chart.CumReturns(opt3.prt.rtn, lwd = 2, ylim = range(cumprod(1 + opt.prt.rtn) - 1))
lines(cumprod(1 + prt.rtn) - 1, col = "blue")
lines(cumprod(1 + opt.prt.rtn) - 1, col = "green")
lines(cumprod(1 + opt2.prt.rtn) - 1, col = "red")
addLegend(legend.loc = "topleft", legend.names = c("DEoptim", "Equal Weight", "Quadratic", "Random"),
          lty = 1, lwd = 2, col = c("black", "blue", "green", "red"))

charts.PerformanceSummary(opt3.prt.rtn)
extractWeights(opt3)
barplot(opt3$weights)

chart.RollingPerformance(R = opt3.prt.rtn, width = 12, FUN = "SharpeRatio.annualized", Rf = 0.02 / 12)
charts.RollingPerformance(R = opt3.prt.rtn, width = 12, Rf = 0.02 / 12)
table.AnnualizedReturns(R = opt3.prt.rtn, scale = 12, Rf = 0.02 / 12)

# While the risk budget objective worked as expected the total returns were disappointing
# Add a sharpe ratio parameter to the optimization, and remove the risk budget
opt4 <- optimize.portfolio(R = ret, portfolio = prt.spc, optimize_method = "ROI", maxSR = TRUE)
barplot(opt4$weights)
opt4.prt.rtn <- Return.portfolio(ret, weights = opt4$weights)

# Check performance
charts.PerformanceSummary(R = opt4.prt.rtn, Rf = 0.02, main = "MaxSR")

# Looking for higher return than the risk budget optimization, but with more  
# diversification than the original quadratic optimization
# Add a box constraint to the weights
prt.spc5 <- add.constraint(portfolio = prt.spc, type = "box", min = 0, max = 0.15)
opt5 <- optimize.portfolio(R = ret, portfolio = prt.spc5, optimize_method = "ROI")
opt5.prt.rtn <- Return.portfolio(R = ret, weights = opt5$weights)

barplot(opt5$weights)
portfolios$Box <- opt5.prt.rtn

vol_budget <- StdDev(R = ret, portfolio_method = "component", weights = opt5$weights)
barplot(vol_budget$pct_contrib_StdDev)

table.DownsideRiskRatio(opt5.prt.rtn)

# Plot the returns of all portfolio solutions
chart.CumReturns(prt.rtn, lwd = 2, ylim = range(cumprod(1 + opt.prt.rtn) - 1))
lines(cumprod(1 + opt.prt.rtn) - 1, col = "green")
lines(cumprod(1 + opt2.prt.rtn) - 1, col = "red")
lines(cumprod(1 + opt3.prt.rtn) - 1, col = "yellow")
lines(cumprod(1 + opt4.prt.rtn) - 1, col = "blue")
lines(cumprod(1 + opt5.prt.rtn) - 1, col = "grey")
addLegend(legend.loc = "topleft", legend.names = c("Equal Weight", "Quadratic", "Random", "DEoptim", "MaxSR", "Box"),
          lty = 1, lwd = 2, col = c("black", "green", "red", "yellow", "blue", "grey"))

# Clean up the process of comparing portfolios
portfolios <- cbind(prt.rtn, opt.prt.rtn, opt2.prt.rtn, opt3.prt.rtn, opt4.prt.rtn, opt5.prt.rtn)
names(portfolios) <- c("Equal", "Quadratic", "Random", "DEoptim", "MaxSR", "Box")

lapply(portfolios, table.AnnualizedReturns, scale = 12, Rf = 0.02 / 12)
lapply(portfolios, FUN = SortinoRatio, MAR = 0.001)
lapply(portfolios, FUN = table.Drawdowns)


# Apply periodic rebalancing to all portfolios
# Start with a simple quarterly rebalance for the equal weight portfolio
equal.rebal <- Return.rebalancing(R = ret, weights =  rep(1 / ncol(ret), ncol(ret)), rebalance_on = "quarters")

charts.PerformanceSummary(equal.rebal)
table.AnnualizedReturns(equal.rebal)

# Rebalance using the various optimization methods
quadratic.rebal <- optimize.portfolio.rebalancing(R = ret, portfolio = prt.spc, 
                                              optimize_method = "ROI", rebalance_on = "quarters", 
                                              training_period = 60, rolling_window = 60)

Return.portfolio(R = ret, weights = extractWeights(quadratic.rebal), rebalance_on = "quarters")


random.rebal <- optimize.portfolio.rebalancing(R = ret, portfolio = prt.spc2, 
                                              optimize_method = "random", rebalance_on = "quarters", 
                                              training_period = 60, rolling_window = 60, trace = TRUE)

random.rebal.returns <- Return.portfolio(R = ret, weights = extractWeights(random.rebal), rebalance_on = "quarters")


DEoptim.rebal <- optimize.portfolio.rebalancing(R = ret, portfolio = prt.spc2, 
                                               optimize_method = "DEoptim", rebalance_on = "quarters", 
                                               training_period = 60, rolling_window = 60, trace = TRUE)

DEoptim.rebal.returns <- Return.portfolio(R = ret, weights = extractWeights(DEoptim.rebal), rebalance_on = "quarters")

chart.CumReturns(equal.rebal["2002/"])
lines(cumprod(1 + random.rebal.returns) - 1, col = "blue")
lines(cumprod(1 + DEoptim.rebal.returns) - 1, col = "red")
addLegend(legend.loc = "topleft", legend.names = c("Equal Weight", "Random", "DEoptim"), lwd = 2, col = c("black", "blue", "red"))


maxSR.rebal <- optimize.portfolio.rebalancing(R = ret, portfolio = prt.spc, 
                                               optimize_method = "ROI", rebalance_on = "quarters", 
                                               training_period = 60, rolling_window = 60, maxSR.rebal = TRUE)

maxSR.rebal.returns <- Return.portfolio(R = ret, weights = extractWeights(maxSR.rebal))



box.rebal <- optimize.portfolio.rebalancing(R = ret, portfolio = prt.spc, 
                                               optimize_method = "ROI", rebalance_on = "quarters", 
                                               training_period = 60, rolling_window = 60)





quadratic.rebal.returns <- Return.portfolio(R = ret, weights = extractWeights(quadratic.rebal["2001/ "]))
charts.PerformanceSummary(quadratic.rebal.returns)

# Chart results of rebalanced portfolio

# Plot results of second rebalanced portfolio optimization
chart.Weights(opt2.rebal)
rr2 <- Return.portfolio(ret, weights = extractWeights(opt2.rebal))
chart.CumReturns(rr2, ylim = c(0, 0.75))

# Compare results of prior optimizations


# Inspect risk / reward metrics
charts.PerformanceSummary(rr2)


