# utilize the PortfolioAnalytics package to optimize portfolios

library(PortfolioAnalytics)
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
# However, it failed to reduce standard devivation
sd(opt.prt.rtn)
sd(prt.rtn)

# Check for diversification, or lack thereof
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

# Clean up the process of comparing portfolios
portfolios <- cbind(prt.rtn, opt.prt.rtn, opt2.prt.rtn, opt3.prt.rtn, opt4.prt.rtn)
names(portfolios) <- c("Equal", "Quadratic", "Random", "DEoptim", "MaxSR")

lapply(portfolios, table.AnnualizedReturns, scale = 12, Rf = 0.02 / 12)

# Check performance
charts.PerformanceSummary(R = opt4.prt.rtn, Rf = 0.02, main = "MaxSR")

# Plot the returns of all portfolio solutions
chart.CumReturns(opt4.prt.rtn, lwd = 2, ylim = range(cumprod(1 + opt.prt.rtn) - 1))
lines(cumprod(1 + prt.rtn) - 1, col = "blue")
lines(cumprod(1 + opt.prt.rtn) - 1, col = "green")
lines(cumprod(1 + opt2.prt.rtn) - 1, col = "red")
lines(cumprod(1 + opt3.prt.rtn) - 1, col = "yellow")
lines(cumprod(1 + opt5.prt.rtn) - 1, col = "orange")
addLegend(legend.loc = "topleft", legend.names = c("MaxSR", "Equal Weight", "Quadratic", "Random", "DEoptim"),
          lty = 1, lwd = 2, col = c("black", "blue", "green", "red", "yellow"))

# Looking for higher return with more diversification than the original optimization
# Add a box constraint to the weights
prt.spc5 <- add.constraint(portfolio = prt.spc, type = "box", min = 0, max = 0.2)
opt5 <- optimize.portfolio(R = ret, portfolio = prt.spc5, optimize_method = "ROI")
opt5.prt.rtn <- Return.portfolio(R = ret, weights = opt5$weights)

barplot(opt5$weights)
portfolios$Box <- opt5.prt.rtn

lapply(portfolios, table.AnnualizedReturns, Rf = 0.02 / 12)
vol_budget <- StdDev(R = ret, portfolio_method = "component", weights = opt5$weights)
barplot(vol_budget$pct_contrib_StdDev)







# Try periodic rebalancing
opt.rebal <- optimize.portfolio.rebalancing(R = ret, portfolio = prt.spc, 
                                            optimize_method = "random", rebalance_on = "years", 
                                            training_period = 60, rolling_window = 60, trace = TRUE)
opt.rebal

rr <- Return.portfolio(ret, weights = extractWeights(opt.rebal))
charts.PerformanceSummary(rr)

# Chart results of rebalanced portfolio
chart.CumReturns(rr, ylim = c(0, 0.75))
lines(cumprod(1 + prt.rtn["2002/"]) - 1, col = "blue")
lines(cumprod(1 + opt.prt.rtn["2002/"]) - 1, col = "red")
lines(cumprod(1 + opt2.prt.rtn["2002/"]) - 1, col = "green")

# Rebalancing has amplified estimation error and locked in drawdown

# Add a weight sum constraint
prt.spc <- add.constraint(portfolio = prt.spc, type = "weight_sum", min_sum = 1, max_sum = 1)


# Add a group constraint
# prt.spc <- add.constraint(portfolio = prt.spc, type = "group", groups = list(c(1, 5, 7, 9, 10, 11), c(2, 3, 4, 6, 8, 12)), group_min = 0.40, group_max = 0.60)

# Add a box constraint
prt.spc <- add.constraint(portfolio = prt.spc, type = "box", min = 0.05, max = 0.40)

opt2.rebal <- optimize.portfolio.rebalancing(R = ret, portfolio = prt.spc, optimize_method = "random", rebalance_on = "quarters", 
                                             training_period = 60, rolling_window = 60, trace = TRUE)

# Plot results of second rebalanced portfolio optimization
chart.Weights(opt2.rebal)
rr2 <- Return.portfolio(ret, weights = extractWeights(opt2.rebal))
chart.CumReturns(rr2, ylim = c(0, 0.75))

# Compare results of prior optimizations
lines(cumprod(1 + prt.rtn["2002/"]) - 1, col = "blue")
lines(cumprod(1 + opt.prt.rtn["2002/"]) - 1, col = "red")
lines(cumprod(1 + opt2.prt.rtn["2002/"]) - 1, col = "green")
lines(cumprod(1 + rr) - 1, col = "gray")

# Inspect risk / reward metrics
charts.PerformanceSummary(rr2)

cbind(rebal2 = SharpeRatio(rr2), ew = SharpeRatio(prt.rtn["2002/"]), optimized = SharpeRatio(opt.prt.rtn["2002/"]),
      optimized2 = SharpeRatio(opt2.prt.rtn["2002/"]), rebal1 = SharpeRatio(rr))

