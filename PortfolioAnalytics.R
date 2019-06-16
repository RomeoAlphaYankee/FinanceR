# utilize the PortfolioAnalytics package to practice optimizing portfolios

library(PortfolioAnalytics)
data("edhec")


# get hedge fund data
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

# expected portfolio return
t(w) %*% mu

# expected portfolio volatility
sqrt(t(w) %*% sigma %*% w)

# Create portfolio specification object
prt.spc <- portfolio.spec(names)

# Begin adding constraints to specifications
prt.spc <- add.constraint(portfolio = prt.spc, type = "long_only")
prt.spc <- add.constraint(portfolio = prt.spc, type = "full_investment")

# Add objectives to specification
prt.spc <- add.objective(prt.spc, type = "return", name = "mean")
prt.spc <- add.objective(prt.spc, type = "risk", name = "StdDev")
prt.spc <- add.objective(prt.spc, type = "risk_budget", name = "StdDev", min_prisk = 0.05, max_prisk = 0.10)

# View the specifications
prt.spc

# Run portfolio optimization using random method
opt <- optimize.portfolio(ret, portfolio = prt.spc, optimize_method = "random", trace = TRUE)
opt

# plot possibile portfolios, individual assets, and optimized solution
chart.RiskReward(opt, risk.col = "StdDev", return.col = "mean", chart.assets = TRUE)

# examine optimized portfolio weights
extractWeights(opt)

# Compare results to the equal weighted portfolio
om <- extractObjectiveMeasures(opt)

om$mean
t(w) %*% mu

sqrt(t(w) %*% sigma %*% w)
om$StdDev[1]

# Examine the performance
opt.prt.rtn <- Return.portfolio(ret, weights = opt$weights)
prt.rtn <- Return.portfolio(ret, weights = as.numeric(w))

chart.CumReturns(opt.prt.rtn, col = "blue")
lines(cumprod(1 + prt.rtn) - 1, col = "red")

# It would appear the optimized portfolio outperformed the equal weighted portfolio
# However, the risk budget constraint failed to allow for a reduced standard devivation
sd(prt.rtn)
sd(opt.prt.rtn)

# Nonetheless, the optimized portfolio reduced max drawdown
maxDrawdown(prt.rtn)
maxDrawdown(opt.prt.rtn)

charts.PerformanceSummary(prt.rtn)
charts.PerformanceSummary(opt.prt.rtn)

VaR(prt.rtn)
VaR(opt.prt.rtn)

# Remove the risk budget constraint
prt.spc <- portfolio.spec(names)
prt.spc <- add.constraint(prt.spc, "full_investment")
prt.spc <- add.constraint(prt.spc, "long_only")
prt.spc <- add.objective(prt.spc, type = "return", name = "mean")
prt.spc <- add.objective(prt.spc, type = "risk", name = "StdDev")

# Run optimization using quadratic solution
opt2 <- optimize.portfolio(R = ret, portfolio = prt.spc,optimize_method = "ROI",  trace = TRUE)

opt2

chart.RiskReward(opt2, risk.col = "StdDev", return.col = "mean", chart.assets = TRUE)

weights2 <- extractWeights(opt3)
opt2.prt.rtn <- Return.portfolio(ret, weights = weights2)
chart.CumReturns(opt2.prt.rtn)
lines(cumprod(1 + opt.prt.rtn) - 1, col = "red")
lines(cumprod(1 + prt.rtn) - 1, col = "blue")

sd(prt.rtn)
sd(opt.prt.rtn)
sd(opt2.prt.rtn)

# Removing the risk budget constraint improved mean return, but also increased risk
SharpeRatio(prt.rtn)
SharpeRatio(opt.prt.rtn)
SharpeRatio(opt2.prt.rtn)

barplot(weights2)

# Add a sharpe ratio parameter to the optimization
opt2 <- optimize.portfolio(R = ret, portfolio = prt.spc, optimize_method = "ROI", maxSR = TRUE)
barplot(opt2$weights)
opt2.prt.rtn <- Return.portfolio(ret, weights = opt2$weights)

SharpeRatio(prt.rtn)
SharpeRatio(opt.prt.rtn)
SharpeRatio(opt2.prt.rtn)

charts.PerformanceSummary(opt2.prt.rtn)
lines(cumprod(1 + opt.prt.rtn) - 1, col = "red")
lines(cumprod(1 + prt.rtn) - 1, col = "blue")

chart.CumReturns(opt2.prt.rtn)

VaR(prt.rtn)
VaR(opt.prt.rtn)
VaR(opt2.prt.rtn)

# Try periodic rebalancing
opt.rebal <- optimize.portfolio.rebalancing(R = ret, portfolio = prt.spc, 
                                            optimize_method = "random", rebalance_on = "years", 
                                            training_period = 60, rolling_window = 60, trace = TRUE)
opt.rebal

rr <- Return.portfolio(ret, weights = extractWeights(opt.rebal))
charts.PerformanceSummary(rr)



chart.CumReturns(rr)
lines(cumprod(1 + opt.prt.rtn["2002/ "]), - 1, col = "red")



# Add a weight sum constraint
prt.spc <- add.constraint(portfolio = prt.spc, type = "weight_sum", min_sum = 1, max_sum = 1)

# Add a box constraint
prt.spc <- add.constraint(portfolio = prt.spc, type = "box", min = c(rep(0.1, 5), rep(0.05, ncol(edhec) - 5)), max = 0.40)

# Add a group constraint
prt.spc <- add.constraint(portfolio = prt.spc, type = "group", groups = list(c(1, 5, 7, 9, 10, 11), c(2, 3, 4, 6, 8, 12)), group_min = 0.40, group_max = 0.60)

# Print the portfolio specification object
print(prt.spc)

# Optimize using random method
opt2 <- optimize.portfolio(ret, prt.spc, optimize_method = "random", trace = TRUE)

# Plot results of second optimization
chart.RiskReward(opt2, risk.col = "StdDev", return.col = "mean", chart.assets = TRUE)
