library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(quadprog)
library(DEoptim)
library(ROI)
library(ROI.plugin.quadprog)
# Concatenate FANG tickers
fang <- c("FB", "AAPL", "AMZN", "NFLX", "GOOG")
from <- "2013-01-01"

# Create an object to hold FANG prices
fang.prices <- NULL

# Use for loop to get stock prices
for(ticker in fang){
  fang.prices <- cbind(fang.prices, Ad(getSymbols(ticker, from = from, auto.assign = FALSE)))
}

colSums(is.na(fang.prices))

colnames(fang.prices) <- fang

head(fang.prices)

# Calculate stock returns
fang.returns <- Return.calculate(fang.prices)[-1, ]

colSums(is.na(fang.returns))

head(fang.returns)

# Check stock charts
charts.PerformanceSummary(fang.returns, main = "FAANG Performance Summary")
plot.zoo(fang.returns, main = "FAANG Returns")
plot.zoo(cumprod(1 + fang.returns), main = 'FAANG Cumulative Returns')

# Look for outliers
par(mfrow = c(3, 2))

for(ticker in fang){
  boxplot(as.numeric(fang.returns[ , ticker]), horizontal = TRUE, main = ticker)
}

summary(coredata(fang.returns))

# Check the distribution of returns
par(mfrow = c(3, 2))

for(ticker in fang){
  hist(fang.returns[ , ticker], breaks = 30, main = ticker)
}

rbind(skewness(fang.returns), kurtosis(fang.returns))

# Examine the best and worst 5% returns
apply(fang.returns, 2, quantile, probs = 0.05)
apply(fang.returns, 2, quantile, probs = 0.95)
(apply(fang.returns, 2, quantile, probs = 0.05) + apply(fang.returns, 2, quantile, probs = 0.95))

apply(fang.returns, 2, quantile, probs = 0.995)
apply(fang.returns, 2, quantile, probs = 0.005)
apply(fang.returns, 2, quantile, probs = 0.995) + apply(fang.returns, 2, quantile, probs = 0.005)

# Unlike typical returns, which chug along at a modestly positve mean, then suffer downside vol,
# these stocks have a considerable amount of extreme upside returns. In fact, the mean return for 
# GOOG is double its median return. The net of rextreme up and down returns for NFLX is 3.4%.
# It therefore may be difficult to fit a standard mean/variance model, 
# as the minimum variance goal will penalize large moves up, and reduce overall portfolio return.

# First establish benchmark portfolio 
# Create a vector of equal weights
weights <- rep(1 / ncol(fang.returns), ncol(fang.returns))

# Establish benchmark returns
benchmark <- Return.portfolio(fang.returns, weights = weights, rebalance_on = "quarters")
colnames(benchmark) <- "Benchmark"

# Investigate benchmark returns
dev.off()
charts.PerformanceSummary(cbind(benchmark, fang.returns))

# Calculate annualized performance
table.AnnualizedReturns(cbind(benchmark, fang.returns), Rf = 0)

# Already, we can see that the benchmark returns are quite good, outperforming all but one of
# the individual stocks in terms of return, and outperforming all on the basis of standard
# deviation and sharpe ratio. Let's recalculate with today's risk free rate.

# Get most recent risk free rate
treas <- getSymbols(Symbols = "DGS3MO", src = "FRED", auto.assign = FALSE)
treas

rfr <- (1 + (last(treas) / 100))^(1/252) - 1
rfr <- as.numeric(rfr)

# Calculate the return summary
table.AnnualizedReturns(R = cbind(benchmark, fang.returns), Rf = rfr)

# Create a portfolio object
fang.port <- portfolio.spec(assets = fang)

# Establish constraints
fang.port <- add.constraint(portfolio = fang.port, type = "long_only")
fang.port <- add.constraint(portfolio = fang.port, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)

fang.port

# Establish objectives
fang.port <- add.objective(portfolio = fang.port, type = "risk", name = "StdDev")
fang.port <- add.objective(portfolio = fang.port, type = "return", name = "mean")

fang.port

# Run optimization
fang.base <- optimize.portfolio.rebalancing(R = fang.returns, portfolio = fang.port, 
                                            optimize_method = "ROI", rebalance_on = "quarters",
                                            training_period = 252,
                                            rolling_window = 252)

fang.base.ret <- Return.rebalancing(R = fang.returns, weights =  extractWeights(fang.base))

# Check the optimized portfolio with quarterly rebalancing
colnames(fang.base.ret) <- "Base"
charts.PerformanceSummary(fang.base.ret)
chart.Weights(fang.base)

# Compare to equal weighted portfolio
returns <- cbind(benchmark, fang.base.ret)
names(returns) <- c("Benchmark", "Base")

charts.PerformanceSummary(returns["2014-04-01/"])
table.AnnualizedReturns(returns["2014-04-01/"], Rf = rfr)
maxDrawdown(returns)

# While the optimized portfolio return performance was good, it was often invested in only one stock.
# As a result, the standard deviation and Sharpe Ratios were much worse than the equal weighted portfolio.
# In addition, it sufferd a max drawdown of over 41%.

# Add a risk budget
fang.port.rb <- add.objective(fang.port, type="risk_budget",
                               name = "SemiVariance",
                               min_prisk = 0.10, 
                               max_prisk = 0.40)

fang.port.rb

# Run the optimization
fang.rb <- optimize.portfolio.rebalancing(R = fang.returns, portfolio = fang.port.rb, 
                              optimize_method = "DEoptim",  
                              rebalance_on = "quarters",
                              training_period = 252,
                              rolling_window = 252)

# Look at the weights of the risk budget portfolio
chart.Weights(fang.rb)

# Calculate the returns
fang.rb.ret <- Return.rebalancing(R = fang.returns, weights = extractWeights(fang.rb))

# Compare to other portfolios
returns <- cbind(returns[ , 1:2], fang.rb.ret)
names(returns) <- c(names(returns)[-3], "Risk.Budget")

# Check the results
charts.PerformanceSummary(returns["2014-04-01/"])
table.AnnualizedReturns(returns["2014-04-01/"])
table.AnnualizedReturns(returns["2014-04/"], Rf = rfr)

# As anticipated, the risk budget constrained allocations to NFLX, but included AAPL in each quarter.
# This led to reduced standard deviation, but significantly lower returns and Sharpe ratio.

# Re-run the optimization using random portfolios
# Generate the random portfolios
rp <- random_portfolios(fang.port.rb, permutations = 10000)

# Run the optimization
opt.random.rb <- optimize.portfolio.rebalancing(R = fang.returns, portfolio = fang.port.rb, 
                                             optimize_method = "random",
                                             rp = rp, rebalance_on = "quarters",
                                             training_period = 252,
                                             rolling_window = 252)

# Calculate the returns
opt.random.rb.returns <- Return.portfolio(R = fang.returns, 
                                       weights = extractWeights(opt.random.rb),
                                       rebalance_on = "quarters")

# Chart weights
chart.Weights(opt.random.rb, main = "Random Risk Budget Weights")

# Compare to Differential Evolution Optimization weights
chart.Weights(fang.rb, main = "DEoptim Risk Budget Weights")

# Compare the returns
returns <- cbind(returns, opt.random.rb.returns)
colnames(returns) <- c("Benchmark", "Quadratic", "DE.Bdgt", "Rnd.Bdgt")

charts.PerformanceSummary(returns["2014-01-01/"])

table.AnnualizedReturns(returns["2014-04-01/"], Rf = rfr)
maxDrawdown(returns)

# Look at the returns over a shorter time frames
charts.PerformanceSummary(returns["2019/"])
table.AnnualizedReturns(returns["2019/"], scale = 252  , Rf = rfr)

charts.PerformanceSummary(returns["2019-06/"])
table.AnnualizedReturns(returns["2019-06/"], scale = 252  , Rf = rfr)

# Interestingly, the random portfolio optimization produced a better set of performance 
# metricts than the differential evolution optimizer over shorter time frames.
# Nonetheless, its performance is lacking.

# Change risk budget measurement from semivariance to CVaR
fang.port.cvar.rb <- add.objective(fang.port, type="risk_budget",
                              name = "CVaR",
                              min_prisk = 0.10, 
                              max_prisk = 0.40)

fang.port.cvar.rb

# Run the optimization
cvar.opt <- optimize.portfolio.rebalancing(R = fang.returns, portfolio = fang.port.cvar.rb,
                                           optimize_method = "DEoptim",
                                           rebalance_on = "quarters",
                                           training_period = 252,
                                           rolling_window = 252)

# Calculate the returns
cvar.ret <- Return.rebalancing(R = fang.returns, 
                               weights = extractWeights(cvar.opt), 
                               rebalance_on = "quarters")

# Chart weights
chart.Weights(cvar.opt)

# Compare the returns
returns <- cbind(returns, cvar.ret)
colnames(returns) <- c(colnames(returns)[-5], "CVaR")

charts.PerformanceSummary(returns["2014-01-01/"])

table.AnnualizedReturns(returns["2014-04-01/"], Rf = rfr)
maxDrawdown(returns)

# The CVaR risk budget produced returns far superior to the StdDev risk budfgets, while also
# providing similar annualized standard deviation. While underperforming in terms of returns, the
# CVaR produced lower StdDev and max drawdown vs. quadratic optimization. 

# Investigate Sortino Ratio
SortinoRatio(returns)

# Based upon the above results, I'm inclined to try a couple of additional techniques. 
# First, based upon the outperformance of the simple equal-weight portfolio, perhaps trying a 
# box constraint instead of a risk budget is in order.
# Second, changing the look back period may produce better results from the risk budgets.

# Add a box constraint
fang.port.box <- add.constraint(portfolio = fang.port, 
                                type="box",
                                min = 0.10,
                                max = 0.40)

fang.port.box

# Run the optimization
fang.box <- optimize.portfolio.rebalancing(R = fang.returns, portfolio = fang.port.box, 
                                          optimize_method = "ROI",  
                                          rebalance_on = "quarters",
                                          training_period = 252,
                                          rolling_window = 252)

# Calculate the returns
fang.box.ret <- Return.rebalancing(R = fang.returns, weights = extractWeights(fang.box))

# Compare to other portfolios
returns <- cbind(returns, fang.box.ret)
names(returns) <- c(names(returns)[-6], "Box")

# Check the results
charts.PerformanceSummary(returns["2014-04/"], main = "Portfolio Returns")
table.AnnualizedReturns(returns["2014-04/"], Rf = rfr)
maxDrawdown(returns)
chart.Weights(fang.box)

# Large improvement in return vs. prior three optimizatio methos,
# and highest Sharpe ratio yet.


# Let's shorten the look-back period and re-run the optimizations that have the most promise.
# Run optimization on the quadratic rebalancing
fang.base2 <- optimize.portfolio.rebalancing(R = fang.returns, portfolio = fang.port, 
                                            optimize_method = "ROI", rebalance_on = "quarters",
                                            training_period = 126,
                                            rolling_window = 126)

fang.base.ret2 <- Return.rebalancing(R = fang.returns, weights =  extractWeights(fang.base2))

# Tidy up the returns
returns2 <- cbind(returns[ , 1], fang.base.ret2)
names(returns2) <- c(names(returns2[, -2]), "Quadratic")

# Check the results
charts.PerformanceSummary(returns2["2013-10/"])
table.AnnualizedReturns(returns2["2013-10/"], Rf = rfr)
maxDrawdown(returns2)
chart.Weights(fang.base2)

# Similar to the original quadratic optimization in terms of non-diversification, vol, etc.

# Optimize with CVaR and a shorter look-back
cvar.opt2 <- optimize.portfolio.rebalancing(R = fang.returns, portfolio = fang.port.cvar.rb,
                                           optimize_method = "DEoptim",
                                           rebalance_on = "quarters",
                                           training_period = 126,
                                           rolling_window = 126)

# Calculate the returns
cvar.ret2 <- Return.rebalancing(R = fang.returns, 
                               weights = extractWeights(cvar.opt2), 
                               rebalance_on = "quarters")

returns2 <- cbind(returns2, cvar.ret2)
names(returns2) <- c(names(returns2)[-3], "CVaR")

#Check the results
charts.PerformanceSummary(returns2["2013-10/"])
table.AnnualizedReturns(returns2["2013-10/"], Rf = rfr)
maxDrawdown(returns2)

chart.Weights(cvar.opt2)

# Again let's try a shorter look-back period with the box constraint
# Run the optimization
fang.box2 <- optimize.portfolio.rebalancing(R = fang.returns, portfolio = fang.port.box, 
                                           optimize_method = "ROI",  
                                           rebalance_on = "quarters",
                                           training_period = 126,
                                           rolling_window = 126)

# Calculate the returns
fang.box.ret2 <- Return.rebalancing(R = fang.returns, weights = extractWeights(fang.box2))

colnames(fang.box.ret2) <- "Box"

returns2 <- cbind(returns2, fang.box.ret2)

# Examine the results
charts.PerformanceSummary(returns2["2013-10/"], main = "Portfolio Returns")
table.AnnualizedReturns(returns2["2013-10/"], Rf = rfr)
maxDrawdown(returns2)

chart.Weights(fang.box2)

# Compare to the results with a one year look-back
table.AnnualizedReturns(returns["2014-04/"], Rf = rfr)

# It is safe to say that the CVaR with a shorter look-back period improved the most
# Examine the top performers
returns3 <- cbind(returns[ , c(1, 2, 5, 6)], returns2[ , 3])
colnames(returns3) <- c("Benchmark", "Quadratic", "CVaR1", "Box", "CVaR2")

charts.PerformanceSummary(returns3["2013-10/"])
table.AnnualizedReturns(returns3["2013-10/"], Rf = rfr)
maxDrawdown(returns3)
