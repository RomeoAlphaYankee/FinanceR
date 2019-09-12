library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(DEoptim)
library(ROI)

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
charts.PerformanceSummary(fang.returns, main = "FAANG Performance")
plot.zoo(fang.returns)
plot.zoo(cumprod(1 + fang.returns))

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

apply(fang.returns, 2, quantile, probs = 0.05)
apply(fang.returns, 2, quantile, probs = 0.995)

# These stocks have a considerable amount of extreme upside returns. In fact, the mean return for 
# GOOG is double its median return. It therefore may be difficult to fit a standard mean/variance 
# model, as the minimum variance goal will penalize large moves up, and reduce overall portfolio return.

# First establish benchmark portfolio
# Create a vector of equal weights
weights <- rep(1 / ncol(fang.returns), ncol(fang.returns))

# Establish benchmark returns
benchmark <- Return.portfolio(fang.returns, weights = weights, rebalance_on = "quarters")
colnames(benchmark) <- "Benchmark"

# Investigate benchmark returns
dev.off()
charts.PerformanceSummary(cbind(benchmark, fang.returns))

table.AnnualizedReturns(cbind(benchmark, fang.returns))

# Already, we can see that the benchmark returns are quite good, outperforming all but one of
# the individual stocks in terms of return, and outperforming all on the basis of standard
# deviation and sharpe ratio. 

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
table.AnnualizedReturns(returns["2014-04-01/"])
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

# Calculate the returns
fang.rb.ret <- Return.rebalancing(R = fang.returns, weights = extractWeights(fang.rb))

# Compare to other portfolios
returns <- cbind(returns[ , 1:2], fang.rb.ret)
names(returns) <- c(names(returns)[-3], "Risk.Budget")

# Check the results
charts.PerformanceSummary(returns["2014-04-01/"])
table.AnnualizedReturns(returns["2014-04-01/"])

chart.Weights(fang.rb)

# As anticipated, the risk budget constrained allocations to NFLX, but included AAPL in each quarter.
# This led to reduced standard deviatio, but significantly lower returns and Sharpe ratio.

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
chart.Weights(opt.random.rb)

# Compare the returns
returns <- cbind(returns, opt.random.rb.returns)
colnames(returns) <- c("Benchmark", "Quadratic", "DE.Bdgt", "Rnd.Bdgt")

charts.PerformanceSummary(returns["2014-01-01/"])

table.AnnualizedReturns(returns["2014-04-01/"])
maxDrawdown(returns)

# Interestingly, the random portfolio optimization produced a better set of performance metrict than
# the differential evolution optimizer. Nonetheless, its performance is lacking.

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

table.AnnualizedReturns(returns["2014-04-01/"])
maxDrawdown(returns)



# Add a box constraint instead
fang.port.box <- add.constraint(portfolio = fang.port, 
                                type="box",
                                min = 0.10,
                                max = 0.40)

fang.port.box

# Run the optimization
fang.box <- optimize.portfolio.rebalancing(R = fang.returns, portfolio = fang.port.box, 
                                          optimize_method = "DEoptim",  
                                          rebalance_on = "quarters",
                                          training_period = 252,
                                          rolling_window = 252)

# Calculate the returns
fang.box.ret <- Return.rebalancing(R = fang.returns, weights = extractWeights(fang.box))

# Compare to other portfolios
returns <- cbind(returns, fang.box.ret)
names(returns) <- c(names(returns)[-4], "Box")

# Check the results
charts.PerformanceSummary(returns["2014/"], main = "Portfolio Returns")
table.AnnualizedReturns(returns["2014/"])

chart.Weights(fang.box)


# Large improvement in return and sharpe ratio
# Replacing DEoptim results with random results in box constraint returns
returns <- merge(returns, opt.random.returns)
returns$Box <- NULL
names(returns) <- c(names(returns)[-4], "Box")

charts.PerformanceSummary(returns["2014/"], main = "Portfolio Returns")
table.AnnualizedReturns(returns["2014/"])


# The randomly generate portfolios returned a better result than the 
# differential evolution optimizer. Since there is no risk budget constraint
# try a quadratic solver.
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

table.AnnualizedReturns(cbind(returns, fang.box.ret)["2014/"])

