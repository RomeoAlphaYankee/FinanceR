# Using Portfolio Analytics to optimize a simple portfolio of bonds, US equities, 
# international equities, and commodities with an objective of creating a 
# minimum variance portfolio. Then add a return objective to create an optimized
# mean / variance portfolio, and compare the two results.

library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(quadprog)
library(DEoptim)
data("indexes")

index_returns <- indexes[ , 1:4]
head(index_returns)

# Create a portfolio specification object
port.spec <- portfolio.spec(assets = colnames(index_returns))

# Add constraints
port.spec <- add.constraint(portfolio = port.spec, type = "full_investment")
port.spec <- add.constraint(portfolio = port.spec, type = "long_only")

# Add objectives  
port.spec <- add.objective(portfolio = port.spec, type = "risk", name = "StdDev")

# Solve the optimization problem for minimum variance portfolio
opt <- optimize.portfolio(R = index_returns, portfolio = port.spec, 
                                      optimize_method = "ROI",
                                      training_period = 12,
                                      rolling_window = 12,
                                      rebalance_on = "months")
opt

# Plot the optimized weights
chart.Weights(opt)
barplot(extractWeights(opt), main= "Min Var")

# Calculate min variance portfolio returns
base <- Return.portfolio(R = index_returns, weights = extractWeights(opt))

charts.PerformanceSummary(base)

# Calculate a benchmark
weights <- rep(1 / ncol(index_returns), ncol(index_returns))
benchmark <- Return.rebalancing(R = index_returns, weights = weights, rebalance_on = "months")

# Check the performance
plot(cumprod(1 + benchmark), col = "red")
lines(cumprod(1 + base), col = "blue")
table.AnnualizedReturns(base)

barplot(weights, main = "Benchmark")

# Now add the return objective and re-run the optimization
port.spec2 <- add.objective(portfolio = port.spec, type = "return", name = "mean")

opt2 <- optimize.portfolio(R = index_returns, portfolio = port.spec2, 
                                       optimize_method = "ROI")

# Plot and compare the results
chart.Weights(opt2)
barplot(extractWeights(opt2), main = "Mean / Var")

# Calculate mean - variance optimized portfolio returns
base2 <- Return.rebalancing(R = index_returns, weights = extractWeights(opt2),
                            rebalance_on = "months")

charts.PerformanceSummary(base2)

# Check the performance
plot(cumprod(1 + base2), col = "blue")
lines(cumprod(1 + benchmark), col = "red")
lines(cumprod(1 + base), col = "black")

# Check the results
charts.PerformanceSummary(base2)

returns <- merge(benchmark, base, base2)
names(returns) <- c("benchmark", "min vol", "mean/var")

# compare the returns and Sharpe ratios
table.AnnualizedReturns(returns)

# Reset the objectives
port.spec3 <- portfolio.spec(assets = colnames(index_returns))

# Add constraints
port.spec3 <- add.constraint(portfolio = port.spec, type = "full_investment")
port.spec3 <- add.constraint(portfolio = port.spec, type = "long_only")

# Add an objective to minimize expected shortfall objective
port.spec3 <- add.objective(portfolio = port.spec, type = "risk", name = "ES", 
                           arguments = list(p = 0.95, method = "gaussian"))

# Add objective to maximize mean
port.spec3 <- add.objective(portfolio = port.spec3, type = "return", name = "mean")

# Run optimization
opt3 <- optimize.portfolio(R = index_returns, portfolio = port.spec3, optimize_method = "ROI")

chart.Concentration(opt3)

# Calculate returns
base3 <- Return.portfolio(R = index_returns, weights = extractWeights(opt3))

# Check the performance
plot(cumprod(1 + base2), col = "blue")
lines(cumprod(1 + benchmark), col = "red")
lines(cumprod(1 + base), col = "black")
lines(cumprod(1 + base3), col = "green")

# Plot and compare the results
chart.Weights(opt)
barplot(extractWeights(opt), main = "ES Opt")

charts.PerformanceSummary(base3)

returns <-merge(returns, base3)
names(returns) <- c("Equal", "Min Vol", "Mean/Var", "Min ES")

table.AnnualizedReturns(returns)

# One more optimization, this time with a rolling rebalance window
opt4 <- optimize.portfolio.rebalancing(R = index_returns, portfolio = port.spec3,
                                       optimize_method = "DEoptim",
                                       rebalance_on = "quarters",
                                       training_period = 12,
                                       rolling_window = 12)

# Plot and compare the results
chart.Weights(opt4)
extractWeights(opt4)
extractObjectiveMeasures(opt4)$mean

rebal_returns <- Return.rebalancing(R = index_returns, weights = extractWeights(opt4), 
                                  rebalance_on = "quarters")

# Check the performance
plot(cumprod(1 + base2), col = "blue", main = "Portfolio Optimizations")
lines(cumprod(1 + benchmark), col = "red")
lines(cumprod(1 + base), col = "black")
lines(cumprod(1 + base3), col = "green")
lines(cumprod(1 + rebal_returns), col = "orange")
addLegend("topleft", legend.names = c("Base", "Benchmark", "Min Vol", "Min ES", "Min ES Rebal"),
          col = c("blue", "red", "black", "green", "orange"),
          lwd = 2)

# Compare annualized returns  
chart.CumReturns(rebal_returns)
charts.PerformanceSummary(rebal_returns)

# Compare returns
returns <- merge(returns, rebal_returns)
colnames(returns) <- c(colnames(returns[ ,-ncol(returns)]), "Rebal.ES")

table.AnnualizedReturns(returns)

# Plot weights, first each index over time, and all indexes by year
rebal_weights <- extractWeights(opt4)

for(i in 1:ncol(rebal_weights)){
  barplot(rebal_weights[ , i], main =  colnames(rebal_weights[ , i]))
}

dev.off()





# Compare returns of optimization and rebalinced optimization
chart.CumReturns(Return.portfolio(index_returns, weights = extractWeights(opt_rebal)))
lines(cumprod(1 + opt_returns["1985/"]) - 1, col = "blue")

# Chart weights
chart.Weights(opt)
chart.Weights(opt_rebal)

# Compare objective measures
extractObjectiveMeasures(opt)
extractObjectiveMeasures(opt_rebal)

# Use a more robust estimate of asset moments
sample_moments <- set.portfolio.moments(R = index_returns, portfolio = port.spec,
                                        method = "sample")

boudt_moments <- set.portfolio.moments(R = index_returns, portfolio = port.spec,
                                       method = "boudt", k = 3)

black_litterman_moments <- set.portfolio.moments(R = index_returns, portfolio = port.spec,
                                        method = "black_litterman")

meucci_moments <- set.portfolio.moments(R = index_returns, portfolio = port.spec,
                                        method = "meucci")

cbind(sample_moments$mu, 
      boudt_moments$mu, 
      black_litterman_moments$mu, 
      meucci_moments$mu)

round(sample_moments$sigma, 6)
round(boudt_moments$sigma, 6)
round(black_litterman_moments$sigma, 6)
round(meucci_moments$sigma, 6)
