# Portfolio optimization
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(MASS)
data("edhec")

asset_returns <- edhec[ , 1:12]

# Create a custom benchmark
# Create a vector of equal weights
equal_weights <- rep(1 / ncol(asset_returns), ncol(asset_returns))

# Compute the benchmark returns
r_benchmark <- Return.portfolio(R = asset_returns, weights = equal_weights, rebalance_on = "quarters")
colnames(r_benchmark) <- "benchmark"

# Look at the benchmark returns
plot(r_benchmark, type = "h")
chart.CumReturns(r_benchmark)

# Create the portfolio specification
port_spec <- portfolio.spec(assets = colnames(asset_returns))

# Add a full investment constraint such that the weights sum to 1
port_spec <- add.constraint(portfolio = port_spec, type = "full_investment")

# Add a long only constraint such that the weight of an asset is between 0 and 1
port_spec <- add.constraint(portfolio = port_spec, type = "long_only")

# Add an objective to minimize portfolio standard deviation
port_spec <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")

# Print the portfolio specification
print(port_spec)

# Run the optimization
opt_rebal_base <- optimize.portfolio.rebalancing(R = asset_returns, portfolio = port_spec, 
                                                 optimize_method = "ROI", 
                                                 rebalance_on = "quarters", 
                                                 training_period = 60,
                                                 rolling_window = 60)

# Print the results
print(opt_rebal_base)

# Chart the weights
chart.Weights(opt_rebal_base)

# Compute the portfolio returns
returns_base <- Return.portfolio(R = asset_returns, weights = extractWeights(opt_rebal_base))
colnames(returns_base) <- "base"

# Add a risk budge objective
port_spec <- add.objective(portfolio = port_spec, 
                           type = "risk_budget", 
                           name = "StdDev", 
                           min_prisk = 0.05, 
                           max_prisk = 0.10)

# Run the optimization
opt_rebal_rb <- optimize.portfolio.rebalancing(R = asset_returns, 
                                               portfolio = port_spec, 
                                               optimize_method = "DEoptim",
                                               trace = TRUE,
                                               rebalance_on = "quarters", 
                                               training_period = 60,
                                               rolling_window = 60)

# Chart the weights
chart.Weights(opt_rebal_rb)

# Chart the percentage contribution to risk
chart.RiskBudget(opt_rebal_rb, match.col = "StdDev", risk.type = "percentage")

# Compute the portfolio returns
returns_rb <- Return.portfolio(R = asset_returns, weights = extractWeights(opt_rebal_rb))
colnames(returns_rb) <- "risk_budget"

# Define custom moments function
moments_robust <- function(R, portfolio){
  out <- list()
  out$sigma <- MASS::cov.rob(R, method = "mcd")$cov
  out
}

# Run the optimization
opt_rebal_rb_robust <- optimize.portfolio.rebalancing(R = asset_returns, 
                                                      momentFUN = "moments_robust",
                                                      portfolio = port_spec, 
                                                      optimize_method = "DEoptim",
                                                      trace = TRUE,
                                                      rebalance_on = "quarters", 
                                                      training_period = 60,
                                                      rolling_window = 60)

# Chart the weights
chart.Weights(opt_rebal_rb_robust)

# Chart the percentage contribution to risk
chart.RiskBudget(opt_rebal_rb_robust, match.col = "StdDev", risk.type = "percentage")

# Compute the portfolio returns
returns_rb_robust <- Return.portfolio(R = asset_returns, weights = extractWeights(opt_rebal_rb_robust))
colnames(returns_rb_robust) <- "rb_robust"

# Combine the returns
ret <- cbind(edhec[ , 13], r_benchmark, returns_base, returns_rb, returns_rb_robust)

# Compute annualized returns
table.AnnualizedReturns(R = ret)

# Chart the performance summary
charts.PerformanceSummary(R = ret)
