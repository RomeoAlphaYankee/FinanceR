library(xts)
library(TTR)
library(fPortfolio)
library(fAssets)
library(DEoptimR)
library(timeSeries)

# Open returns from hedge funds, edhec, S&P500, 10yr, 3mo
ret.ts <- as.timeSeries(na.omit(managers[ , 5:10]))

n.col <- ncol(ret.ts)

# Set a box weight constraint
min.max <- c("minW[1:n.col] = 0.1", "maxW[1:n.col] = 0.5")

# Calculate the optimized portfolio using weight box constraint
min.var.constraints <- minvariancePortfolio(data = ret.ts, 
                                            constraints = min.max)
# Check constraints
min.var.constraints

# Examine weights
round(min.var.constraints@portfolio@portfolio$weights * 100, 2)

# Calculate efficient frontier
eff.frontier <- portfolioFrontier(ret.ts)

# Chart
plot(eff.frontier)
1

eff.frontier

# plot weights
weightsPlot(eff.frontier)


# Add a weight constraint for the hedge fund category
min.max.1 <- c("minW[1:n.col] = 0.10",
               "maxW[1:n.col] = 0.50",
               "minsumW[1:3] = 0.35",
               "maxsumW[1:3] = 0.45")

min.var.constraints.1 <- minvariancePortfolio(ret.ts,
                                              constraints = min.max.1)

round(min.var.constraints.1@portfolio@portfolio$weights * 100, 2)