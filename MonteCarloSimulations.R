library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(xts)

# Monte Carlo Simulations

# Use managers data set from PerformanceAnalytics
data(managers)
head(managers)

# S&P 500 total return
ret.sp500 <- managers$`SP500 TR`

# simulate S&P 500 returns
ret.sp500.sim.1 <- sample(ret.sp500, replace = TRUE)

ret.all <- cbind(ret.sp500, as.numeric(ret.sp500.sim.1))

# Plot returns vs. simulated returns
plot(ret.all)
matplot(ret.all, type = "l")

# Simulate multi-series returns of stocks, bonds, and t-bills
ret.a <- managers[ , 8:10]
wgt.a <- c(0.6, 0.3, 0.1)

ret.a.sim <- apply(ret.a, 2, FUN = sample, replace = TRUE) 

ret.a.sim.1 <- xts(ret.a.sim, order.by = index(ret.a))

ret.a.sim.1

cor(ret.a)
cor(ret.a.sim.1)

# Let's devise a sampling scheme that maintains the real correlations
i <- 1:nrow(ret.a)
i <- sample(i, replace = FALSE)

ret.a.sim <- matrix(as.numeric(ret.a), ncol = 3)[i, ]

head(ret.a)
head(ret.a.sim)


ret.a.sim.2 <- xts(ret.a.sim, order.by = index(ret.a))

# Plot to confirm xts hasn't matched the returns
plot(ret.a)
plot(ret.a.sim.2)

# Check correlations
cor(ret.a)
cor(ret.a.sim.2)

# Correlations check out. Resample with replace = T
i <- 1:nrow(ret.a)
ind <- sample(i, replace = TRUE)
ret.a.sim <- matrix(as.numeric(ret.a), ncol = 3)[ind, ]
ret.a.sim <- xts(ret.a.sim, order.by = index(ret.a))

# Plot
port.a <- Return.rebalancing(R = ret.a, weights = wgt.a, rebalance_on = "years", wealth.index = TRUE)
port.a.1 <- Return.rebalancing(R = ret.a.sim.1, weights = wgt.a, rebalance_on = "years", wealth.index = TRUE)
port.a.2 <- Return.rebalancing(R = ret.a.sim.2, weights = wgt.a, rebalance_on = "years", wealth.index = TRUE)
port.a.3 <- Return.rebalancing(R = ret.a.sim.3, weights = wgt.a, rebalance_on = "years", wealth.index = TRUE)

# Plot results of simulations
plot(port.a)
lines(port.a.1, lty = 2)
lines(port.a.2, lty = 2)
lines(port.a.3, lty = 2)

# Try to replicate numerous portfolios
rm(portfolios)
portfolios <- xts(order.by = index(ret.a))

n <- 100
i <- 1:nrow(ret.a)

for (prt in 1:n) {
  ind <- sample(i, replace = TRUE)
  ret.a.sim <- matrix(as.numeric(ret.a), ncol = 3)[ind, ]
  ret.a.sim <- xts(ret.a.sim, order.by = index(ret.a))
  port.a.sim <- Return.rebalancing(R = ret.a.sim, weights = wgt.a, rebalance_on = "years", wealth.index = TRUE)
  portfolios <-merge(portfolios, port.a.sim)
}


# Create a mean portfolio
mean.port <- rowMeans(portfolios)
mean.port <- xts(mean.port, order.by = index(ret.a))

# Plot actual returns with n simulated returns
plot(port.a, ylim = c(0.5, 5), col = "red", lwd = 2,
     main = "Simulated Portfolios With Actual and Mean Returns",
     ylab = "Wealth Index")
lines(portfolios, lty = 2, col = "black")
lines(mean.port, col = 'blue', lwd = 2)

###
# Non-parametric simulations

library(MASS)

assets.sim <- mvrnorm(n = nrow(ret.a), mu = colMeans(ret.a), 
                      Sigma = var(ret.a))

head(assets.sim)

colMeans(assets.sim)
colMeans(ret.a)

var(ret.a)
var(assets.sim)

# Create fat-tails distribution
tdist.ret <- rt(1000000, df = 5.83) * 0.01
summary(tdist.ret)
sd(tdist.ret)

# Grab actual data
spx <- getSymbols("SPY", from = as.Date("1994-12-15"), to = as.Date("2020-12-31")  , auto.assign = FALSE)
spx <- Return.calculate(Ad(to.monthly(spx)))

spx <- spx[-1, ]
head(spx)
tail(spx)

# Fit T distribution
fitdistr(spx, "t")
fitdistr(managers[ , 8], "t")

spx.t.model <- fitdistr(spx, "t")
spx.t.model

# Create simulated returns based upon Student's T-distribution
library(QRM)
library(mvtnorm)

fit <- fit.mst(managers[ , 8:10])
mu <- fit$mu
Sigma <- fit$Sigma
nu <- fit$df
n <- nrow(managers[ , 8:10])

# Simulate the returns
sim.dat <- rmvt(n = n, sigma = Sigma, df = nu, delta = mu)

sim.dat


# Nonparametric bootstrapping
library(boot)

hf.ret <- managers[ , c(1, 3, 4, 8)]

colnames(hf.ret) <- c("HF-A", "HF-B", "HF-C", "SP500")

coeff <- lm(SP500 ~., data = hf.ret) 

# Check betas
coeff

# Write a function to check how betas could change over time
coef.fun <- function(equation, data, x) {
  a <- data[x, ]
  output <- lm(equation, data = a)
  return(coef(output))
}

results <- boot(data = hf.ret,
                statistic = coef.fun,
                R = 1000,
                equation = SP500 ~.)

str(results)

boot.ci(results, type = "basic", index = 2)

# Plot distributions
plot(results, index = 1)
plot(results, index = 2)
plot(results, index = 3)
