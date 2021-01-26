### Style Analysis and Replicating Indexes
library(quantmod)
symbols <- c("SPY", "AGG", "EFA", "EEM", "GSG", "AOR")

getSymbols(Symbols = symbols, auto.assign = TRUE)

prices <- do.call(merge, lapply(symbols, function(x) to.monthly(Ad(get(x)), OHLC = FALSE)))

returns <- na.omit(ROC(prices["2010-12-31/2017-12-31"]))

colnames(returns) <- symbols

head(returns)

# Style analysis and factor analytics
library(FactorAnalytics)

aor.style <- style.fit(R.fund = returns$AOR,
                       R.style = returns[ , 1:5],
                       method = "constrained",
                       leverage = FALSE)

round(aor.style$weights, 3)

# Compare Factor Analytics style.fit with a linear regression
regression.1 <- lm(AOR ~., data = returns)

summary(regression.1)

# Rolling period style analysis
style.roll.weights <- table.RollingStyle(R.fund = returns$AOR,
                                         R.style = returns[ , 1:5],
                                         method = "constrained",
                                         leverage = FALSE)

# Inspect
round(head(style.roll.weights, 3), 3)

# Plot
matplot(style.roll.weights, type = "l")

# Bar chart
chart.RollingStyle(R.fund = returns$AOR,
                   main = "Style Weighs for AOR",
                   R.style = returns[ , 1:5],
                   method = "constrained",
                   leverage = FALSE,
                   ylab = "Weight")

# Replicate the S&P 500 using sector ETFs
symbols <- c("XLF", "XLK", "XLI",
             "XLB", "XLY", "XLV",
             "XLU", "XLP", "XLE",
             "XLC", "XLRE", "SPY")

getSymbols(Symbols = symbols, auto.assign = TRUE)

prices <- do.call(merge, lapply(symbols, function(x) to.monthly(Ad(get(x)), OHLC = FALSE)))

colnames(prices) <- symbols

returns <- na.omit(ROC(prices))

head(returns)

# Generate style weights
style.roll.weights.sp <- table.RollingStyle(R.fund = returns$SPY,
                                            R.style = returns[ , -ncol(returns)],
                                            method = "constrained",
                                            leverage = FALSE)

style.roll.weights.sp

chart.RollingStyle(R.fund = returns$SPY, R.style = returns[ , -ncol(returns)],
                   method = "constrained", leverage = FALSE,
                   main = "S&P Sector Weights",
                   ylab = "Weight")

# Create replicated SPY indes with quarterly realancing
spy.rep <- Return.rebalancing(R = returns[ , -ncol(returns)], weights, style.roll.weights.sp,
                              rebalance_on = "quarters",
                              verbose = TRUE)$wealthindex

spy.actual <- cumprod(1 + returns$SPY)

# Combine S&P 500 and replicated index
spy.rep.actual <- merge(spy.rep, spy.actual)

colnames(spy.rep.actual) <- c("Replicated", "Actual")

# Plot 
plot(spy.rep.actual,
        type = "l", lty = c(1, 2),
        main = "SPY: Actual vs. Replicated",
        col = "black")
addLegend("topleft", lty = c(1, 2),
       c(colnames(spy.rep.actual)),
       col = "black")

# Check the correlation
spy.rep.actual.ret <- ROC(spy.rep.actual, 1, "discrete", na.pad = FALSE)
spy.actual.corr <- cor(spy.rep.actual.ret)
spy.actual.corr


###
# Compare to one of my old funds
getSymbols("SOAVX")
returns <- na.omit(merge(returns, ROC(Ad(to.monthly(SOAVX)))))
colnames(returns) <- c(symbols, "SOAVX")

# Generate style weights
style.roll.weights.vx <- table.RollingStyle(R.fund = returns$SOAVX,
                                            R.style = returns[ , 1:11],
                                            method = "constrained",
                                            leverage = FALSE)

# Inspect weights
tail(style.roll.weights.vx)

# Chart weights
chart.RollingStyle(R.fund = returns$SOAVX, R.style = returns[ , 1:11],
                   method = "constrained", leverage = FALSE,
                   main = "SOAVX Sector Weights",
                   ylab = "Weight")

# Create replicated SOAVX with quarterly realancing
soavx.rep <- Return.rebalancing(R = returns[ , 1:11], weights, style.roll.weights.vx,
                              rebalance_on = "quarters",
                              verbose = TRUE)$wealthindex

soavx.actual <- cumprod(1 + returns$SOAVX)

# Combine SOAVX and replicated index
soavx.rep.actual <- merge(soavx.rep, soavx.actual, spy.actual)

colnames(soavx.rep.actual) <- c("Replicated VX", "Actual VX", "S&P 500")

# Plot 
plot(soavx.rep.actual,
     type = "l", lty = c(1, 2, 3),
     main = "SOAVX: Actual vs. Replicated",
     col = "black")

addLegend("topleft", lty = c(1, 2, 3),
          c(colnames(soavx.rep.actual)),
          col = "black")

# Check with regression
lm(SOAVX ~., data = returns[ , -12])
lm(SOAVX ~ SPY, data = returns)
