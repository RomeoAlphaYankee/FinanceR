### Risk Parity
library(quantmod)

symbols <- c("SPY", "AGG", "EFA", "EEM", "GSG")

getSymbols(symbols)

prices <- na.locf(do.call(merge, lapply(symbols, function(x) Cl(get(x)))))

colnames(prices) <- symbols

returns <- na.omit(ROC(prices))                                  

head(returns)                  

# Set asset allocation weights
wgt <- rep(x = 1 / ncol(returns), times = ncol(returns))

# Portfolio volatility
port.vol <- sqrt(t(wgt) %*% cov(returns) %*% wgt)

# Marginal contribution to risk
mcr <- wgt %*% cov(returns) / port.vol[1, 1]

# Component contribution to risk
cr <- mcr * wgt

# Percent of contribution to overall portfolio risk
cr.port <- cr / port.vol[1, 1]

# Combine cr and cr.port
cr.all <- round(rbind(cr, cr.port), 5)
rownames(cr.all) <- c("Contribution to risk:", "% of risk contribution")

cr.all

# Easy way
library(PerformanceAnalytics)

cr.1 <- StdDev(returns, weights = wgt, portfolio_method = "component")
lapply(cr.1, round, 5)

# Rolling risk contribution
analyze.sd <- function(x, y) {
  a <- StdDev(x, weights = wgt, portfolio_method = "component")
  b <- round(a$pct_contrib_StdDev, 3)
  return(b)
}

cr.rolling <- na.omit(rollapply(returns, 252, 
                                function(x) analyze.sd(x, w),
                                by.column = FALSE,
                                align = "right"))

head(cr.rolling)

dat.1 <- cr.rolling * 100
dat.2 <- dat.1[ , order(-dat.1[nrow(dat.1), ])]

# Plot the rolling risk contribution
plot(dat.2, main = "Risk Conrtibution", lwd = 2)
addLegend(legend.loc = "bottomleft", ncol = 1, lty = 1, legend.names = colnames(dat.2), col = 1:5, lwd = 2)

### Build a dynamic risk parity portfolio
library(FRAPO)

covar.1 <- cov(returns)
risk.parity.w <- PERC(covar.1)
risk.parity.w

# Build a risk parity portfolio using quadprog in portfolio analytics
library(PortfolioAnalytics)
library(ROI)
library(quadprog)
library(ROI.plugin.quadprog)

funds <- colnames(returns)

port.a <- portfolio.spec(assets = funds)
port.a <- add.constraint(portfolio = port.a, type = "long_only")
port.a <- add.constraint(portfolio = port.a, type = "full_investment")
port.a <- add.objective(portfolio = port.a, type = "risk_budget", min_concentration = TRUE, name = "StdDev")

port.a1 <- optimize.portfolio(R = returns, portfolio = port.a,
                              optimize_method = "quadprog", trace = TRUE)

round(extractWeights(port.a1) * 100, 3)
