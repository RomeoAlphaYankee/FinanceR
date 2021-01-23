# Tail risk analysis
library(quantmod)
library(PortfolioAnalytics)

data(managers)

sp500.ret <- managers$`SP500 TR`

# Trailing expected value at risk of S&P 500
chart.BarVaR(R = sp500.ret, legend.loc = "topleft",
             methods = "HistoricalES",
             main = "S&P 500 Returns and ES",
             lty = c(1, 2),              
             legend.cex = 0.8, width = 12)

# Modeling tail risk
library(MASS)
fit.t <- fitdistr(sp500.ret, "t")
VaR.t <- fit.t$estimate[2] * qt(df = fit.t$estimate[3], p = 0.05)
names(VaR.t) <- "VaR.t"

library(TTR)
ES.h <- ES(sp500.ret)
names(ES.h) <- "ES.hist"

VaR.h <- VaR(sp500.ret)
names(VaR.h) <- "VaR.hist"

VaR.norm <- qnorm(p = 0.05, mean = mean(sp500.ret), sd = sd(sp500.ret))
names(VaR.norm) <- "VaR.norm"

hist(sp500.ret, breaks = 50,
     freq = FALSE,
     main = "Distribution of S&P 500 Returns",
     col = "cornflowerblue")
abline(v = VaR.norm, lwd = 2, lty = 1)
abline(v = VaR.t, lwd = 2, lty = 2)
abline(v = VaR.h, lwd = 2, lty = 3)
abline(v = ES.h, lwd = 2, lty = 4)
legend("topleft", legend = c("VaR.norm", "VaR.t", "VaR.hist", "ES.hist"), 
       col = "black",
       lty = c(1, 2, 3, 4))


# tickers for stock and bond data ETFs
tickers <- c("VFINX", "VFITX")

getSymbols(tickers, from = as.Date("1989-12-20"))

prices <- do.call(merge, lapply(tickers, function(x) Cl(get(x))))
head(prices)

ret <- ROC(prices, 1)
ret <- na.omit(ret)
head(ret)

colnames(ret) <- tickers

# Generate 60/40 portfolio data
wgt <- c(0.6, 0.4)

port.60.40 <- Return.rebalancing(R = ret,
                                 weights = wgt,
                                 wealth.index = FALSE,
                                 rebalance_on = "years")

head(port.60.40)

# Density histogram
hist(port.60.40, freq = F,
     breaks = 100, 
     col = "cornflowerblue",
     main = "Distirbution of 60/40 Returns",
     panel.first = grid())

curve(dnorm(x,
            mean(port.60.40),
            sd(port.60.40)),
      add = TRUE,
      col = "black",
      lwd = 2)

legend("topright",
       c("Empirical", "Normal"),
       fill = c("cornflowerblue", "black"))

abline(v = VaR(port.60.40), lty = 2)

# Extreme Value Theory
# Empirical CDF  plot
plot.ecdf(x = as.numeric(port.60.40),
          main = "CDF Chart",
          xlab = "Return",
          col = "gray50",
          lwd = 2,
          panel.first = grid())

curve(pnorm(x, mean(port.60.40), sd(port.60.40)),
      add = TRUE,
      lwd = 2,
      col = "black")

mtext(side = 5,
      "Based on daily returns for 60/40 strategy",
      line = 0.2)
legend("topleft", c("Empirical", "Normal"),
       fill = c("grey", "black"))

abline(v = 0, h = 0.5, lty = 2)

# Zoom in on left tail
plot.ecdf(x = as.numeric(port.60.40),
          main = "Left Tail: CDF Chart",
          xlab = "Return",
          xlim = c(-0.05, -0.01), ylim = c(0, 0.04),
          lwd = 2, cex.points = 1.5,
          verticals = TRUE,
          panel.first = grid())

curve(pnorm(x, mean(port.60.40), sd(port.60.40)),
      add = TRUE, lwd = 2, lty = 2, col = "black")

legend("topleft", c("Empirical", "Normal"),
       lty = c(1, 2), bg = "white")

# Q-Q plot
qqnorm(port.60.40, panel.first = grid())
qqline(port.60.40)
abline(h = -0.01, lty = 2)

# Continue with extreme value analysis
library(fExtremes)

# Model
gpd.fit.1 <- gpdFit(as.numeric(port.60.40),
                    u = -0.01)

# Estimate tail risk
tailRisk(gpd.fit.1, prob = 0.99)

# Compare to historical tail risk
var.hist <- PerformanceAnalytics::VaR(port.60.40,
                                      p = 0.99, 
                                      method = "historical")

es.hist <- PerformanceAnalytics::ES(port.60.40,
                                    p = 0.99,
                                    method = "historical")

# Simulate extreme tail events over a longer time horizon
gpd.sim.1 <- rgpd(1000000,
                  xi = gpd.fit.1@fit$par.ests[1],
                  beta = gpd.fit.1@fit$par.ests[2])

max(gpd.sim.1)

# Plot a left-tail histogram
truehist(gpd.sim.1 * -1,
         main = "Simulated Left-Tail Density Histogram",
         ylim = c(0, 1),
         xlim = c(-0.06, -0.035),
         xlab = "Estimated Daily Loss",
         col = "grey",
         panel.first = grid())

box()
