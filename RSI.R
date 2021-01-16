library(quantmod)
library(PerformanceAnalytics)

getSymbols("XLK")

head(XLK)
XLK$returns <- Return.calculate(Cl(XLK))

XLK.monthly <- to.monthly(XLK)

head(XLK.monthly)

XLK.monthly$returns <- Return.calculate(Cl(XLK.monthly))

hist(XLK$returns, breaks = 30, col = "blue")

getSymbols("XLE")

XLE.monthly <- to.monthly(XLE)

head(XLE.monthly)

XLE.monthly$returns <- Return.calculate(Cl(XLE.monthly))

head(XLE.monthly)

hist(XLE.monthly$returns, breaks = 20, col = "brown")

returns.monthly <- merge(XLK.monthly$returns, XLE.monthly$returns)
  
colnames(returns.monthly) <- c("Tech", "Energy")

head(returns.monthly)

chart.Correlation(returns.monthly)

plot.zoo(RSI(Cl(XLK["2020"])), ylim = c(1, 100), col = "blue", lwd = 2, main = "Tech Sector RSI")
abline(h = 70, col = "red", lty = 2)
abline(h = 30, col = "green", lty = 2)

plot.zoo(RSI(Cl(XLE["2020"])), ylim = c(1, 100), col = "royalblue", lwd = 2, main = "Energy Sector RSI")
abline(h = 70, col = "red", lty = 2)
abline(h = 30, col = "green", lty = 2)
