# Data Center REITs

library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)

to <- Sys.Date()
from <- to - (365.25 * 5)


data_tickers <- c("DLR", "COR", "EQIX", "CONE", "QTS")

SPX <- getSymbols("SPY", from = from, to = to, auto.assign = FALSE)

data_centers <- xts(order.by = index(SPX))

for(ticker in data_tickers){
  temp <- getSymbols(ticker, from = from, to = to, auto.assign = FALSE)
  temp <- Ad(temp)
  data_centers <- merge(data_centers, temp)
}

head(data_centers)
tail(data_centers)

colnames(data_centers) <- data_tickers

data_center_returns <- apply(data_centers, FUN = Return.calculate, MARGIN = 2)
data_center_returns <- xts(data_center_returns, order.by = index(data_centers))
data_center_returns <- data_center_returns[-1, ]

# Inspect cumulative returns for crossovers
chart.CumReturns(data_center_returns, legend.loc = 'topleft')

# Inspect returns for correlation and covariance
cor(data_center_returns)
cov(data_center_returns)

# DLR vs. CONE
dlr_cone <- data_centers$DLR / data_centers$CONE
dlr_cone$mean <- rollapply(dlr_cone, width = 252, FUN = mean)
dlr_cone$sd <- rollapply(dlr_cone$DLR, width = 252, FUN = sd)

names(dlr_cone) <- c("ratio", "mean", "sd")
head(dlr_cone)
tail(dlr_cone)

plot(dlr_cone$ratio, main = "DLR / CONE")
lines(dlr_cone$mean, col = "blue")
lines(dlr_cone$mean + dlr_cone$sd * 2, col = "red")
lines(dlr_cone$mean - dlr_cone$sd * 2, col = "green")

#EQIX vs. DLR
eqix_dlr <- data_centers$EQIX / data_centers$DLR

eqix_dlr$mean <- rollapply(eqix_dlr$EQIX, FUN = mean, width = 252)
eqix_dlr$sd <- rollapply(eqix_dlr$EQIX, FUN = sd, width = 252)

names(eqix_dlr) <- c("ratio", "mean", "sd")

tail(eqix_dlr)

plot(eqix_dlr$ratio, main = "EQIX / DLR")
lines(eqix_dlr$mean, col = "blue")
lines(eqix_dlr$mean + 2 * eqix_dlr$sd, col = "red")
lines(eqix_dlr$mean - 2 * eqix_dlr$sd, col = "green")

#COR vs. CONE.
cor_cone <- data_centers$COR / data_centers$CONE
cor_cone$mean <- rollapply(cor_cone, width = 252, FUN = mean)  
cor_cone$sd <- rollapply(cor_cone$COR, width = 252, FUN = sd)

names(cor_cone) <- c("ratio", "mean", "sd")

plot(cor_cone$ratio, main = "COR / CONE")
lines(cor_cone$mean, col = "blue")
lines(cor_cone$mean + cor_cone$sd, col = "red")
lines(cor_cone$mean + cor_cone$sd * 2, col = "red")
lines(cor_cone$mean - cor_cone$sd, col = "green")
lines(cor_cone$mean - cor_cone$sd * 2, col = "green")


cor(data_center_returns)
cov(data_center_returns)
