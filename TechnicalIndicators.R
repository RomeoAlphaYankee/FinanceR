# Technical Indicators
library(quantmod)
library(xts)
library(TTR)

# Acquire price data
AMZN <- getSymbols("AMZN", auto.assign = FALSE)


# Moving average crossover
AMZN.sma <- AMZN$AMZN.Close
AMZN.sma$sma50 <- rollapply(AMZN.sma$AMZN.Close, width = 50, FUN = mean)
AMZN.sma$sma200 <- rollapply(AMZN.sma$AMZN.Close, width = 200, FUN = mean)
tail(AMZN.sma)

span <- "504 days"
AMZN.sma.recent <- last(AMZN.sma, n = span)
plot(AMZN.sma.recent, main = "AMZN - Simple Moving Average")

# Dow Jones Index
djia.sma <- Ad(djia)
djia.sma$sma50 <- rollapply(djia.sma, width = 50, FUN = mean)
djia.sma$sma200 <- rollapply(Ad(djia.sma), width = 200, FUN = mean)
plot(djia.sma["2007/2012"], main = "Dow Jones Industrial Average - Simple Moving Average")


# Bollinger Bands
AMZN.bb <- AMZN$AMZN.Close
AMZN.bb$avg <- rollapply(AMZN.bb$AMZN.Close, width = 20, FUN = mean)
AMZN.bb$sd <- rollapply(AMZN.bb$AMZN.Close, width = 20, FUN = sd)
tail(AMZN.bb)

AMZN.bb <- AMZN.bb["2019/2020"]

AMZN.bb$upper <- AMZN.bb$avg + 2 * AMZN.bb$sd
AMZN.bb$lower <- AMZN.bb$avg - 2 * AMZN.bb$sd

y.range <- range(AMZN.bb[ , -3], na.rm = TRUE)

plot(AMZN.bb$AMZN.Close,
     ylim = y.range,
     main = "AMZN - Bollinger Bands")
lines(AMZN.bb[, 2], col = "blue")
lines(AMZN.bb[ , 4], col = "red", lwd = 2)
lines(AMZN.bb[ , 5], col = "green", lwd = 2)
addLegend("topleft", 
          c("AMZN Price", "SMA - 20 Day", "Upper Band", "Lower Band"),
          lty = 1,
          col = c("black", "blue", "red", "green"))

# DJIA Bollinger Bands
djia.bb <- djia$DJI.Close
djia.bb$avg <- rollapply(djia.bb$DJI.Close, width = 20, FUN = mean)
djia.bb$sd <- rollapply(djia.bb$DJI.Close, width = 20, FUN = sd)
tail(djia.bb)

djia.bb <- djia.bb["2019/2020"]

djia.bb$upper <- djia.bb$avg + 2 * djia.bb$sd
djia.bb$lower <- djia.bb$avg - 2 * djia.bb$sd

y.range <- range(djia.bb[ , -3], na.rm = TRUE)

plot(djia.bb$DJI.Close,
     ylim = y.range,
     main = "Dow Jones Industrial Average - Bollinger Bands")
lines(djia.bb[, 2], col = "blue")
lines(djia.bb[ , 4], col = "red", lwd = 2)
lines(djia.bb[ , 5], col = "green", lwd = 2)
addLegend("topleft", 
          c("DJI Price", "SMA - 20 Day", "Upper Band", "Lower Band"),
          lty = 1,
          col = c("black", "blue", "red", "green"))

# Relative Strength Indicator
AMZN.rsi <- AMZN$AMZN.Close
AMZN.rsi$delta <- diff(AMZN.rsi$AMZN.Close)
AMZN.rsi$up <- ifelse(AMZN.rsi$delta > 0, AMZN.rsi$delta, 0)
AMZN.rsi$down <- abs(ifelse(AMZN.rsi$delta < 0, AMZN.rsi$delta, 0))
AMZN.rsi$up.avg <- rollapply(AMZN.rsi$up, width = 14, FUN = mean)
AMZN.rsi$down.avg <- rollapply(AMZN.rsi$down, width = 14, FUN = mean)

AMZN.rsi <- data.frame(AMZN.rsi["2019/2020", ])

for(i in 2:nrow(AMZN.rsi)){
  AMZN.rsi$up.avg[i] <- (AMZN.rsi$up.avg[i - 1] * 13 + AMZN.rsi$up[i]) / 14
}

for(i in 2:nrow(AMZN.rsi)){
  AMZN.rsi$down.avg[i] <- ((AMZN.rsi$down.avg[i - 1] * 13 + AMZN.rsi$down[i]) / 14)
}

AMZN.rsi$RS <- AMZN.rsi$up.avg / AMZN.rsi$down.avg
AMZN.rsi$RSI <- 100 - (100 / (1 + AMZN.rsi$RS))

AMZN.rsi <- as.xts(AMZN.rsi, order.by = as.Date(rownames(AMZN.rsi)))

plot(x = index(AMZN.rsi), y = AMZN.rsi$RSI,
     xlab = "Date",
     ylab = "AMZN - RSI (14-Day Moving Avg.)",
     ylim = c(0, 100), 
     type = "l",
     main = "Amazon Relative Strength Index")
abline(h = c(30, 70), lty = 2, col = "red")

# All three together
par(mfrow = c(3, 1))

y.range <- range(AMZN.bb["2019", -3], na.rm = TRUE)


plot(AMZN.bb$AMZN.Close["2019/"],
     main = "AMZN - Bollinger Bands")
lines(AMZN.bb[, 2], col = "blue")
lines(AMZN.bb[ , 4], col = "red", lwd = 2)
lines(AMZN.bb[ , 5], col = "green", lwd = 2)
addLegend("topleft", 
          c("AMZN Price", "SMA - 20 Day", "Upper Band", "Lower Band"),
          lty = 1,
          col = c("black", "blue", "red", "green"))

plot(AMZN.sma.recent["2019/"], main = "AMZN - 50/200 Day Moving Averages")

plot(x = index(AMZN.rsi["2019/"]), y = AMZN.rsi$RSI["2019/"],
     xlab = "Date",
     ylab = "AMZN - RSI (14-Day Moving Avg.)",
     ylim = c(0, 100), 
     type = "l",
     main = "Amazon Relative Strength Index")
abline(h = c(30, 70), lty = 2, col = "red")
