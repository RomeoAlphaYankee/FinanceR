library(quantmod)
library(PerformanceAnalytics)

tickers <- c("EQR", "AVB", "ESS", "UDR", "AIV", "MAA", "CPT")

getSymbols(tickers)

Ad(eval(parse(text = tickers)))

tmp <- NULL
apts <- NULL

for(ticker in tickers){
  tmp <- Ad(eval(parse(text = ticker)))
  apts <- cbind(apts, tmp)
}

head(apts)

names(apts) <- tickers

head(apts)

pairs(as.zoo(apts))

cor(apts)
cov(apts)

plot.zoo(apts[ , 1:7])

# Strongest correlation between EQR and AVB
apts$ratio <- apts$AVB / apts$EQR
plot(apts$ratio)
sd(apts$ratio)
mean(apts$ratio)


# Establish some technical indicators
apts$sma <- rollapply(apts$ratio, mean, width = 252 * 5)
apts$sd <- rollapply(apts$ratio, sd, width = 252)

# Plot some indicators
par(mfrow = c(2, 1))
plot(apts$ratio)

plot(apts$ratio["2010/"], main = "AVB / EQR")
lines(mean(apts$ratio["2010/"]) + 2 * apts$sd, col = "red")
lines(mean(apts$ratio["2010/"]) - 2 * apts$sd, col = "red")
lines(apts["2010/", "sma"], col = "blue")
lines(apts$sma252 + 2 * apts$sd, col = "green")
lines(apts$sma252 - 2 * apts$sd, col = "green")

Rel.strength <- data.frame(RSI(apts$ratio)["2010/"])
plot(x = as.Date(rownames(Rel.strength)), y = Rel.strength$rsi, type = "l")
abline(h = c(30, 70), lty = 2, col = "red")

# Second strongest correlation is between UDR and AIV
apts$ratio2 <- apts$AIV / apts$UDR
apts$sma2 <- rollapply(apts$ratio2, mean, width = 252 * 5)
apts$sd2 <- rollapply(apts$ratio2, sd, width = 252)

plot(apts$ratio2["2010/"], main = "AIV / UDR")
lines(mean(apts$ratio2["2010/"]) + 2 * apts$sd2, col = "red")
lines(mean(apts$ratio2["2010/"]) - 2 * apts$sd2, col = "red")
lines(apts["2010/", "sma2"], col = "blue")
lines(apts$sma2 + 2 * apts$sd2, col = "green")
lines(apts$sma2 - 2 * apts$sd2, col = "green")

Rel.strength <- data.frame(RSI(apts$ratio)["2010/"])
plot(x = as.Date(rownames(Rel.strength)), y = Rel.strength$rsi, type = "l")
abline(h = c(30, 70), lty = 2, col = "red")

# Third strongest correlation is between MAA and CPT
apts$ratio3 <- apts$MAA / apts$CPT
apts$sma3 <- rollapply(apts$ratio3, mean, width = 252 * 5)
apts$sd3 <- rollapply(apts$ratio3, sd, width = 252)

plot(apts$ratio3["2010/"], main = "MAA / CPT")
lines(mean(apts$ratio3["2010/"]) + 2 * apts$sd3, col = "red")
lines(mean(apts$ratio3["2010/"]) - 2 * apts$sd3, col = "red")
lines(apts["2010/", "sma3"], col = "blue")
lines(apts$sma3 + 2 * apts$sd3, col = "green")
lines(apts$sma3 - 2 * apts$sd3, col = "green")

# Fourth strongest correlation is between ESS and UDR
apts$ratio4 <- apts$ESS / apts$UDR
apts$sma4 <- rollapply(apts$ratio4, mean, width = 252 * 5)
apts$sd4 <- rollapply(apts$ratio4, sd, width = 252)

plot(apts$ratio4["2010/"], main = "ESS / UDR")
lines(mean(apts$ratio4["2010/"]) + 2 * apts$sd4, col = "red")
lines(mean(apts$ratio4["2010/"]) - 2 * apts$sd4["2010/"], col = "red")

lines(apts["2010/", "sma4"], col = "blue")
lines(apts$sma4 + 2 * apts$sd4, col = "green")
lines(apts$sma4 - 2 * apts$sd4, col = "green")
