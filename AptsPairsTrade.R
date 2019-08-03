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

plot.zoo(apts[ , 1:4])

# Strongest correlation between EQR and AVB
apts$ratio <- apts$AVB / apts$EQR
plot(apts$ratio)
sd(apts$ratio)
mean(apts$ratio)

apts$sma <- rollapply(apts$ratio, mean, width = 20)
apts$sma252 <- rollapply(apts$ratio, mean, width = 252 * 5)
apts$sd <- rollapply(apts$ratio, sd, width = 252)

plot(apts$ratio["2010/"], main = "AVB / EQR")
lines(mean(apts$ratio["2010/"]) + 2 * apts$sd, col = "red")
lines(mean(apts$ratio["2010/"]) - 2 * apts$sd, col = "red")
lines(apts["2010/", "sma252"], col = "blue")
lines(apts$sma252 + 2 * apts$sd, col = "green")
lines(apts$sma252 - 2 * apts$sd, col = "green")

# Second strongest correlation is between UDR and AIV
apts$ratio2 <- apts[ , 4] / apts[ , 3]
apts$sma2 <- rollapply(apts$ratio2, mean, width = 252 * 5)
apts$sd2 <- rollapply(apts$ratio2, sd, width = 252)

plot(apts$ratio2["2010/"], main = "AIV / UDR")
lines(mean(apts$ratio2["2010/"]) + 2 * apts$sd2, col = "red")
lines(mean(apts$ratio2["2010/"]) - 2 * apts$sd2, col = "red")
lines(apts["2010/", "sma2"], col = "blue")
lines(apts$sma2 + 2 * apts$sd2, col = "green")
lines(apts$sma2 - 2 * apts$sd2, col = "green")

# Third strongest correlation is between MAA and CPT
apts$ratio3 <- apts[ , 6] / apts[ , 5]
apts$sma3 <- rollapply(apts$ratio3, mean, width = 252 * 5)
apts$sd3 <- rollapply(apts$ratio3, sd, width = 252)

plot(apts$ratio3["2010/"], main = "CPT / MAA")
lines(mean(apts$ratio3["2010/"]) + 2 * apts$sd3, col = "red")
lines(mean(apts$ratio3["2010/"]) - 2 * apts$sd3, col = "red")
lines(apts["2010/", "sma3"], col = "blue")
lines(apts$sma3 + 2 * apts$sd3, col = "green")
lines(apts$sma3 - 2 * apts$sd3, col = "green")