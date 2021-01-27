### Estimating the effects of stocks on asset prices
library(vars)
library(quantmod)

fred.tickers <- c("MCOILWTICO")
getSymbols(fred.tickers, src = "FRED", auto.assign = TRUE)

head(MCOILWTICO)

SP500 <- getSymbols("^GSPC", src = "yahoo", auto.assign = FALSE, from = as.Date("1985-12-31"))
head(SP500)


sp500 <- Ad(to.monthly(SP500, indexAt = 'firstof', OHLD = FALSE))
head(sp500)

# combine data sets
oil.sp500.a <- na.omit(merge(MCOILWTICO, sp500))

head(oil.sp500.a)

# Calculate returns
oil.sp500.ret <- ROC(oil.sp500.a, 1, "discrete", na.pad = FALSE)
colnames(oil.sp500.ret) <- c("WTI", "SP500")

# Vector Auto-regression lag selection
var.lag <- VARselect(oil.sp500.ret)

var.model <- VAR(oil.sp500.ret * 100, p = var.lag$selection[1])

# Plot
model <- irf(var.model,
         impulse = "WTI",
         response = "SP500",
         boot = TRUE,
         n.ahead = 12,
         cumulative = FALSE)

plot(model, main = "Impulse Response From Oil")


# Examine the date
model$irf
model$Upper
model$Lower

# Examine negative shocks
plot(model$irf$WTI * -1, type = "l",
     main = "Response of S&P 500 to Negative WTI Price Shock",
     ylab = "Response", xlab = "Months")

# Examine 2 standard deviation shocks
model$irf$WTI * 2

plot(model$irf$WTI * 2, type = "l",
     main = "Response of S&P 500 to 2 Standard Deviation Shock in WTI",
     ylab = "S&P 500 Response", xlab = "Months")
