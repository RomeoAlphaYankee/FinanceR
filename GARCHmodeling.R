# GARCH Modeling
library(rugarch)
library(PerformanceAnalytics)
library(quantmod)

# Get market prices and convert to returns
sp500prices <- getSymbols("^GSPC", from = "1989-01-01", auto.assign = FALSE)
sp500ret <- CalculateReturns(Ad(sp500prices))[-1]

# Explore volatility clustering
plot(sp500ret, type = "h")

# Compute the annualized volatility for the complete sample
sd(sp500ret) * sqrt(252)

# Compute the annualized standard deviation for the year 2009
sqrt(252) * sd(sp500ret["2009"])

# Compute the annualized standard deviation for the year 2017 
sqrt(252) * sd(sp500ret["2017"])

# Showing two plots of rolling annualized volatility
par(mfrow=c(2,1)) 

# Compute the rolling 1 month estimate of annualized volatility
chart.RollingPerformance(R = sp500ret["2000/2017"], width = 21,
                         FUN = "sd.annualized", scale = 252, 
                         main = "One month rolling volatility")

# Compute the rolling 3 months estimate of annualized volatility
chart.RollingPerformance(R = sp500ret["2000/2017"], width = 63,
                         FUN = "sd.annualized", scale = 252, 
                         main = "Three months rolling volatility")

# Calculate the mean return
mu <- mean(sp500ret)

# Define the series of prediciton errors
e <- sp500ret - mu

# Plot the absolute value of the prediction errors
par(mfrow = c(2,1),mar = c(3, 2, 2, 2))

plot(abs(e))

# Add plot of autocorrelation
acf(abs(e))

# GARCH model
# Set the parameter values
alpha <- 0.1
beta <- 0.8
omega <- var(sp500ret) * (1 - alpha - beta)

# Set series of prediction errors
e2 <- e^2

# Compute the predicted variances
predvar <- rep(NA, length(sp500ret))

predvar[1] <- var(sp500ret) 

for(t in 2:length(sp500ret)){
  predvar[t] <- omega + alpha * e2[t-1] + beta * predvar[t-1]
}

# Comopare with the unconditional vovlatility
par(mfrow = c(1, 1))
plot(xts(sqrt(predvar), order.by =index(sp500ret)), main = "Predicted Volatility")
uncvol <- sqrt(omega / (1 - alpha - beta))
uncvol <- xts(rep(uncvol, length(sp500ret)), order.by = time(sp500ret))
lines(uncvol, col = "red")

# Create annualized predicted volatility
ann_predvol <- xts(sqrt(252) * sqrt(predvar), order.by = time(sp500ret))

# Plot the annual predicted volatility in 2008 and 2009
par(mfrow = c(1, 1))
plot(ann_predvol["2008/2009"], main = "Ann. S&P 500 vol in 2008-2009")

# Constant mean, standard GARCH(1, 1) model
garchspec <- ugarchspec(mean.model = list(armaOrder = c(0, 0)),
                                          variance.model = list(model = "sGARCH"),
                                          distribution.model = "norm")

# Estimate the GARCH model
garchfit <- ugarchfit(data = sp500ret, 
                      spec = garchspec)

# Coefficients
garchcoef <- coef(garchfit)

# Examine the coefficients
print(garchcoef)

# Unconditional variance and standard deviation
garchuncvar <- uncvariance(garchfit)
sqrt(garchuncvar)

# Predicted mean
garchmean <- fitted(garchfit)
tail(garchmean, 1)

# Estimated volatilities
garchvol <- sigma(garchfit)
plot(garchvol["2008/2009"])

# Forecast the folatility of future returns
garchforecast <- ugarchforecast(fitORspec = garchfit,
                                n.ahead = 5)
# Extract the predicted volatilities and print them
print(sigma(garchforecast))

# Compute the annualized volatility
annualvol <- sqrt(252) * sigma(garchfit)

# Compute the 5% vol target weights  
vt_weights <- 0.05 / annualvol

# Compare the annualized volatility to the portfolio weights in a plot
plot(merge(annualvol, vt_weights), multi.panel = TRUE)

# obtain standardized returns
stdret <- residuals(garchfit, standardize = TRUE)

library(PerformanceAnalytics)
chart.Histogram(sp500ret, methods = c("add.normal", "add.density"),
                colorset = c("gray", "red", "blue"))


# Change distribution model arguement to Student T
garchspec <- ugarchspec(mean.model = list(armaOrder = c(0, 0)),
                        variance.model = list(model = "sGARCH"),
                        distribution.model = "sstd")

# Estimate the GARCH model
garchfit <- ugarchfit(data = sp500ret, 
                      spec = garchspec)

# Inspect the cocefficients
coef(garchfit)

# Compute the standardized returns
stdret <- residuals(garchfit, standardize = TRUE)

# Compute the standardized returns using fitted() and sigma()
stdret <- (ret - fitted(garchfit)) / sigma(garchfit)

# Chart the histogram of returns
chart.Histogram(stdret, methods = c("add.normal","add.density" ), 
     colorset = c("gray","red","blue"))

# Lets use a GJR GARCH model on MSFT returns
msftret<- getSymbols("MSFT", from = "1999-01-01", to = "2017-12-31", auto.assign = FALSE)
msftret <- Delt(Ad(MSFT))
head(msftret)
