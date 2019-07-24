# Quantitative Risk Management
library(xts)
library(QRM)
library(qrmtools)
library(qrmdata)

## First explore different types of time series data
## that may be included in an investment portfolio

# Load DJIA data
data("DJ")
data("DJ_const")

# Subset for financial crisis
dj0809 <- DJ["2008/2009"]
plot(dj0809)

# Compute log returns
dj0809_x <- diff(log(dj0809))[-1, ]
plot(dj0809_x, type = "h")
hist(dj0809_x, breaks = 20, col = "grey")

# Extract contituents
tail(DJ_const)
stocks <- DJ_const["2008/2009", c(1:4)]
plot.zoo(stocks)
plot.zoo(stocks, plot.type = "single", col = 1:4)
legend(julian(x = as.Date("2009-01-01")), y = 75, legend = names(stocks)[1:4], fill = 1:4)

# Compute log returns
stocks_x <- diff(log(stocks))[-1, ]
plot.zoo(stocks_x, type = "h")
pairs(as.zoo(stocks_x))
cor(stocks_x)

# Calculate monthly returns
stocks_x_mon <- apply.monthly(stocks_x, FUN = colSums)
plot.zoo(stocks_x_mon, type = "l")

# Look at S&P 500 data
data("SP500")
summary(SP500)
plot(SP500)
SP_returns <- diff(log(SP500))
plot(SP_returns, type = "h")
hist(SP_returns, breaks = 50)

plot(apply.weekly(SP_returns, FUN = sum))
plot(apply.quarterly(SP_returns, sum), type = "h")

# Look at fx rates
data("GBP_USD")
data("EUR_USD")

# Plot log differences
exrate <- diff(log(GBP_USD))[-1, ]
plot(exrate)

fx <- merge(GBP_USD, EUR_USD, all = TRUE)
fx1015 <- fx["2010/2015", ]
plot.zoo(fx1015)

# Look at commodities price correlation
data("GOLD")
data("OIL_Brent")

plot.zoo(cbind(GOLD, OIL_Brent))

# Calculate daily log-returns
goldx <- diff(log(GOLD))
oilx <- diff(log(OIL_Brent))

# Calculate monthly log-returns
goldx_m <- apply.monthly(goldx, sum)
oilx_m <- apply.monthly(oilx, sum)

# Merge the commodities return data
coms <- merge(goldx_m, oilx_m)["1990/"]
plot.zoo(coms, type = "h")

# Correlation scatterplot
cor(na.locf(coms))
pairs(as.zoo(coms))

# Look at zero coupon bond yield data
data("ZCB_CA")
zcb <- ZCB_CA["2006/2015"] * 100
head(zcb)
tail(zcb)

# Calculate the log returns and simple returns
zcb_x <- diff(log(zcb))
zcb_x2 <- diff(zcb)

# Plot the changes in yield
plot.zoo(zcb_x[ , c("1.00y", "5.00y", "10.00y")])
plot.zoo(zcb_x2[ , c("1.00y", "5.00y", "10.00y")])

# Plot the yield curve for the first and last day in the series
colnames(zcb)
maturities <- seq(0.25, 30, 0.25)
maturities

plot(maturities, zcb[1, ], ylim = range(zcb), type = "l", ylab = "yield %", col = "red")
lines(maturities, zcb[nrow(zcb), ])

# Look at different stock indices to compare returns to a normal distribution
# Apply to FTSE data
data("FTSE")
ftse <- diff(log(FTSE))
head(ftse)
ftse <- ftse["2008/2009"]
ftse <- sort(coredata(ftse))

# calculate mean and standard deviation
mu <- mean(ftse)
sigma <- sd(ftse)
c(mu, sigma)

# Plot histogram and superimpose normal distribution
hist(ftse, breaks = 20, probability = TRUE, main = "FTSE Returns")
lines(ftse, dnorm(ftse, mu, sigma), col = "red")

plot(density(ftse))
lines(ftse, dnorm(ftse, mu, sigma), col = "red")

# Test for normality with a quantile-quantile plot
qqnorm(ftse)
qqline(ftse, col = "red")

library(moments)
skewness(ftse)
kurtosis(ftse)
jarque.test(ftse)

# Apply to DJIA
dj_returns <- diff(log(DJ))
djx <- sort(coredata(dj_returns["2008/2009"]))
summary(djx)

# Calculate mean and sd
mu <- mean(djx)
sigma <- sd(djx)
c(mu, sigma)

# Plot histogram and superimpose narmal distribution
hist(djx, probability = TRUE, breaks = 20, main = "Probability Histogram of DJIA Returns")
lines(djx, dnorm(djx, mu, sigma), col = "red")

# Plot non-parametric KDE of djx
plot(density(djx), main = "Density Curve of DJIA Returns")
lines(djx, dnorm(djx, mu, sigma), col = "red")

# Test for normal distribution with Q-Q plot
qqnorm(djx)
qqline(djx, col = "red")

# Broaden the time horizon
djx <- dj_returns["2008/2011"]
summary(djx)

# Check distribution skewness and kurtosis
skewness(djx)
kurtosis(djx)
jarque.test(djx)

# Plot skewness and kurtosis of DJIA constituents
dj_const_returns <- diff(log(DJ_const))
dj_const_returns <- dj_const_returns["2008/2011"]

s <- apply(X = dj_const_returns, MARGIN = 2, FUN = skewness)
k <- apply(X = dj_const_returns, MARGIN = 2, FUN = kurtosis)

plot(s, k, type = "n", xlab = "Skewness", ylab = "Kurtosis")
text(s, k, names(s), cex = 0.6)

# Aggregate the returns over longer periods
# Check distribution of constituent returns
djia_w <- apply.weekly(dj_const_returns, colSums, na.rm = TRUE)
djia_m <- apply.monthly(dj_const_returns, colSums, na.rm = TRUE)

# Calculate the p-value for each series in djia
apply(dj_const_returns, 2, function(v){jarque.test(v)$p.value})
apply(djia_w, 2, function(v){jarque.test(v)$p.value})
apply(djia_m, 2, function(v){jarque.test(v)$p.value})

# Just checking pairs
pairs(as.zoo(dj_const_returns[ ,1:6]))

# Use rolling returns to aggregate data yet retain a suitable number of observations
# Calculate a 21 day moving sum of DJIA log returns
djx21 <- rollapply(djx, width = 21, FUN = sum)[-(1:20)]
djx63 <- rollapply(djx, width = 63, FUN = sum)[-(1:62)]

# Merge all three series
djx2 <- merge(djx, djx21, djx63, all = FALSE)
head(djx2)
names(djx2) <- c("Daily", "Monthly", "Quarterly")

# Plot the rolling returns
plot.zoo(djx2)

hist(coredata(djx21), breaks = 20, probability = TRUE)
lines(sort(coredata(djx21)), dnorm(sort(coredata(djx21)), mean(djx21), sd(djx21)), col = "red")

hist(djx63, breaks = 20, probability = TRUE)
lines(sort(coredata(djx63)), dnorm(sort(coredata(djx63)), mean(djx63), sd(djx63)), col = "red")

# Compute the skewness and kurtosis for each series
apply(X = djx2, MARGIN = 2, FUN = skewness)
apply(X = djx2, MARGIN = 2, FUN = kurtosis)

# Conduct the Jarque-Bera test to each series in djx2
apply(djx2, 2, jarque.test)

# Results are not normally distributed
# Student t test distribution may be better
tfit <- fit.st(ftse)
tpars <- tfit$par.ests
tpars

nu <- tpars[1]
mu <- tpars[2]
sigma <- tpars[3]

# Plot the distributions
hist(ftse, breaks = 20, main = "FTSE Returns", probability = TRUE)
lines(ftse, dnorm(ftse, mean = mean(ftse), sd = sd(ftse)), col = "red")

yvals <- dt((ftse - mu) / sigma, df = nu) / sigma
lines(ftse, yvals, col = "blue")

legend("topleft", legend = c("Normal distribution", "Student t distribution"), 
       col = c("red", "blue"), lwd = 1)

# Fit a Student's t test to the DJIA
dj <- sort(coredata(djx))
tfit <- fit.st(dj)
tpars <- tfit$par.ests

nu <- tpars[1]
mu <- tpars[2]
sigma <- tpars[3]

# Plot a histogram and superimpose t-distrubution
yvals <- dt((dj - mu) / sigma, df = nu) / sigma
hist(dj, breaks = 20, probability = TRUE, ylim = range(yvals), main = "Histogram of DJIA")

lines(dj, yvals, col = "red")

# Examine the returns of foreign exchange rates
# First gather the data
data("JPY_USD")
data("GBP_USD")
data("EUR_USD")

# Plot the daily log returns
GBP.USD <- diff(log(GBP_USD))
EUR.USD <- diff(log(EUR_USD))
USD.JPY <- diff(log(JPY_USD))

# Aggregate data and subset for the period of 2001-2015
fx_d <- cbind(GBP.USD, EUR.USD, USD.JPY)
fx_d <- fx_d["2001/2015"]

# Plot daily log-returns
plot.zoo(fx_d)

# Apply the Jarque-Bera test
apply(fx_d, 2, jarque.test)

# Aggregate monthly data
fx_m <- apply.monthly(fx_d, colSums)
head(fx_m)

# Plot monthly data
plot.zoo(fx_m, type = "h")

# Apply Jarque-Bera test to monthly data
apply(fx_m, 2, jarque.test)

# Fit a Student's t distribution to the monthly returns
apply(fx_m, 2, function(v){fit.st(v)$par.ests})

hist(sort(coredata(fx_m$USD.JPY, breaks = 20,  probability = TRUE)), main = "Distribution of JPY/USD Returns")
lines(sort(coredata(fx_m$USD.JPY)), 
                    dnorm(sort(coredata(fx_m$USD.JPY)), mean(fx_m$USD.JPY), sd(fx_m$USD.JPY)), col = "red")

# Investigatge return data of Canadian zero coupon bond yields
data("ZCB_CA")
zcb <- ZCB_CA[ , c("1.00y", "5.00y", "10.00y")]
zcbx <- diff(log(zcb))
zcbx2 <- diff(zcb)
zcbx_m <- apply.monthly(zcbx, colSums)
zcbx2_m <- apply.monthly(diff(zcb), colSums)

# Subset 2006-2015
zcbx_m <- zcbx_m["2006/2015"]
zcbx2_m <- zcbx2_m["2006/2015"]

# Plot the interest-rate return series zcbx_m and zcbx2_m
plot.zoo(zcbx_m, type = "h") 
plot.zoo(zcbx2_m, type = "h")

# Make Q-Q plots of the 3rd component series of zcbx_m and zcbx2_m
qqnorm(zcbx_m[ , 3])
qqline(zcbx_m[ , 3], col = "red")

qqnorm(zcbx2_m)
qqline(zcbx2_m, col = "blue")

# Compute the kurtosis of each series
apply(zcbx_m, 2, kurtosis)
apply(zcbx2_m, 2, kurtosis)

# Conduct the Jarque-Bera test on each series in zcbx_m and zcbx2_m
apply(zcbx_m, 2, jarque.test)
apply(zcbx2_m, 2, jarque.test)

# Investigate the volatility of the DJIA
n <- length(djx)

# Fit a normal distribution and a student t distribution
npars <- c(mu = mean(djx), sigma = sd(djx))
tfit <- fit.st(djx)
tpars <- tfit$par.ests

# Generate a normal sample of size n with parameters in npars
ndata <- rnorm(n, npars[1], npars[2])
hist(ndata, breaks = 40)

# Generate a t=distributed sample of size n with parameters in tpars
tdata <- rt(n, df = tpars[1])*tpars[3] + tpars[2]
hist(tdata, breaks = 40)

# Make ndata and tdata into xts objects
ndatax <- xts(ndata, time(djx))
tdatax <- xts(tdata, time(djx))

# Merge djx, ndatax, and tdatax and plot
alldata <- merge(djx, ndatax, tdatax)
plot.zoo(alldata, type = "h", ylim = range(alldata))

acf(djx)
acf(abs(djx))

# Plot all three data objects, djx, normal sample, and t-sample
par(mfrow = c(3, 1))

acf(djx)
acf(ndatax)
acf(tdatax)

# Plot the acfs of the absolute values
acf(abs(djx))
acf(abs(ndatax))
acf(abs(tdatax))

# Plot the acfs of the squares of the values
acf(djx^2)
acf(ndata^2)
acf(tdata^2)

# Apply the Ljung-Box test to djx
Box.test(djx, lag = 10, type = "Ljung")

# Apply the Ljung-Box test to absolute values of djx
Box.test(abs(djx), lag = 10, type = "Ljung")

# Apply the Ljung-Box test to all return series in djall
apply(dj_const_returns, 2, Box.test, lag = 10, type = "Ljung")

# Apply the Ljung-Box test to absolute values of all returns in djall
apply(abs(dj_const_returns), 2, Box.test, lag = 10, type = "Ljung")

# Create monthly log-returns from djx
djx_m <- apply.monthly(djx, sum)

# Apply Ljung-Box test
Box.test(djx_m, lag = 10, type = "Ljung")
Box.test(abs(djx_m), lag = 10, type = "Ljung")

# Apply Box test to DJIA constituent monthly returns
apply(djia_m, 2, Box.test, lag = 10, type = "Ljung")
apply(abs(djia_m), 2, Box.test, lag = 10, type = "Ljung")

# Look at the longer term log-returns of the FTSE
ftse <- diff(log(FTSE["1991/2010"]))
ftse_losses <- ftse[ftse < 0]

# Plot the volatility of the FTSE
plot(ftse_losses, type = "h")
quantile(ftse, 0.02, na.rm = TRUE)

# Take the absolute value of the largest 2% of returns (approx +-0.025)
ftse_extremes <- ftse[abs(ftse) > 0.025]
plot(abs(ftse_extremes), type = "h", auto.grid = FALSE)

# Compute the space between volitile returns to verify volatility clustering
ftse_spaces <- diff(time(ftse_extremes))
hist(as.numeric(ftse_spaces), breaks = 2000)

# More than half of extreme moves happen within one week of one another
quantile(ftse_spaces, 0.5)

# This is not a normal distribution at all
qqnorm(ftse_spaces)

## Investigate the correlation between return series
data(SMI)
range(index(SMI))

djx <- diff(log(DJ))["2005/2015"]
ftsex <- diff(log(FTSE))["2005/2015"]
smix <- diff(log(SMI))["2005/2015"]

indices <- merge(djx, ftsex, smix)
any(is.na(indices))

indices <- na.locf(indices)

# Plot time series and pairwise scatterplot
plot.zoo(indices)
pairs(as.zoo(indices))

# Calculate the correlation matrix
cor(indices)

# Plot adfs and cross-correlation functions
acf(indices)

# Plot ACFs and cross-correlations for absolute values of indices
acf(abs(indices))

# Plot volatility of fx pairs
data(CHF_USD)
CHF.USD <- diff(log(CHF_USD))

fx <- merge(GBP.USD, EUR.USD, USD.JPY, CHF.USD)
fx_w <- apply.weekly(fx, colSums)

fx <- fx["2011/2015"]
fx_w <- fx_w["2011/2015"]

plot.zoo(fx, type = "h")
plot.zoo(fx_w, type = "h")

par(mfrow = c(2, 2))
apply(fx, 2, hist, breaks = 20, probability = TRUE)

pairs(as.zoo(fx))
pairs(as.zoo(fx_w))

# Plot the acf and absolute value
acf(fx)
acf(abs(fx))

# Ljung-Box test to the components of fx and their absolute values
apply(fx, 2, Box.test, lag = 10, type = "Ljung")
apply(abs(fx), 2, Box.test, lag = 10, type = "Ljung")

# Plot the acf on weekly fx returns, and absolute values of returns
acf(fx_w)
acf(abs(fx_w))

# Apply the Ljung-Box test to the components of fx_w and their absolute values
apply(fx_w, 2, Box.test, lag = 10, type = "Ljung")
apply(abs(fx_w), 2, Box.test, lag = 10, type = "Ljung")

# Voliatility and correlation of interest rate data
zcbx <- zcbx[-1, ]

# ACF plots of zero coupon bond log_returns
dev.off()
acf(zcbx)
acf(abs(zcbx))

#Ljung-Box test to bond returns and absolute values
apply(zcbx, 2, Box.test, lag = 10, type = "Ljung")
apply(abs(zcbx), 2, Box.test, lag = 10, type = "Ljung")

hist(zcbx$`5.00y`, breaks = 40)
hist(zcbx$`10.00y`, breaks = 40)

# Plot ACF of monthly zero coupon bond log-returns and their absolute values
acf(zcbx_m)
acf(abs(zcbx_m))

# Apply Ljung_Box test to monthly zero coupon bond monthly log-returns
apply(zcbx_m, 2, Box.test, lag = 10, type = "Ljung")
apply(abs(zcbx_m), 2, Box.test, lag = 10, type = "Ljung")

par(mfrow = c(3, 1))
apply(zcbx_m, 2, hist, breaks = 40, probability = TRUE)

## Computing VaR and ES for normal distribution
mu <- mean(djx["2008/2009"])
sigma <- sd(djx["2008/2009"])

xvals <- seq(-4 * sigma, 4 * sigma, length.out = 100)

# generate a normally distributed density 
ndens <- dnorm(xvals, mean = mu, sd = sigma)

# Plot the density distribution
plot(xvals, ndens, type = "l")

# Compute the 99% VaR and 99% ES of a N(mu, sigma^2) distribution
VaR99 <- qnorm(.99, mean = mu, sd = sigma)
ES99 <- ESnorm(.99, mu = mu, sd = sigma)

# Draw vertical lines at VaR99 and ES99 in red and green
abline(v = VaR99, col = "red")
abline(v = ES99, col = "green")

## Constructing a hypothetical international equity portfolio 
# for an investor based in the U.K.
data("USD_GBP")
data("CHF_GBP")
riskfactors <- merge(FTSE, SP500, SMI, USD_GBP, CHF_GBP, all = FALSE)
head(riskfactors)
tail(riskfactors)

plot.zoo(riskfactors)

returns <- diff(log(riskfactors))[-1, ]
plot.zoo(returns)

# Compute the skewness and kurtosis for each series
apply(X = returns, MARGIN = 2, FUN = skewness)
apply(X = returns, MARGIN = 2, FUN = kurtosis)

# Conduct the Jarque-Bera test on each series
apply(returns, 2, jarque.test)

# QQ plot
qqnorm(returns[ , 5])
qqline(returns[ , 5], col = "red")

# Histogram
hist(returns[ , 5], breaks = 40)

# Box and whisker
boxplot(coredata(returns[ , 5]), horizontal = TRUE)

# Plot ACF and ACF of absolute values
acf(returns)
acf(abs(returns))

# Write a function to calculate portfolio losses and gains
# with weights 30% FTSE, 40% S&P, 30% SMI.
lossop <- function(xseries,wts=c(0.3,0.4,0.3)){
  if (is.xts(xseries))
    x <- coredata(xseries)
  else if (is.matrix(xseries))
    x <- xseries
  else
    x <- matrix(xseries,nrow=1)
  ll <- apply(x,1,function(x,wts){
    1-(wts[1]*exp(x[1]) + wts[2]*exp(x[2]+x[4]) + wts[3]*exp(x[3]+x[5]))},wts=wts)
  if (is.xts(xseries))
    ll <- xts(ll,time(xseries))
  ll
}

# Apply lossop() to returns and plot hslosses
hslosses <- lossop(returns)

# Estimate the 99th sample percentile of the distribution of hslosses
quantile(hslosses, 0.99)

# Estimate the 99% ES
mean(hslosses[hslosses >= quantile(hslosses, 0.99)])

# Estimate the mean and standard deviation of hslosses
mu <- mean(hslosses)
sigma <- sd(hslosses)

# Compute the 99% quantile of a normal distribution
qnorm(0.99, mean = mu, sd = sigma)

# Compute the 99% ES of a normal distribution
ESnorm(0.99, mu, sigma)
plot(hslosses)

# Form a Q-Q plot of hslosses against normal
qqnorm(hslosses)

# Plot the sample acf of hslosses and their absolute values
acf(hslosses)
acf(abs(hslosses))

# Estimate the 99th sample percentile for the distribution hslosses
quantile(hslosses, 0.99)

# Estimate the expected shortfall
mean(hslosses[hslosses <= quantile(hslosses, 0.99)])

# Trying a different method to find risks for currency adjusted returns of an 
# international equity portfolio for a U.K. investor.
currency.adjusted.returns <- matrix(NA, nrow(returns), ncol = 3)
currency.adjusted.returns[ , 1] <- returns$X.FTSE
currency.adjusted.returns[ , 2] <- (returns$X.GSPC + returns$USD.GBP)
currency.adjusted.returns[ , 3] <- (returns$X.SSMI + returns$CHF.GBP)
currency.adjusted.returns <- xts(currency.adjusted.returns, time(returns))
names(currency.adjusted.returns) <- c("U.K.", "U.S.", "Swiss")
head(currency.adjusted.returns)

# Calculate the weighted portfolio returns
currency.adjusted.returns$Portfolio <- rowSums(wts * currency.adjusted.returns)

# Calculate mu and sigma
mu <- mean(currency.adjusted.returns$Portfolio)
sigma <- sd(currency.adjusted.returns$Portfolio)

# Check for normal distribution
qqnorm(currency.adjusted.returns$Portfolio)
qqline(currency.adjusted.returns$Portfolio, col = "red")

hist(currency.adjusted.returns$Portfolio, breaks = 40, probability = TRUE, main = "Currency Adjusted Portfolio Returns")
lines(sort(coredata(currency.adjusted.returns$Portfolio)), dnorm(sort(coredata(currency.adjusted.returns$Portfolio)), mu, sigma), col = "red")

skewness(currency.adjusted.returns$Portfolio)
kurtosis(currency.adjusted.returns$Portfolio)

# Conduct the Jarque-Bera test on each series
apply(currency.adjusted.returns, 2, jarque.test)

# Test for serial correlation
cor(currency.adjusted.returns[1:3])

acf(currency.adjusted.returns)

acf(currency.adjusted.returns$Portfolio)
acf(abs(currency.adjusted.returns$Portfolio))

# Conduct Ljung-Box test
Box.test(currency.adjusted.returns$Portfolio, lag = 10, type = "Ljung")

# Estimate the VaR
quantile(currency.adjusted.returns$Portfolio, 0.05)
quantile(currency.adjusted.returns$Portfolio, 0.01)

#Estimate the expected shortfall
mean(currency.adjusted.returns$Portfolio[currency.adjusted.returns$Portfolio < quantile(currency.adjusted.returns$Portfolio, 0.01)])

## Black Scholes option pricing
data(VIX)
plot(VIX)

args(Black_Scholes)

# Set interest rate at 1%, volatility at 0.2, and strike price at $100
r <- 0.01
sigma <- 0.2
K <- 100

# Price calls with stock price of $80 and $120
Black_Scholes(t = 0, S = 80, r = r, sigma = sigma, K = K, T = 1, type = "call")
Black_Scholes(t = 0, S = 120, r, sigma, K, 1, "call")

# Price puts with stock price of $80 and $120
Black_Scholes(t = 0, S = 80, r, sigma, K, 1, "put")
Black_Scholes(0, 120, r, sigma, K, 1, "put")

# Implied volatility
riskfactors <- merge(SP500["1990/2010"], VIX["1990/2010"])
plot.zoo(riskfactors)


returns <- diff(log(riskfactors))[-1, ]
plot.zoo(returns, type = "h")

# Plot the returns and check for correlation
plot(as.matrix(returns))
abline(reg = lm(returns$X.VIX ~ returns$X.GSPC), col = "blue")
cor(returns)

# Test returns for normal distribution
apply(returns, 2, jarque.test)
qqnorm(returns$X.VIX)
qqline(returns$X.VIX, col = "red")

# Check returns for serial correlation
acf(returns)
acf(abs(returns))

# Set up a function to simulate losses on options using Black-Scholes
lossop <- function(xseries, r = 0.01, K=100, T = 1, sigma = 0.2, S = 100){
  if (is.xts(xseries))
    x <- coredata(xseries)
  else if (is.matrix(xseries))
    x <- xseries
  else
    x <- matrix(xseries,nrow = 1)
  ll <- apply(x, 1, function(x, r, K, T, sigma, S){
    deltat <- 1/250
    V_t0 <- Black_Scholes(0, S, r, sigma, K, T, "call")
    V_t1 = Black_Scholes(deltat, exp(log(S)+x[1]), r, exp(log(sigma)+x[2]), K, T, "call")
    - (V_t1 - V_t0)/V_t0
  },
  r= r, K = K, T = T, sigma = sigma, S = S)
  if (is.xts(xseries))
    ll <- xts(ll, time(xseries))
  ll
}

# Calculate the first loss
lossop(c(-0.1, -0.1), S = 80, sigma = 0.2)

# Calculate the second loss
lossop(c(-0.1, 0.1), S = 100, sigma = 0.2)

# Create and plot hslosses
hslosses <- lossop(returns, S = 100, sigma = 0.2)
plot(hslosses)

# Form a Q-Q plot of hslosses against normal
qqnorm(hslosses)
qqline(hslosses)

# Plot the sample acf of raw data and absolute values in hslosses
acf(hslosses)
acf(abs(hslosses))

# Estimate the 99.5% percentile of the distribution
quantile(hslosses, 0.995)

# Estimate the 99.5% ES
mean(hslosses[hslosses >= quantile(hslosses, 0.995)])

# Estimate the mean and standard deviation of hslosses
mu <- mean(hslosses)
sigma <- sd(hslosses)

# Compute the 99.5% quantile of a normal distribution
qnorm(0.995, mu, sigma)

# Compute the 99.5% ES of a normal distribution
ESnorm(0.995, mu, sigma)
