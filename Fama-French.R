library(xts)
library(PerformanceAnalytics)

# Load French-Fama three factor data

download.file("http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_TXT.zip",
              destfile = "F-F_Research_Data_Factors.zip",
              mode = "wb")

unzip("F-F_Research_Data_Factors.zip")

# Find the last row before annual data
ff.factors <- read.delim("F-F_Research_Data_Factors.txt", 
                         sep = "",
                         nrows = 1133,
                         header = TRUE,
                         skip = 3,
                         stringsAsFactors = FALSE)

head(ff.factors)
tail(ff.factors)

# Format a vector of dates
dates.1 <- as.yearmon(rownames(ff.factors), "%Y%m")
head(dates.1)
tail(dates.1)

dates.1 <- as.Date(dates.1)
head(dates.1)

# Create an xts object
ff.factors.date <- as.xts(ff.factors[ , 1:4], order.by = dates.1)
ff.factors.date <- to.monthly(ff.factors.date, OHLC = FALSE)

head(ff.factors.date)                   

# Download S&P 500 and subtract 3 month T-bill return data
library(quantmod)

SPX <- getSymbols(Symbols = "^GSPC", from = first(index(ff.factors.date)), to = last(index(ff.factors.date)),
           auto.assign = FALSE)

SPX <- Return.calculate(Ad(to.monthly(SPX)))

colnames(SPX) <- "SP500"
SPX <- SPX[-1, ]
SPX <- round(SPX, 4)
head(SPX)

#Merge data sets
ff.factors.sp500 <- merge(ff.factors.date * 0.01, SPX)

# Calculate the return premium from S&P 500 less risk-free rate
ff.factors.sp500$sp500.premium <- ff.factors.sp500$SP500 - ff.factors.sp500$RF

# Clean up
ff.factors.sp500 <- na.omit(ff.factors.sp500)
head(ff.factors.sp500)

# Regression analysis
sp500.factors <- lm(sp500.premium ~., data = ff.factors.sp500["2018/" , c(1:3, 6)])
summary(sp500.factors)

rp.estimate <- sp500.factors$coefficients[1] +
  sp500.factors$coefficients[2] * ff.factors.sp500$Mkt.RF +
  sp500.factors$coefficients[3] * ff.factors.sp500$SMB +
  sp500.factors$coefficients[4] * ff.factors.sp500$HML

# Plot expected vs. historical risk premia
plot(as.numeric(ff.factors.sp500$sp500.premium),
     as.numeric(rp.estimate),
     main = "Fama - French 3 Factor Model for S&P 500",
     ylab = "Estimated Premia",
     xlab = "Historical Premia",
     col = "red")
grid()


# Run a factor analysis on a fund of funds
returns <- na.omit(managers[ , 1:6])

# First use one factor
fit <- factanal(x = returns, factors = 1)
fit

# Check principal components
prcomp(returns)

# Summarize
ret.pc <- prcomp(returns)
print(summary(ret.pc), digits = 3)

# Plot principal components
plot(prcomp(returns), 
     main = "Principal Component Analysis",
     xlab = "Component Portfolios",
     col = "cornflowerblue")

# It looks like 3 components would be more useful
factanal(x = returns, factors = 3)

ret.pc$rotation

# Lets compare some actual funds
VX <- getSymbols("SOAVX", auto.assign = FALSE)
VX <- Return.calculate(Ad(to.monthly(VX)))[-1, ]
head(VX)

# Merge into Fama French factor data frame
ff.vx <- merge(ff.factors.date * .01, VX)
ff.vx <- na.omit(ff.vx)
ff.vx$vx.rf <- ff.vx$VX.Adjusted - ff.vx$RF

# Run the regression analysis
ff.vx.fit <- lm(vx.rf~., data = ff.vx[ , c(1:3, 6)])

summary(ff.vx.fit)

# Plot the model
vx.rp.estimate <- ff.vx.fit$coefficients[1] +
  ff.vx.fit$coefficients[2] * ff.vx$Mkt.RF +
  ff.vx.fit$coefficients[3] * ff.vx$SMB +
  ff.vx.fit$coefficients[4] * ff.vx$HML

# Plot expected vs. historical risk premia
plot(as.numeric(ff.vx$vx.rf),
     as.numeric(vx.rp.estimate),
     main = "Fama - French 3 Factor Model for SOAVX",
     ylab = "Estimated Premia",
     xlab = "Historical Premia",
     col = "red")
abline(ff.vx.fit, col = "blue")
grid()

# Let's compare the VBR Vanguard Small Cap Value ETF
vbr <- getSymbols("VBR", auto.assign = FALSE)

vbr <- Return.calculate(Ad(to.monthly(vbr)))[-1, ]
head(vbr)

# Merge with Fama French Factors
ff.vbr <- merge(ff.factors.date, vbr)
ff.vbr$vbr.rf <- ff.vbr$vbr.Adjusted - ff.vbr$RF
ff.vbr <- na.omit(ff.vbr)
head(ff.vbr)

# Model factors for vbr
ff.vbr.fit <- lm(vbr.rf ~., data = ff.vbr[ , c(1:3, 6)])
summary(ff.vbr.fit)

# Create the estimated premia from the model
vbr.rp.estimate <- ff.vbr.fit$coefficients[1] +
  ff.vbr.fit$coefficients[2] * ff.vbr$Mkt.RF +
  ff.vbr.fit$coefficients[3] * ff.vbr$SMB +
  ff.vbr.fit$coefficients[4] * ff.vbr$HML

# Plot expected vs. historical risk premia
plot(as.numeric(ff.vbr$vbr.rf),
     as.numeric(vbr.rp.estimate),
     main = "Fama - French 3 Factor Model for Vanguard Small Cap Value ETF",
     ylab = "Estimated Premia",
     xlab = "Historical Premia",
     col = "red")
abline(ff.vbr.fit, col = "blue")
grid()

# Run the analysis for the Fidelity Small Cap Value Fund
fcpvx <- getSymbols("FCPVX", auto.assign = FALSE)
fcpvx <- Return.calculate(Ad(to.monthly(fcpvx)))[-1, ]

# Merge with ff factors
ff.fcpvx <- na.omit(merge(ff.factors.date * .01, fcpvx))
ff.fcpvx$fcpvx.rf <- ff.fcpvx$fcpvx.Adjusted - ff.fcpvx$RF
head(ff.fcpvx)

# Model factors for fcpvx
ff.fcpvx.fit <- lm(fcpvx.rf ~., data = ff.fcpvx[ , c(1:3, 6)])
summary(ff.fcpvx.fit)

# Create the estimates from the model
fcpvx.rp.estimate <- ff.fcpvx.fit$coefficients[1] +
  ff.fcpvx.fit$coefficients[2] * ff.fcpvx$Mkt.RF +
  ff.fcpvx.fit$coefficients[3] * ff.fcpvx$SMB +
  ff.fcpvx.fit$coefficients[4] * ff.fcpvx$HML

# Plot expected vs. historical risk premia
plot(as.numeric(ff.fcpvx$fcpvx.rf),
     as.numeric(fcpvx.rp.estimate),
     main = "Fama - French 3 Factor Model for Fidelity Small Cap Value ETF",
     ylab = "Estimated Premia",
     xlab = "Historical Premia",
     col = "blue")
abline(ff.fcpvx.fit, col = "red")
grid()
