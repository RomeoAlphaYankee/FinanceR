# This script will do three things:

# 1. Calculate valuation of lodging REITs using a variety of methods, including
# discount/premium to net asset value per share, price/FFO, EV/EBITDA, etc..

# 2. Calculate beta and cost of equity for a stock using the CAPM, and then estimate 
# the intrinsic value using the dividend discount model.

# 3.Model industry demand and generate a forecast using high frequency economic data to
# get a feel for the environment for multiple expansion/contraction.

# While many firms will have access to much of this data in a database, this script will assume 
# that analysts' models and estimates for earnings, shares out, EBITDA, etc., are in a spreadsheet.
# This script will manipulate the spreadsheet using XLconnect, and retreive real-time prices
# using AlphaVantage. It will then write the valuations to a new spreadsheet in the workbook.

library(XLConnect)
library(quantmod)

wb <- loadWorkbook('~/DataFile/R_Files/data/lodging.xlsx')
getSheets(wb)

reits <- readWorksheet(wb, "companies")
reits

rownames(reits) <- reits$Col1
reits <- reits[ , -1]
reits

# Insert a colunm of NAs to take the updated share prices
reits$last <- rep(NA, nrow(reits))

# Write a function to acquire updated intraday prices from AlphaVantage
get_last <- function(x){
  last_price <- getSymbols(Symbols = x, src = "alphavantage", 
                           api.key = "KI9V6IBX981342IW",  
                           from = (Sys.Date() - 3),
                           to = Sys.Date(),
                           auto.assign = FALSE)
  last_price <- last(Cl(last_price))
  return(as.numeric(last_price))
}

# Use a for loop to iterate over the reits dataframe and insert updated prices.
# Note that there is a 12 second pause between calls to the get_last function because 
# AV will limit requests to five per minute, causing the table to fail to populate fully. 
for(ticker in rownames(reits)){
  reits[ticker, "last"] <- get_last(ticker)
  Sys.sleep(12)
}

# Calculate the market capitalization
reits$mkt_cap <- reits$last * reits$shares_out
reits

# Clean up the cap rate data
sum(is.na(reits$cap_rate))
mean(reits$cap_rate, na.rm = TRUE)

reits$cap_rate <- na.fill(reits$cap_rate, mean(reits$cap_rate, na.rm = TRUE))

# Calculate the market value of each REITs assets
reits$asset_value <- reits$NOI / (reits$cap_rate / 100)

# Calculate the NAV per share
reits$cash <- na.fill(reits$cash, 0)
reits$nav_ps <- (reits$asset_value - reits$debt + reits$cash - reits$pfd) / reits$shares_out

# Calculate the discount or premium to NAV
reits$nav_disc <- reits$last / reits$nav_ps

# Calculate some common relative valuation multiples such as price / ffo
reits$p_ffo <- reits$last / reits$FFO_2019

# and enterprise value to EBITDA
reits$ev_ebitda <- (reits$mkt_cap + reits$debt + reits$pfd) / reits$EBITDA

# Create a for loop to calculate the valuatio of a stock at its peers' price/ffo multiple
for(i in 1:nrow(reits)){
  reits$p_ffo_val[i] <- reits$FFO_2019[i] * mean(reits$p_ffo[-i])
}
  
# Do the same for the EV/EBITDA multiple
for(i in 1:nrow(reits)){
  reits$ev_ebitda_val[i] <- ((mean(reits$ev_ebitda[-i]) * reits$EBITDA[i]) - reits$debt[i] + reits$cash[i] - reits$pfd[i]) / reits$shares_out[i]
}

# Clean up the valuations and write to a new worksheet
valuations <- subset(reits, select = c(last, nav_ps, p_ffo_val, ev_ebitda_val))
valuations$disc_prem <- (valuations$last / rowMeans(valuations[ , 2:4])) - 1

# visualize potential relationship between discount and debt levels
debt_ebitda <- reits$debt / reits$EBITDA

plot(x = debt_ebitda, y = valuations$disc_prem, col = "blue",
     main = "Valuation vs. Leverage", 
     xlab = "Debt/EBITDA", 
     ylab = "Discount/Premium")

val_reg <- lm(valuations$disc_prem ~ debt_ebitda)

abline(reg = val_reg, col = "red")

# Look at the coefficients
val_reg

# As we can see, shares will trade at par with 2X debt / EBITDA, however the discount will
# increase 3% with each additional turn of leverage in this environment.


# Bind the rownames in a lead colunm so they appear in the new spreadsheet
valuations <- cbind(rownames(reits), valuations)

# Create a new worksheet to hold our valuations
createSheet(wb, "valuation")
writeWorksheet(wb, valuations, "valuation")
saveWorkbook(wb)


# Moving on to a dividend discunt model, we'll select a couple of names and determine the 
# intrinsic valuation.

valuations$ddm <- rep(NA, nrow(valuations))

# We'll start with APLE
# Establish a timeframe from the APLE IPO
from <- "2015-05-01"
to <- Sys.Date()

# Get stock prices, and convert to returns
aple <- getSymbols("APLE", from = from, to = to, auto.assign = FALSE)
aple <- Ad(to.monthly(aple))
aple <- Delt(aple)

# Get S&P 500 data, and convert to returns
spy <- getSymbols("SPY", from = from, to = to, auto.assign = FALSE)
spy <- Cl(to.monthly(spy))
spy <- Delt(spy)

# Get 3-month T-bill
tbill <- getSymbols("DGS3MO", src = "FRED", from = from, to = to, auto.assign = FALSE)
tbill <- Cl(to.monthly(tbill))[paste(from, to, sep = "/")]

plot(tbill$tbill.Close)

# Convert to monthly returns 
tbill$monthly <- ((1 + (tbill / 100))^(1/12) - 1)

# Merge into one data set
rets <- merge(aple, spy, tbill$monthly)[-1]
colnames(rets) <- c("aple", "spy", "treas")

charts.PerformanceSummary(rets[ , 1:2])

# Calculate equity risk premium
rets$erp <- rets$spy - rets$treas
head(rets)

# Calculate the current risk free rate and the historical equity risk premium
rfr <- (1 + last(rets$treas))^12 - 1
erp <-(1 + mean(rets$erp))^12 - 1

# Capital Asset Pricing Model
# Calculate beta using covariance / variance
cov_beta <- cov(rets)[1, 2] / diag(cov(rets))[2]

# Estimate beta using linear regression
reg <- lm(formula = aple ~ spy, data = rets)
beta <- reg$coefficients[2]

# Check beta calculations
round(cov_beta - beta, 8)

beta <- mean(c(beta, cov_beta))

# Estimate cost of equity using CAPM
CoEq <- as.numeric(rfr + beta * erp)
CoEq

# Get dividend data
dividends <- getDividends("APLE", from = from, to = to)
dividends

# Looks like monthly dividend
dividends$growth <- diff(dividends, lag = 12) / lag(dividends, 12)
names(dividends) <- c( "div", "growth")
plot.zoo(dividends, col = 1:2, lwd = 2)

# Zero growth. Nonetheless, I'll go through the paces in the event the dividend is 
# increased in the future

# Estimate future dividend growth
div_growth <- diff(dividends$div, lag = 12)[nrow(dividends)] / last(dividends$div)

# Dividend discount model
value <- (last(dividends$div) * (1 + div_growth) * 12) / (CoEq - div_growth)

# add the ddm to our valuations
valuations['APLE', 'ddm'] <- value

# Print out a summary of our findings
valuations['APLE', ]

# Calculate the payout ratio
sum(last(dividends$div, 4)) / reits['APLE', 'FFO_2019']


# Let's look at PEB as it trades at a discount to NAV

# Establish a timeframe from the PEB IPO
from <- "2009-12-31"
to <- Sys.Date()

# Get stock prices, and convert to returns
peb <- getSymbols("PEB", from = from, to = to, auto.assign = FALSE)
peb <- Ad(to.monthly(peb))
peb <- Delt(peb)

# Get S&P 500 data, and convert to returns
spy <- getSymbols("SPY", from = from, to = to, auto.assign = FALSE)
spy <- Cl(to.monthly(spy))
spy <- Delt(spy)

# Get 3-month T-bill
tbill <- getSymbols("DGS3MO", src = "FRED", from = from, to = to, auto.assign = FALSE)
tbill <- Cl(to.monthly(tbill))[paste(from, to, sep = "/")]

plot(tbill$tbill.Close)

# Convert to monthly returns 
tbill$monthly <- ((1 + (tbill / 100))^(1/12) - 1)

# Merge into one data set
rets <- merge(peb, spy, tbill$monthly)[-1]
colnames(rets) <- c("peb", "spy", "treas")

head(rets)

charts.PerformanceSummary(rets[ , 1:2])

# Calculate equity risk premium
rets$erp <- rets$spy - rets$treas
head(rets)

# Calculate the current risk free rate and the historical equity risk premium
rfr <- (1 + last(rets$treas))^12 - 1
erp <-(1 + mean(rets$erp))^12 - 1

# Capital Asset Pricing Model
# Calculate beta using covariance / variance
cov_beta <- cov(rets)[1, 2] / diag(cov(rets))[2]

# Estimate beta using linear regression
reg <- lm(formula = peb ~ spy, data = rets)
beta <- reg$coefficients[2]

# Check beta calculations
round(cov_beta - beta, 8)

beta <- mean(c(beta, cov_beta))

# Estimate cost of equity using CAPM
CoEq <- as.numeric(rfr + beta * erp)
CoEq

# Get dividend data
dividends <- getDividends("PEB", from = from, to = to)
dividends

# Looks like special dividends paid
dividends$growth <- diff(dividends, lag = 5) / lag(dividends, 5)
names(dividends) <- c( "div", "growth")
plot.zoo(dividends, col = 1:2, lwd = 2)

# Zero growth. Nonetheless, I'll go through the paces in the event the dividend is 
# increased in the future

# Estimate future dividend growth
div_growth <- diff(dividends$div, lag = 6)[nrow(dividends)] / last(dividends$div)

# Dividend discount model
value <- (last(dividends$div) * (1 + div_growth) * 4) / (CoEq - div_growth)

# Add this to our valuations
valuations['PEB', 'ddm'] <- value

# Print out a summary of our findings
valuations['PEB', ]

# Calculate the payout ratio
sum(last(dividends$div, 4)) / reits['PEB', 'FFO_2019']

# Let's look at HST
# Establish a timeframe from the HST IPO
from <- Sys.Date() - (365 * 10)
to <- Sys.Date()

# Get stock prices, and convert to returns
hst <- getSymbols("HST", from = from, to = to, auto.assign = FALSE)
hst <- Ad(to.monthly(hst))
hst <- Delt(hst)

# Get S&P 500 data, and convert to returns
spy <- getSymbols("SPY", from = from, to = to, auto.assign = FALSE)
spy <- Cl(to.monthly(spy))
spy <- Delt(spy)

# Get 3-month T-bill
tbill <- getSymbols("DGS3MO", src = "FRED", from = from, to = to, auto.assign = FALSE)
tbill <- Cl(to.monthly(tbill))[paste(from, to, sep = "/")]

plot(tbill$tbill.Close)

# Convert to monthly returns 
tbill$monthly <- ((1 + (tbill / 100))^(1/12) - 1)

# Merge into one data set
rets <- merge(hst, spy, tbill$monthly)[-1]
colnames(rets) <- c("hst", "spy", "treas")

head(rets)

charts.PerformanceSummary(rets[ , 1:2])

# Calculate equity risk premium
rets$erp <- rets$spy - rets$treas
head(rets)

# Calculate the current risk free rate and the historical equity risk premium
rfr <- (1 + last(rets$treas))^12 - 1
erp <-(1 + mean(rets$erp))^12 - 1

# Capital Asset Pricing Model
# Calculate beta using covariance / variance
cov_beta <- cov(rets)[1, 2] / diag(cov(rets))[2]

# Estimate beta using linear regression
reg <- lm(formula = hst ~ spy, data = rets)
beta <- reg$coefficients[2]

# Check beta calculations
round(cov_beta - beta, 8)

beta <- mean(c(beta, cov_beta))

# Estimate cost of equity using CAPM
CoEq <- as.numeric(rfr + beta * erp)
CoEq

# Get dividend data
dividends <- getDividends("HST", from = from, to = to)
dividends

# Looks like special dividends paid
dividends$growth <- diff(dividends, lag = 4) / lag(dividends, 4)
names(dividends) <- c( "div", "growth")
plot.zoo(dividends, col = 1:2, lwd = 2)

# Zero growth. Nonetheless, I'll go through the paces in the event the dividend is 
# increased in the future

# Estimate future dividend growth
div_growth <- diff(dividends$div, lag = 4)[nrow(dividends)] / last(dividends$div)

# Dividend discount model
value <- (last(dividends$div) * (1 + div_growth) * 4) / (CoEq - div_growth)

# Add this to our valuations
valuations['HST', 'ddm'] <- value

# Print out a summary of our findings
valuations['HST', ]

# Calculate the payout ratio
sum(last(dividends$div, 4)) / reits['HST', 'FFO_2019']

# The DDM penalizes lodging REITs for their high beta and low dividend growth. 


# Let's look at the industry demand drivers and forecast if there is going to be enough
# demand to outstrip the massive supply coming on to the market.

getSheets(wb)


drivers <-readWorksheet(wb, "drivers_ann")
drivers

stats <- readWorksheet(wb, "industry_stats")
head(stats)
stats <- stats[-ncol(stats)]

head(stats)
stats[ , 1:5] <- stats[ , 1:5] / 100

# Download some industry drivers, including payroll growth and consumer spending
from <- as.Date('1994-12-01')
from

payroll <- getSymbols("PAYEMS", from = from, src = "FRED", auto.assign = FALSE)
payroll <- to.yearly(payroll)
payroll <- Cl(payroll["1994-12/"])
payroll <- diff(log(payroll))[-1]

head(payroll)

consum <- getSymbols("PCE", src = "FRED", from = from, auto.assign = FALSE)
head(consum)

consum <- to.yearly(consum)
consum <- Cl(consum["1994-12/"])
consum <- diff(log(consum))[-1]
head(consum)

# Merge our data
# We have some time differences with the low frequency data, and some forecast years
# Trim them off to create a training set

stats_train <- xts(stats[-nrow(stats), -1], order.by = index(consum))

stats_train <- merge(stats_train, payroll)
stats_train <- merge(stats_train, consum)
head(stats_train)

colnames(stats_train) <- c(colnames(stats_train)[-c((ncol(stats_train) - 1):ncol(stats_train))], "payroll", "pce")

tail(stats_train)

stats_train <- stats_train[-((nrow(stats_train) - 1):nrow(stats_train)), ]
stats_train <- stats_train[-1, ]

# Plot and regress demand against change in non-farm payroll
plot(as.numeric(stats_train$payroll), as.numeric(stats_train$demand), 
     main = "Change in Demand Given Payrolls",
     xlab = "Change in non-farm payrolls",
     ylab = "Change in lodging demand", 
     col = "blue")
abline(reg = lm(demand ~ payroll, data = stats_train), col = "red")

# Plot and regress demand against change in PCE
plot(x = as.numeric(stats_train$pce), y = as.numeric(stats_train$demand), 
     main = "Change in Demand Given PCE",
     xlab = "Change in PCE",
     ylab = "Change in Demand",
     col = "blue")
abline(reg = lm(demand ~ pce, data = stats_train), col = "red")

# Plot and regress change in revpar against change in PCE
plot(x = as.numeric(stats_train$pce), y = as.numeric(stats_train$RevPAR_change), 
     main = "Change in RevPAR Given PCE",
     xlab = "Change in PCE",
     ylab = "Change in RevPAR",
     col = "blue")
abline(reg = lm(RevPAR_change ~ pce, data = stats_train), col = "red")

reg <- lm(formula = RevPAR_change ~ (payroll + supply + pce), data = as.data.frame(stats_train))

# Let's plug in some more recent data on payrolls, PCE, and industry supply growth
# to forecast RevPAR changes for 2019. 

reg$coefficients[1] + (reg$coefficients[2] * as.numeric(last(payroll))) + (reg$coefficients[3] * as.numeric(stats$supply[nrow(stats)])) + (reg$coefficients[3] * as.numeric(last(consum)))

# According to our model, industry estimates for RevPAR growth remain too high based upon the
# signinficant amount of new supply entering the market, and the slowdown in job growth.

# We should look for downward revenue and earnings revisions, which will probably result
# in additional multiple contraction, despite relatively low valuations.

# I would expect well managed lodging REITs to sell assets, and repurchase shares below NAV.