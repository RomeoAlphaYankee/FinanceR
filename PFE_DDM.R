# Capital Asset Pricing Model
library(quantmod)
library(xts)

# Establish a timeframe
from <- "2014-01-01"
to <- "2019-08-31"

# Get stock prices, and convert to returns
pfe <- getSymbols("PFE", from = from, to = to, auto.assign = FALSE)
pfe <- Ad(to.monthly(pfe))
pfe <- Delt(pfe)

# Get S&P 500 data, and convert to returns
spy <- getSymbols("SPY", from = from, to = to, auto.assign = FALSE)
spy <- Cl(to.monthly(spy))
spy <- Delt(spy)

# Get 3-month T-bill
tbill <- getSymbols("DGS3MO", src = "FRED", from = from, to = to, auto.assign = FALSE)
tbill <- Cl(to.monthly(tbill))[paste(from, to, sep = "/")]

plot.zoo(tbill)

# Convert to monthly returns 
tbill$monthly <- ((1 + (tbill / 100))^(1/12) - 1)

# Merge into one data set
rets <- merge(pfe, spy, tbill$monthly)[-1]
colnames(rets) <- c("pfe", "spy", "treas")
rets$erp <- rets$spy - rets$treas

head(rets)

# Calculate the current risk free rate and the historical equity risk premium
rfr <- (1 + last(rets$treas))^12 - 1
erp <-(1 + mean(rets$erp))^12 - 1

# Calculate beta using covariance / variance
cov_beta <- cov(rets)[1, 2] / diag(cov(rets))[2]

# Estimate beta using linear regression
reg <- lm(formula = pfe ~ spy, data = rets)
beta <- reg$coefficients[2]

# Check beta calculations
round(cov_beta - beta, 8)

tax <- 0.059
DEq <- 0.90

unlevered_beta <- beta / (1 + (1 - tax) * DEq)

target_beta <- unlevered_beta * (1 + (1 - tax) * 0.82)
target_beta

# Estimate cost of equity using CAPM
CoEq <- as.numeric(rfr + beta * erp)
CoEq

# Get dividend data
dividends <- getDividends("PFE", from = from, to = to)
dividends$growth <- diff(dividends, lag = 4) / lag(dividends, 4)
names(dividends) <- c( "div", "growth")
tail(dividends)

# Estimate dividend growth
div_growth <- diff(dividends$div, lag = 4)[nrow(dividends)] / last(dividends$div)

# Dividend discount model
value <- (last(dividends$div) * 4) / (CoEq - div_growth)
paste0('Present Value: $', round(as.numeric(value), 2))
