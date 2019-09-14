# This code will calculate the intrinsic value of Pfizer Inc. stock (PFE) using the
# dividend discount method, and then calculalte the relative value based upon the 
# P/E and P/Book of peers. It thencalculates an implied P/B multiple based on the 
# return on equity.

# All historical stock prices are downloaded from Yahoo Finance, T-bill rates from the
# Federal Reserve, and real-time data from AlphaVantage, (API key required). 

# Note: fundamental data such as EPS and book value may have to be entered manually
# when estimates are periodically updated, or following quarterly reports. 
# Alternatively, read from a local source, or use bloomberg.

library(xts)
library(quantmod)

# Establish a ten year timeframe
from <- Sys.Date() - (252 * 10)
to <- Sys.Date()

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

plot(tbill$tbill.Close)

# Convert to monthly returns 
tbill$monthly <- ((1 + (tbill / 100))^(1/12) - 1)

# Merge into one data set
rets <- merge(pfe, spy, tbill$monthly)[-1]
colnames(rets) <- c("pfe", "spy", "treas")

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
reg <- lm(formula = pfe ~ spy, data = rets)
beta <- reg$coefficients[2]

# Check beta calculations
round(cov_beta - beta, 8)

beta <- mean(c(beta, cov_beta))

# Historic tax rate and debt/equity ratio
# Tax rate is so low that unlevered beta may not be different than actual
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
plot.zoo(dividends, col = 1:2, lwd = 2)

# Estimate future dividend growth
div_growth <- diff(dividends$div, lag = 4)[nrow(dividends)] / last(dividends$div)

# Dividend discount model
value <- (last(dividends$div) * (1 + div_growth) * 4) / (CoEq - div_growth)
paste0('Dividend Discount Valuation: $', round(as.numeric(value), 2))

# Check realtive valuation

# First create a table of peer comapanies' fundamental data, including forward EPS estimates
# and book valaue per share
peer_tickers <-  c("ABBV", "AGN", "AMGN", "BMY", "JNJ", "LLY", "MRK", "MYL", "PFE")

peers <- data.frame(last = rep(NA, length(peer_tickers)),
                    book = c(-5.79, 181.99, 17.93, 9.81, 23.01, 3.04, 10.77, 23.07, 10.71),
                    eps_ttm = c(2.74, -25.50, 12.60, 3.80, 6.02, 7.93, 3.57, 0.06, 2.16),
                    eps_ntm = c(9.49, 16.92, 15.77, 6.13, 9.12, 5.65, 5.47, 4.55, 2.81))

rownames(peers) <- peer_tickers

# Write a function to acquire updated intraday prices from AlphaVantage

# Without alphavantage api key, use prices below, updated as of September 13, 2019
# peers$last <- c(71.2300, 166.5300, 196.0250,  49.7155, 130.5800, 111.1900,  82.8300,  22.1800,  36.9700)

get_last <- function(x){
  last_price <- getSymbols(Symbols = x, src = "alphavantage", 
                           api.key = "KI9V6IBX981342IW",  
                           from = (Sys.Date() - 1),
                           to = Sys.Date(),
                           auto.assign = FALSE)
  last_price <- last(Cl(last_price))
  return(as.numeric(last_price))
}

# Use a for loop to iterate over the peers table and insert updated prices.
# Note that there is a 12 second pause between calls to the get_last function because 
# AV will limit requests to five per minute, causing the table to fail to populate fully. 
for(ticker in rownames(peers)){
  peers[ticker, "last"] <- get_last(ticker)
  Sys.sleep(12)
}

# Check results
peers

# Calculate valuation multiples
peers$p_e <- ifelse(peers$eps_ntm > 0, peers$last / peers$eps_ntm, NA)

peers$p_b <- ifelse(peers$book > 0, peers$last / peers$book, NA)

# Row index of PFE
pfe_index <- which(rownames(peers) == "PFE")

# Calculate average multiples
multiples <- data.frame(PE = mean(peers[-pfe_index, "p_e"], na.rm = TRUE), 
                        PB = mean(peers[-pfe_index, "p_b"], na.rm = TRUE))

# Calculate the implied valuation
implied_val <- multiples *  peers["PFE", c("eps_ntm", "book")]

round(implied_val, 2)

# Clearly an outlier skewing the P/B multiple. Identify and remove.
pb_out <- which(peers$p_b == max(peers$p_b, na.rm = TRUE))

# Re-calculate average multiples without outlier
multiples <- data.frame(PE = mean(peers[-pfe_index, "p_e"], na.rm = TRUE), 
                        PB = mean(peers[-c(pfe_index, pb_out), "p_b"], na.rm = TRUE))

# Calculate the implied valuation
implied_val <- multiples *  peers["PFE", c("eps_ntm", "book")]

round(implied_val, 2)


# Look at the relationship between price / book ratio and ROE
# First calculate ROE, excluding negative book value or negative earnings
peers$roe <- ifelse(peers$eps_ntm <= 0 | peers$book <= 0, NA, peers$eps_ntm / peers$book)

# Visualize the relationship
plot(x = peers$roe[-pfe_index], y = peers$p_b[-pfe_index], 
     xlab = "Return on Equity", 
     ylab = "Price-to-Book", col = "blue")

pb_reg <- lm(p_b ~ roe, data = peers[-pfe_index])
abline(reg = pb_reg, col = "red")

# Clearly there is an outlier skewing the relationship. Removing outlier and re-running
roe_out <- which(peers$roe > 1)
plot(x = peers$roe[-c(pfe_index, roe_out)], y = peers$p_b[-c(pfe_index, roe_out)], 
     xlab = "Return on Equity", 
     ylab = "Price-to-Book", 
     main = "P/B Multiple for Given ROE",
     col = "blue")

pb_reg <- lm(peers$p_b[-c(pfe_index, roe_out)] ~ peers$roe[-c(pfe_index, roe_out)])
abline(reg = pb_reg, col = "red")

# Summarize the relationship
summary(pb_reg)

# Store the intercept and slope coefficients
a <- pb_reg$coefficients[1]
b <- pb_reg$coefficients[2]

# Calculate implied P/B
implied_pb <- a + b * peers$roe[pfe_index]

implied_price <- implied_pb * peers$book[pfe_index]

implied_price

# Print the results
# Assemble a table of peer mean relative valuation
multiple_means <- data.frame(
  "Means" = round(colMeans(peers[-pfe_index, 5:6], na.rm = TRUE), 1),
  "Ex-LLY" = round(colMeans(peers[-c(pfe_index, roe_out), 5:6], na.rm = TRUE), 1),
  check.names = FALSE
)

rownames(multiple_means) <- c("P/E", "P/B")

PFE_val <- data.frame(
  Last = peers["PFE", "last"],
  DDM = round(as.numeric(value), 2),
  "P/E" = round(as.numeric(implied_val[1]), 2),
  "P/B" = round(as.numeric(implied_val[2]), 2),
  ROE = round(implied_price, 2),
  check.names = FALSE
)

rownames(PFE_val) <- "Valuation:"


peers
multiple_means
PFE_val
rowMeans(PFE_val)