# CIO analysis and valuation

library(quantmod)
library(PerformanceAnalytics)
library(dplyr)
library(ggplot2)
library(ggthemes) # to get the WSJ and Economist themes

# Collect price data from IPO date
ipo = "2014-04-15"
getSymbols(Symbols = "CIO", from = ipo)

head(CIO)

chartSeries(CIO)

# Select adjusted, and calculate returns
CIO <- Ad(CIO)
CIO$return <- Return.calculate(CIO)
names(CIO) <- c("Price", "Return")

# Check returns
plot(CIO$Return)
hist(CIO$Return, breaks = 30)

# Check Value at Risk
VaR(CIO$Return)

# Worst return was 2019-02-27 when Q4 2018 results were reported
CIO$Return[CIO$Return == min(CIO$Return[-1])]

# Plot stock price
CIO %>% ggplot(aes(x = index(CIO), y = Price)) +
  geom_line(color = "red") +
  theme_wsj() +
  ggtitle("City Office REIT (CIO)")

# Dividend Discount Model
# Get stock prices, and convert to monthly returns
cio.ddm <- getSymbols("CIO", from = ipo, auto.assign = FALSE)
cio.ddm <- Ad(to.monthly(cio.ddm))
cio.ddm <- Delt(cio.ddm)

# Get S&P 500 data, and convert to returns
spy <- getSymbols("SPY", from = ipo, auto.assign = FALSE)
spy <- Cl(to.monthly(spy))
spy <- Delt(spy)

# Get 3-month T-bill
tbill <- getSymbols("DGS3MO", src = "FRED", from = ipo, auto.assign = FALSE)

head(tbill)

head(tbill[paste(as.character(ipo), "/")])

tbill <- Cl(to.monthly(tbill))[paste(as.character(ipo), "/") ]

ggplot(tbill, aes(x = index(tbill), y = tbill.Close)) +
  geom_line() +
  theme_wsj() +
  ggtitle("3 Mo. Treasury Yield")

# Convert to monthly returns 
tbill$monthly <- ((1 + (tbill / 100))^(1/12) - 1)

# Merge into one data set
cio.ddm <- merge(cio.ddm, spy, tbill$monthly)[-1]
names(cio.ddm) <- c("CIO", "SPX", "Treas")

# Check performance vs. S&P
charts.PerformanceSummary(cio.ddm[2:nrow(cio.ddm) , 1:2])

# Calculate equity risk premium
cio.ddm$erp <- cio.ddm$SPX - cio.ddm$Treas

tail(cio.ddm)

# Calculate the current risk free rate and the historical equity risk premium
rfr <- (1 + last(cio.ddm$Treas))^12 - 1
erp <-(1 + mean(cio.ddm$erp))^12 - 1

# Calculate beta using covariance / variance
cov_beta <- cov(cio.ddm)[1, 2] / diag(cov(cio.ddm))[2]

# Estimate beta using linear regression
reg <- lm(formula = CIO ~ SPX, data = cio.ddm)
beta <- reg$coefficients[2]

# Check beta calculations
round(cov_beta - beta, 8)

# Tax and D/E ratio
tax <- 0.0
DEq <- 651693 / (112000 + 476 + 483200 + 247)

unlevered_beta <- beta / (1 + (1 - tax) * DEq)

# target_beta <- unlevered_beta * (1 + (1 - tax) * 0.82)
# target_beta

# Estimate cost of equity using CAPM
CoEq <- as.numeric(rfr + beta * erp)
CoEq

# Get dividend data
dividends <- getDividends("CIO", from = ipo)
dividends$growth <- diff(dividends, lag = 4) / lag(dividends, 4)
names(dividends) <- c( "div", "growth")
plot.zoo(dividends, col = 1:2, lwd = 2)

# Estimate dividend growth (if any)
div_growth <- diff(dividends$div, lag = 4)[nrow(dividends)] / last(dividends$div)

# Dividend discount model
value <- (last(dividends$div) * 4) / (CoEq - div_growth)
paste0('Dividend Discount Valuation: $', round(as.numeric(value), 2))


library(XLConnect)

# load spreadsheet with updated estimates
wb <- loadWorkbook('~/DataFile/R_Files/data/reits.xlsx')

# list the names of workbook sheets
getSheets(wb)

# read the sheet into R
reits <- readWorksheet(wb, "office")
reits

# Rename rows
rownames(reits) <- reits$ticker
reits <- reits[ , -1]
reits

# Filter for complete datasets
reits <- reits[complete.cases(reits), ]
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
reits$nav_ps <- (reits$asset_value - reits$debt + reits$cash - reits$pfd) / (reits$shares_out * 1000)

# Calculate the discount or premium to NAV
reits$nav_disc <- reits$last / reits$nav_ps

median(reits$nav_disc[-nrow(reits)])

# Calculate some common relative valuation multiples such as price / ffo
reits$p_ffo <- reits$last / reits$FFO

mean(reits$p_ffo[-length(reits$p_ffo)])

reits['CIO', 'FFO'] * mean(reits$p_ffo[-length(reits$p_ffo)])

# and enterprise value to EBITDA
reits$ev_ebitda <- (reits$mkt_cap + reits$debt + reits$pfd) / (reits$EBITDA * 1000)
mean(reits$ev_ebitda[-nrow(reits)])

# Create a for loop to calculate the valuatio of a stock at its peers' price/ffo multiple
for(i in 1:nrow(reits)){
  reits$p_ffo_val[i] <- reits$FFO[i] * mean(reits$p_ffo[-i])
}

# Do the same for the EV/EBITDA multiple
# Something wrong here

(mean(reits$ev_ebitda[-nrow(reits)]) * reits$EBITDA[nrow(reits)] * 1000 + reits$cash[nrow(reits)] - reits$debt[nrow(reits)]) / (reits$shares_out[nrow(reits)] * 1000)


for(i in 1:nrow(reits)){
  reits$ev_ebitda_val[i] <- ((mean(reits$ev_ebitda[-i]) * (reits$EBITDA[i] * 1000)) - reits$debt[i] + reits$cash[i] - reits$pfd[i]) / reits$shares_out[i]
}

# Clean up the valuations and write to a new worksheet
valuations <- subset(reits, select = c(last, nav_ps, p_ffo_val, ev_ebitda_val))

valuations$disc_prem <- (valuations$last / rowMeans(valuations[ , 2:4])) - 1

valuations

# visualize potential relationship between discount and debt levels
debt_ebitda <- reits$debt / (reits$EBITDA * 1000)

plot(x = debt_ebitda, y = valuations$disc_prem, col = "blue",
     main = "Valuation vs. Leverage", 
     xlab = "Debt/EBITDA", 
     ylab = "Discount/Premium")

val_reg <- lm(valuations$disc_prem ~ debt_ebitda)

abline(reg = val_reg, col = "red")

# Look at the coefficients
val_reg