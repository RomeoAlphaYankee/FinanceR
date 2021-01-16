library(quantmod)
library(Quandl)
library(readr)
# Using different methods to download commodities price data
getSymbols(Symbols = "CHRIS / CME_HG11", source = "Quandl")

# gold
getMetals(Metals = "XAU", from = Sys.Date() - 252)

XAUUSD
plot(XAUUSD, main = "Gold")

# copper

# getSymbols(symbols = "HG=F")

# copper <-getSymbols(Symbols = "PCOPPUSDM", src = "FRED", auto.assign = FALSE)

# Just read in a flat file
copper <- read_csv("C:/Users/angel/Downloads/HG=F.csv", col_types = c("Dnnnnnn"))

head(copper)

#Remove "Null" rows
i <- !is.na(copper[ , 2])

copper <- copper[i, ]

# Convert to xts
copper <- as.xts(copper[ , -1], order.by = copper$Date)

copper <- to.monthly(copper)

chartSeries(copper, theme = "white")

# Calculate changes
copper$returns <- diff(log(Cl(copper)))

# Get Dow Jones Industrial Average
djia <- getSymbols("^DJI", auto.assign = FALSE)
djia <- to.monthly(djia)
djia <- Return.calculate(Ad(djia))

# Let's have a look at Doctor Copper
# Compare to DJI during financial crisis
plot(copper$returns["2008/2010"])
lines(diff(log(Ad(to.monthly(djia)))), col = "red")

# Chart copper this year
chartSeries(copper["2020/"])

# Plot copper vs DJI from 2018 to today
plot(copper$returns["2018/"], main = "Change in Price of Copper")
lines(diff(log(Ad(to.monthly(djia)))), col = "red")
