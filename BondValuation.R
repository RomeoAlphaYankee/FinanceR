# Bond Valuation 

# create a vector of cash flows
cf <- c(5, 5, 5, 5, 105)

# convert to data frame
cf <- data.frame(cf)
class(cf)

# add a time index
cf$t <- as.numeric(rownames(cf))

# create discount factor
r <- 0.06
cf$pv_factor <- 1 / (1 + r)^cf$t

#calculate pv
cf$pv <- cf$cf * cf$pv_factor
sum(cf$pv)

# using above I'll create function for bond valuation 
# where p = par value, r = coupon rate, tm = time until maturity
# cf = cash flows, yld = current yield, pv = present value

bond_val <- function(p, r, tm, yld){
  cf <- c(rep((p * r), tm - 1),  (p + (p * r)))
  cf <- data.frame(cf)
  cf$t <- as.numeric(rownames(cf))
  cf$pv_factor <- 1 / (1 + yld)^cf$t
  cf$pv <- cf$cf * cf$pv_factor
  sum(cf$pv)
}

bond_val(p = 100, r = 0.05, tm = 5, yld = 0.06)

# Acquire some current bond yield data

library(Quandl)
Baa <- Quandl("FED/RIMLPBAAR_N_M")
BBB <- Quandl("ML/BBBEY")
BBB <- as.xts(BBB[ , 2], order.by = BBB[ , 1])
AAA <- Quandl("USTREASURY/HQMYC")

# Examine the AAA bond yield curve
yield2 <- AAA[1, -1] / 100

yield_curve <- data.frame(months = 1:length(yield2) * 6)

for(i in 1:length(yield2)){
  yield_curve$yield[i] <- yield2[1, i]
}

plot(yield_curve, type = "l", col = "red", main = "AAA Bond Yield Curve")

# Examine BBB convexity

prc_yield <- seq(0.01, 0.25, 0.00125)
prc_yield <- data.frame(yield = prc_yield)

yield <- BBB_yield$BAMLC0A4CBBBEY[1] / 100
bond_val(p = 100, r = 0.05, tm = 10, yld = yield)

for(i in 1:length(prc_yield$yield)){
  prc_yield$price[i] <- bond_val(p = 100, r = yield, tm = 10, yld = prc_yield$yield[i])
}

plot(prc_yield, type = "l", col = "4", main = "Price/YTM Relationship", lwd = 2)
abline(h = 100, col = 2)
axis(side = 4)

# Create a function to look up prices 1/8 of a point on either side of a ytm quote

price_lookup <- function(yld){
  prc_yield[(which(prc_yield$yield == yld) - 1): (which(prc_yield$yield == yld) + 1), ]
}

price_lookup(ytm = .04125)

# Examine historic treasury rates
library(quantmod)
t10yr <- getSymbols("DGS10", src = "FRED", auto.assign = FALSE)
t10yr <- t10yr["2007/2019"]
plot(x = index(t10yr), y = t10yr$DGS10, type = "l", col = 3 , main = "10-Year US Treasury Yields", xlab = "Date", ylab = "Yield %")

# spread analysis
BBB.xts <- as.xts(BBB[ , -1], order.by = BBB[ , 1])
spreads <- merge.xts(t10yr, BBB.xts)
spreads <- spreads["2007/2019"]
names(spreads) <- c("T10", "BBB")
spreads$Spread <- (spreads$BBB - spreads$T10) * 100
plot(x = index(spreads), y = spreads$Spread, col = 4, main = "BBB - US 10-Year Treasury Spread", type = "l", xlab = "Date", ylab = "Spread (bps)")

# find yield to maturity given a bond price and coupon
# generate cashflow vector given a bond's price, coupon, time until maturity
cashflow <- function(prc, coup, par = 100, t){
  c(-prc, rep(coup * par, t - 1), ((coup * par) +par))
}

cf <- cashflow(prc = 99, coup = .05, t = 10)

# Create bond valuation function
bval <- function(i, cf,
                 t=seq(along = cf))
  sum(cf / (1 + i)^t)

# Create yield to maturity function using uniroot
ytm <- function(cf) {
  uniroot(bval, c(0, 1), cf = cf)$root
}

ytm(cf)

# estimate duration on a bond
