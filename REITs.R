library(xts)
library(quantmod)
library(PerformanceAnalytics)

# Import price data for 50+ REITs
tickers <- c("SPG", "TCO", "MAC", "PEI", "CBL", "WPG", "REG", "KIM", "FRT", "ADC", "EQR", "AVB", 
             "UDR", "ESS", "AIV", "CPT", "MAA", "ELS", "SUI", "BXP", "HPP", "KRC", "VNO", 
             "SLG", "ARE", "WELL", "VTR", "HCP", "NHI", "PSA", "EXR", "PEB", "HST", "APLE", 
             "RLJ", "DRH", "RHP", "SHO", "DLR", "COR", "CONE", "EQIX", "PLD", "DRE", "COLD", 
             "FR", "PSB", "AMT", "CCI", "SBAC", "BDN", "ESRT", "WPC", "O", "NNN")

getSymbols(tickers, from = "2014-04-28", to = "2019-05-31")

REIT <- NULL
tmp <- NULL

for(ticker in tickers){
  tmp <- Ad(to.period(eval(parse(text = ticker)), period = "months"))
  REIT <- cbind(REIT, tmp)
}

colnames(REIT) <- tickers

# Benchmark
RMZ <- getSymbols("^RMZ", from = "2014-04-28", to = "2019-05-31", auto.assign = FALSE)
RMZ <- to.period(RMZ, period = "months")
RMZ <- RMZ$RMZ.Close
RMZ_returns <- Return.calculate(RMZ)
RMZ_returns[1] <- 0

# clean data
bad <- vector(length = ncol(REIT))
for(i in 1:ncol(REIT)){
  bad[i] <- any(is.na(REIT[ , i]))
}
any(bad)
which(bad)

# Calculate log returns
R <- diff(log(REIT))

# Set month 1 returns from NA to 0
R[1, ] <- 0
colnames(R) <- tickers

# Remove stocks with incomplete data sets due to recent IPOs
R <- R[ , -which(bad)]

# Calculate average returns
mu <- colMeans(R, na.rm = TRUE)
mu_sort <- sort(mu)

# Look into best performers
barplot(mu_sort[(length(mu_sort) - 7): length(mu_sort)])
winners <- mu_sort[(length(mu_sort) - 5) : length(mu_sort)]

# Extract the data
winners_returns <- data.frame(R[ , which(rank(mu) >= (length(mu) - 5))])
exp(colSums(winners_returns)) - 1

# Inspect correlation
pairs(winners_returns)
cor(winners_returns)
library(corrplot)
corrplot.mixed(cor(winners_returns), upper = "color")

# Strongest correlation between these two. Same property type, manufactured housing, very defensive.
plot(winners_returns$ELS, winners_returns$SUI)
abline(reg = lm(winners_returns$ELS ~ winners_returns$SUI), col = "red")

plot(cumprod(R$SUI + 1))
lines(cumprod(1 + R$ELS), col = "blue")
lines(cumprod(RMZ_returns + 1), col = "red")
addLegend("topleft", legend.names = c("SUI", "ELS", "RMZ"), lwd = 1, col = c("black", "blue", "red"))

# Strong correlation between all data center stocks
plot(winners_returns$COR, winners_returns$CONE)
abline(reg = lm(winners_returns$COR ~ winners_returns$CONE), col = "red")

plot(cumprod(R$COR + 1))
lines(cumprod(R$CONE + 1), col = "blue")
lines(cumprod(RMZ_returns + 1), col = "red")
addLegend("topleft", legend.names = c("COR", "CONE", "RMZ"), lwd = 1, col = c("black", "blue", "red"))

# ADC is an outlier as it is the only outperforming retailer.
# Tenant base very defensive, tires, oil-change, paint, etc. Not losing share to internet.
plot(cumprod(1 + R$ADC), col = "green", main = "ADC vs. RMZ")
lines(cumprod(1 + RMZ_returns), col = "blue")

# Look into worst performers
barplot(mu_sort[1:8])
losers <- which(rank(mu) <= 8)
mu[losers]

loser_returns <- data.frame(R[ , losers])
tail(loser_returns)
exp(colSums(loser_returns)) - 1

# look at correlations of worst performers
pairs(loser_returns)
cor(loser_returns)

corrplot.mixed(cor(loser_returns), upper = "color")

# Two worst performers highly correlated. Both are mall property REITs. 
# Likely suffering negative investor sentiment due to rise of AMZN, retailer bankruptcies.
plot(loser_returns$TCO, loser_returns$MAC)
abline(reg = lm(loser_returns$TCO ~ loser_returns$MAC), col = "red")

plot(cumprod(R$TCO + 1), 
     ylim = range(cumprod(R$MAC + 1)),
     main = "Relative performance of TCO & MAC vs. RMZ")
lines(cumprod(R$MAC + 1), col = "blue")
lines(cumprod(RMZ_returns + 1), col = "red")
addLegend("bottomleft", legend.names = c("MAC", "TCO", "RMZ"), lwd = 1, col = c("black", "blue", "red"))


# Other two highly correlated losers are hotel REITs. Higher ratio floating/fixed. Economiclaly sensitive.
plot(loser_returns$PEB, loser_returns$RLJ)
abline(reg = lm(loser_returns$PEB ~ loser_returns$RLJ), col = "red")

plot(cumprod(R$PEB + 1), 
     ylim = range(c(cumprod(R$RLJ + 1), cumprod(R$PEB + 1))),
     main = "Relative performance of PEB & RLJ vs. RMZ")
lines(cumprod(R$RLJ + 1), col = "blue")
lines(cumprod(RMZ_returns + 1), col = "red")
addLegend("bottomleft", legend.names = c("PEB", "RLJ", "RMZ"), lwd = 1, col = c("black", "blue", "red"))

# Closer look at PEB returns. Tends to outperform RLJ for long periods, then revert.
par(mfrow = c(2, 2))
boxplot(loser_returns$PEB, horizontal = TRUE)

hist(loser_returns$PEB, breaks = 20, probability = TRUE)
lines(density(R))

acf(loser_returns[ , 1])

qqnorm(loser_returns[ , 1])
qqline(loser_returns[ , 1], col = "red")

dev.off()

# Plot correlogram of all stocks selected for potential inclusion.
# Whole group positively correlated. Individual property types strongly correlated.
corrplot.mixed(cor(R), upper = "color")

# Plot performance of stock group
# Group includes mainly larger cap names with institutional holders. 
# Equal weighted group selected outperforms broader index.
R$total <- rowMeans(R)
charts.PerformanceSummary(R$total)
charts.PerformanceSummary(RMZ_returns)
plot(cumprod(R$total + 1))
lines(cumprod(RMZ_returns + 1), col = "red")

# Stock universe returns examined
# Fat tails on the distribution of returns
total <- data.frame(R$total)
par(mfrow = c(2, 2))

boxplot(total, horizontal = TRUE)

hist(total$total, breaks = 20, probability = TRUE)
lines(density(R), col = "red")

acf(total)

qqnorm(total$total)
qqline(total, col = "red")

dev.off()

# baseline returns for available stocks, equal weight
REIT_returns <- Return.calculate(REIT)
REIT_returns[1, ] <- 0

any(is.na(REIT_returns))
REIT_returns <- REIT_returns[ , -which(bad)]

REIT_portfolio <- Return.portfolio(REIT_returns[-1, ], weights = rep((1 / ncol(REIT_returns)), ncol(REIT_returns)), rebalance_on = "months")
plot(Return.cumulative(REIT_portfolio))


# Mean-Variance portfolio optimization
library(tseries)

max_weights <- rep(0.05, ncol(R))
opt <- portfolio.optim(REIT_returns[-1, ], pm = 1.1 * mean(REIT_returns), 
                       shorts = FALSE, reshigh = max_weights)

weights <- opt$pw
names(weights) <- colnames(R)
opt_weights <- weights[weights > 0.001]
barplot(opt_weights)
opt_weights

opt$pm
opt$ps

mean(RMZ_returns)
sd(RMZ_returns)

REIT.rebal <- Return.portfolio(REIT_returns[ , opt$pw > 0.001], 
                 weights = opt_weights, rebalance_on = "months")

chart.CumReturns(REIT.rebal, col = "blue", main = "Optimized Portfolio Return")
charts.PerformanceSummary(REIT.rebal)

plot(cumprod(REIT.rebal + 1), col = "green", main = "Optimized Portfolio vs. RMZ Index")
lines(cumprod(RMZ_returns + 1), col = "red")
addLegend(legend.loc = "topleft", lwd = 1, col = c("green", "red"), legend.names = c("Opt. Portfolio", "RMZ Index"))

table.AnnualizedReturns(REIT.rebal)
charts.RollingPerformance(REIT.rebal, scale = 12)
VaR(REIT.rebal)

SemiDeviation(REIT.rebal)
SemiDeviation(RMZ_returns)
table.AnnualizedReturns(RMZ_returns)

# expected portfolio performance
# matrix calculations
R <- as.matrix(REIT_returns)
mu <- as.matrix(colMeans(REIT_returns))
sigma <- cov(REIT_returns)
w <- as.matrix(opt$pw)

#expected return
t(w) %*% mu

# expected portfolio standard deviation
sqrt(t(w) %*% sigma %*% w)

# Training period - test period test for Mean-Variance optomized portfolio

train.prt <- R["2014/2018", -5]
test.prt <- R["2019", -5]

train.prt[c(1:3, nrow(train.prt)), 1:6]
test.prt[c(1:3, nrow(test.prt)), 1:6]

max_weights <- rep(0.05, ncol(train.prt))
min_weights <- rep(0.00, ncol(train.prt))
train.prt.opt <- portfolio.optim(train.prt, pm = 2 * mean(train.prt), shorts = FALSE, reslow = min_weights,reshigh = max_weights)

train.weights <- train.prt.opt$pw
names(train.weights) <- colnames(R[ , -5])
barplot(train.weights[train.weights > 0.001])
train.weights[train.weights > 0.001]
train.weights[train.weights < 0] <- 0

# notes on pm target = portfolio mean
# Removed CBL from optimized portfolio. Likely added because it is only negatively correlated REIT
# However, company is uninvestable.
# Also, reversed weights of MAC and ELS as mall REIT cannot be max weight in this environment.
# train.weights["MAC"] <- 0.021876441
# train.weights["ELS"] <- 0.05



prt.rtn <- Return.portfolio(test.prt, weights = train.weights, rebalance_on = "quarters")

x <- xts(0, order.by = as.Date("2018-12-31"))
prt.rtn <- rbind(x, prt.rtn)

plot(cumprod(prt.rtn + 1), 
     main = "Test Port", ylim = c(1, 1.25))
lines(cumprod(RMZ_returns["2019"]  + 1), col = "red")
lines(cumprod(Return.portfolio(test.prt) + 1), col = "blue")
addLegend("topleft", lwd = 1, col = c("black", "red", "blue"), 
          legend.names = c("Test Portfolio", "RMZ", "Equal Weight All"))

charts.PerformanceSummary(prt.rtn)
StdDev(test.prt, portfolio_method = "component", weights = train.weights)

VaR(prt.rtn)

SharpeRatio.annualized(prt.rtn, Rf = 0.023, scale = 12)

# Portfolio with shorts
train.prt.opt <- portfolio.optim(train.prt, pm = 1.5 * mean(train.prt), shorts = TRUE, reslow = -max_weights, reshigh = max_weights)

train.weights <- train.prt.opt$pw
names(train.weights) <- colnames(R[ , -5])
barplot(train.weights)

train.weights

prt.rtn <- Return.portfolio(test.prt, weights = train.weights, rebalance_on = "months")

prt.rtn <- rbind(x, prt.rtn)

plot(cumprod(prt.rtn + 1), 
     main = "Test Port", ylim = c(1, 1.25))
lines(cumprod(RMZ_returns["2019"]  + 1), col = "red")
lines(cumprod(Return.portfolio(test.prt) + 1), col = "blue")
addLegend("topleft", lwd = 1, col = c("black", "red", "blue"), 
          legend.names = c("Test Portfolio", "RMZ", "Equal Weight All"))
