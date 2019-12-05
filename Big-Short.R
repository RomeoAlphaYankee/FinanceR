# Attempt to recreate the conditions surrounding the credit crisis, where a small change in the underlying
# assumptions led to a big change in the distribution of returns.

#sample loan defaults
n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample(c(0, 1), n, prob = c(1 - p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)

#monte carlo simulation
B <- 10000
losses <- replicate(B, {
  defaults <- sample(c(0, 1), n, prob = c(1 - p, p), replace = TRUE)
  sum(defaults * loss_per_foreclosure)
})

#plot distribution
data.frame(losses_in_millions = losses/10^6) %>% ggplot(aes(losses_in_millions)) + 
  geom_histogram(binwidth = 0.6, col = "black")

#calculation using CLT
ES <- n * (p * loss_per_foreclosure + (1-p) * 0)
SES <- sqrt(n) * abs(loss_per_foreclosure) * sqrt(p * (1 - p))

#break even expected value calculation
# loss_per_foreclosure * p + x * (1 - p)
# solving for x using algebra results in
x = -loss_per_foreclosure * p / (1 - p)

# To get a one percent loss rate i.e. S < 0 = 0.01
# subtract the expected value of S from both sides of the less-than symbol,
# then divide both sides by standard error of S
# (S - ES)/SES < -ES/SES
# the left side becomes a standard random variable with expected value of 0 and sd of 1 
# which are the default values of the qnorm function, we will call Z
# the right side can be replaced with known values and must be equal to qnorm(0.01)
l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l * (n * p - z * sqrt(n * p * (1 - p))) / (n * (1 - p) + z * sqrt(n * p * (1 - p)))
x #amount that must be charged
x / 180000 #interest rate
loss_per_foreclosure * p + x * (1 - p) # is the expected value per loan
n * (loss_per_foreclosure * p + x * (1 - p))
abs(x - loss_per_foreclosure) * sqrt(p * (1-p))

#to check this with a monte carlo simulation
B <- 10000
profit <- replicate(B, {
  loans <- sample(c(x, loss_per_foreclosure), n, prob = c((1 - p), p), replace = TRUE)
  sum(loans)
})
mean(profit)
mean(profit < 0)
pnorm(0, mean(profit), sd(profit))

#if an economic shift changes default rates up or down 1%
n <- 22163
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
  new_p <- p + sample(seq(0.01, -0.01, length = 100), 1)
  draws <- sample(c(x, loss_per_foreclosure), n, replace = TRUE, prob = c(1 - new_p, new_p))
  sum(draws)
})
mean(profit)
mean(profit < 0)
mean(profit < -1000000)
data.frame(profit_in_millions = profit / 10^6) %>% ggplot(aes(profit_in_millions)) + 
  geom_histogram(color = "black", binwidth = 5)
