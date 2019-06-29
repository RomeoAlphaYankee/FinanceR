library(forecast)
library(dplyr)

# Forecast the annual international visitors to Australia
head(austa)

# Set up forecast functions for ETS and ARIMA models
fets <- function(x, h) {
  forecast(ets(x), h = h)
}
farima <- function(x, h) {
  forecast(auto.arima(x), h = h)
}

# Compute cross-validation errors for ETS
e1 <- tsCV(austa, fets, h = 1)

# Compute cross-validation errors for ARIMA
e2 <- tsCV(austa, farima, h = 1)

# Find mean squared error of each model class
mean(e1^2, na.rm = TRUE)
mean(e2^2, na.rm = TRUE)

# Check residuals of preferred model
aa <- auto.arima(austa)
checkresiduals(aa)

summary(aa)
forecast(aa)

# Plot 10-year forecasts using the best model class
austa %>% farima(h = 10) %>% autoplot()

# Forecast the future use of debit cards in Iceland
head(debitcards)

# Calculate the cross-validation errors of ETS and ARIMA models
d1 <- tsCV(debitcards, fets, h = 1)
d2 <- tsCV(debitcards, farima, h = 1)

# Find mean squared error of each model class
mean(d1^2, na.rm = TRUE)
mean(d2^2, na.rm = TRUE)

# Check residuals
dets <- ets(debitcards)
checkresiduals(dets)
summary(dets)
forecast(dets)

# That wasn't a very good fit. So, let's check both
darima <- auto.arima(debitcards)
checkresiduals(darima)
summary(darima)

# Surprisingly, auto.arima didn't select d = 1

# Apply Box-Cox transformation
lambda <- BoxCox.lambda(debitcards)
debit2 <- BoxCox(debitcards, lambda)
plot(debit2)

# Retry time series cross validation using transformed data
d3 <- tsCV(debit2, fets, h = 1)
d4 <- tsCV(debit2, farima, h = 1)

# Find mean squared error of each model class
mean(d3^2, na.rm = TRUE)
mean(d4^2, na.rm = TRUE)

debit_ets <- ets(debitcards, lambda = lambda)
checkresiduals(debit_ets)
summary(debit_ets)

debit_arima <- auto.arima(debitcards, lambda = lambda)
checkresiduals(debit_arima)
summary(debit_arima)

# double checking arima with stepwise set to FALSE
debit_arima2 <- auto.arima(debitcards, lambda = lambda, stepwise = FALSE)
checkresiduals(debit_arima2)

# Creating a training set and test set
train <- window(debitcards, start = c(2000, 1), end = c(2011, 8))
lambda2 <- BoxCox.lambda(train)
debit_ets2 <- ets(train, lambda = lambda2)
debit_arima3 <- auto.arima(train, lambda = lambda2)
debit_arima4 <- auto.arima(train, lambda = lambda2, stepwise = FALSE)

checkresiduals(debit_ets2)
checkresiduals(debit_arima3)
checkresiduals(debit_arima4)

fc1 <- forecast(debit_ets2, h = 24)
fc2 <- forecast(debit_arima3, h = 24)
fc3 <- forecast(debit_arima4, h = 24)

accuracy(fc1, debitcards)
accuracy(fc2, debitcards)
accuracy(fc3, debitcards)

# It looks conclusive that the ETS model is better here
autoplot(fc1) +autolayer(debitcards)
autoplot(fc2) + autolayer(debitcards)
autoplot(fc3) + autolayer(debitcards)

# Plot 2-year forecasts using the best model class based on RMSE and AICc
debit_ets %>% forecast(h = 24) %>% autoplot()
forecast(debit_ets)
