library(forecast)
library(fpp2)
library(astsa)
library(tidyverse)

str(uschange)
colnames(uschange)
dim(uschange)

uschange %>%
  as.data.frame() %>%
  ggplot(aes(x = Income, y = Consumption)) +
  ylab("Consumption (quarterly % change)") +
  xlab("Income (quarterly % change)") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

lm(uschange[ , "Consumption"], uschange[ ,"Income"])
tslm(Consumption ~ Income, data = uschange)
tslm(Consumption ~ Income + Unemployment, data = uschange)
tslm(Consumption ~ Income + Unemployment + Savings, data = uschange)
consumer <- tslm(Consumption ~ Income + Production + Unemployment + Savings, data = uschange)
summary(consumer)

autoplot(uschange[,'Consumption'], series="Data") +
  autolayer(fitted(consumer), series="Fitted") +
  xlab("Year") + ylab("") +
  ggtitle("Percent change in US consumption expenditure") +
  guides(colour=guide_legend(title=" "))

cbind(Data = uschange[ , "Consumption"], Fitted = fitted(consumer)) %>%
  as.data.frame() %>%
  ggplot(aes(x = Data, y = Fitted, ylab("Fitted (prediction values)"), xlab("Data(actual values)"))) +
  geom_point() +
  ggtitle("Percent change in US consumption expenditures") +
  geom_abline(intercept = 0, slope = 1)

plot(diff(fitted(consumer)))
acf2(diff(fitted(consumer)))

res <- residuals(consumer)

mean(res)
acf(res)
hist(res)
qqnorm(res)
qqline(res, col = "blue")
checkresiduals(consumer)

data_frame(Residuals = residuals(consumer), Fitted = fitted(consumer)) %>%
  ggplot(aes(Residuals, Fitted)) + geom_point()
