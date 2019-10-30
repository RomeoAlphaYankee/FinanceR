# GARCH Model using rugarch package
library(rugarch)
library(quantmod)

sp500ret <- getSymbols(Symbols = "SPY", src = "yahoo", auto.assign = FALSE)
sp500ret <- Ad(sp500ret)
sp500ret <- Delt(sp500ret)
sp500ret <- sp500ret[-1]

# Constant mean, standard garch(1, 1) model
garchspec <- ugarchspec(
  mean.model = list(armaOrder = c(0, 0)),
  variance.model = list(model = "sGARCH"),
  distribution.model = "norm")

garchfit <- ugarchfit(data = sp500ret,
                      spec = garchspec)

garchforecast <- ugarchforecast(fitORspec = garchfit, 
                                n.ahead = 5)

