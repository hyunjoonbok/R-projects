##==================================================================================================##
##SARIMA
require(forecast)
arima1 = auto.arima(aa$GrossCollection, seasonal= TRUE, trace = TRUE, test = "kpss", ic= "bic")
summary(arima1)
confint(arima1)

# Residaul Diagonistics
## SHouldn't be violated!
plot.ts(arima1$residuals)
Box.test(arima1$residuals)
acf(arima1$residuals)
require(tseries)
jarque.bera.test(arima1$residuals) # Residuals are normally distributed test


## Forecast
arima1_forecast = forecast(arima1, h=3);arima1_forecast
plot(arima1_forecast)
autoplot(arima1_forecast)


require(TSPred)
plotarimapred(aa$GrossCollection,arima1, xlim = c(2017,11))
###

## Lets use 
arima2 = auto.arima(log(aa$GrossCollection), seasonal= TRUE, trace = TRUE, test = "kpss", ic= "bic")
confint(arima2)
## Repeat all above Residual Diagnostics
arima2_forecast = forecast(arima2, h=3);arima2_forecast
autoplot(arima2_forecast)
