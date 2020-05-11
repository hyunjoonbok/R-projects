library(dataiku)
library(forecast)
library(dplyr)

ts_passengers = ts(intl_passengers$International_Passengers,
                   start=1949,
                   frequency=12)
plot(ts_passengers)

##Choosing a forecasting model

#Model 1: Exponential State Smoothing

m_ets = ets(ts_passengers)
f_ets = forecast(m_ets, h=24) # forecast 24 months into the future
plot(f_ets)

#Model 2: ARIMA
m_aa = auto.arima(ts_passengers)
f_aa = forecast(m_aa, h=24)
plot(f_aa)

#Model 3: TBATS
##designed for use when there are multiple cyclic patterns
m_tbats = tbats(ts_passengers)
f_tbats = forecast(m_tbats, h=24)
plot(f_tbats)

#Model comparison
#use AIC to compare the different models.
barplot(c(ETS=m_ets$aic, ARIMA=m_aa$aic, TBATS=m_tbats$AIC),ylab="AIC")

last_date = index(ts_passengers)[length(ts_passengers)]
forecast_df = data.frame(passengers_predicted=f_aa$mean,
                         passengers_lower=f_aa$lower[,2],
                         passengers_upper=f_aa$upper[,2],
                         date=last_date + seq(1/12, 2, by=1/12))
forecast_df = forecast_df %>%
  mutate(year=floor(date)) %>%
  mutate(month=round(((date %% 1) * 12) + 1))

write.csv(forecast_df,"forecast")