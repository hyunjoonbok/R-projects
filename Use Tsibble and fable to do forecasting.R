# Use Tsibble and fable to do forecasting 

# 1. Asutrailan eating-out expenditure forecast
require(tsibble)
require(ggplot2)
require(tidyr)
require(fable)
require(fpp2)

# start making tibble
cafe <-as_tsibble(auscafe)
# we could see that it's designed for multiple series
cafe %>% ARIMA(log(value) ~ pdq(2,1,1) + PDQ(2,1,2))

cafe %>% ARIMA(log(value) ~ pdq(2,1,1) + PDQ(2,1,2)) %>% summary()

# pass it into forecast
cafe %>% ARIMA(log(value) ~ pdq(2,1,1) + PDQ(2,1,2)) %>% forecast()

# pass above to summary
cafe %>% ARIMA(log(value) ~ pdq(2,1,1) + PDQ(2,1,2)) %>% forecast() %>% summary()
cafe %>% ARIMA(log(value) ~ pdq(2,1,1) + PDQ(2,1,2)) %>% forecast() %>% autoplot()


# 2. Half-hourly electrictiy demand
elecdemandd <- as_tsibble(elecdemand) %>% spread(key, value)
fit2 <- ARIMA(elecdemandd, Demand ~ Temperature + I(Temperature^2) + WorkDay)

summary(fit2)

forecast(fit2, newdata = elecdemandfuture) %>% autoplot()