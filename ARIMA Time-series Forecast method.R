setwd("C:/Users/Benson/Desktop")
require(ggplot2)
require(forecast)
require(tseries)
daily_data = read.csv("day.csv",stringsAsFactors = FALSE)

#Basic Look
daily_data$Date = as.Date(daily_data$dteday)
ggplot(daily_data,aes(Date,cnt)) + geom_line()


#cleaning data
## make data as time-series using 'ts()' first, then use 'tsclean()' to get rid of odd data (average out the outliers)'
count_ts = ts(daily_data[ , c('cnt')])
daily_data$clean_cnt = tsclean(count_ts)
ggplot() + geom_line(data = daily_data, aes(x=Date, y= clean_cnt))


#Smoothing the series -> make it more stable, predictable
## More order -> Smoother curve
daily_data$cnt_ma = ma(daily_data$clean_cnt, order=7) # using the clean count with no outliers
daily_data$cnt_ma30 = ma(daily_data$clean_cnt, order=30) 

##To see how it goes
ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_cnt, colour = "Counts")) +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma30, colour = "Monthly Moving Average"))  +
  ylab('Bicycle Count')

  
  
# ARIMA
  
## Seasonal data need more complicated model structure, picking right parameters, so this time we use non-seasonal one

## Calulating seasonal component of data using "stl()"

#Step 3: Decompose Your Data
count_ma = ts(na.omit(daily_data$cnt_ma),frequency = 30) #Allowing 30 obervations per month because we are looking at daily data
decomp = stl(count_ma, s.window = "periodic") 
#or decomp = stlm(count_ma, s.window = "periodic",allow.multiplicative.trend = TRUE)
#or dec
deseasonal_cnt <- seasadj(decomp) #subtracting seasonal component from origianl series
plot(decomp)


#Step 4: Stationarity
##Augmented Dickey-Fuller Test
## Formal statistical test 
## ADF procedure tests whether the change in Y can be explained by lagged value and a linear trend
adf.test(count_ma, alternative = "stationary")

#If p-value is less than 0.05, our dataset is stationary.


#Step 5: Autocorrelations(ACF) and Choosing Model Order
# ACF is useful visual tool in determining whether a series is stationary
Acf(count_ma, main='')
Pacf(count_ma, main='')

#see above ACF and the number spike -> difereneces
count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")
Acf(count_d1, main='')
Pacf(count_d1, main='')

## This is the most important thing!!!
#Step 6: Fitting an ARIMA model
auto.arima(deseasonal_cnt, seasonal=FALSE)
#AR(1) coefficient p = 0.551 tells us that the next value in the series is taken 
#as a dampened previous value by a factor of 0.55 and depends on previous error lag.


#Step 7: Evaluate and Iterate
fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals')
# There is a clear pattern present in ACF/PACF and model residuals plots repeating at lag 7. 
# This suggests that our model may be better off with a different specification, such as p = 7 or q = 7. 


fit2 = arima(deseasonal_cnt, order=c(1,1,7))
fit2

tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')

## Now forecasting!!!
fcast <- forecast(fit2, h=30) #h periods ahead for predictions to be made
plot(fcast)


#improve the forecast and iterate on this model
fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
fit_w_seasonality

seas_fcast <- forecast(fit_w_seasonality, h=30)
plot(seas_fcast)
