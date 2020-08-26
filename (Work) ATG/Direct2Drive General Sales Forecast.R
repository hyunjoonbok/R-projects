##====Direct2Drive General Sales Forecast====##
require(ggplot2)
require(forecast)
require(tseries)
require(xlsx)
setwd("C:/Users/Benson/Desktop/US Raw transaction data (Nov.2014 - Nov.2017)")


##====Reading Excel files====##
US_2014_December <- read.xlsx("US_2014_December.xlsx",1)
US_2014_November <- read.xlsx("US_2014_November.xlsx",1)

# US_2015_January <- read.xlsx("US_2015_January.xlsx",1) # Disregard due to unexpectancy
US_2015_February <- read.xlsx("US_2015_February.xlsx",1)
US_2015_March <-read.xlsx("US_2015_March.xlsx",1)
US_2015_April <-read.xlsx("US_2015_April.xlsx",1)
US_2015_May <-read.xlsx("US_2015_May.xlsx",1)
US_2015_June <-read.xlsx("US_2015_June.xlsx",1)
US_2015_July <-read.xlsx("US_2015_July.xlsx",1)
US_2015_August <-read.xlsx("US_2015_August.xlsx",1)
US_2015_September <-read.xlsx("US_2015_September.xlsx",1)
US_2015_October <-read.xlsx("US_2015_October.xlsx",1)
US_2015_November <-read.xlsx("US_2015_November.xlsx",1)
US_2015_December <-read.xlsx("US_2015_December.xlsx",1)

US_2016_January <- read.xlsx("US_2016_January.xlsx",1)
US_2016_February <- read.xlsx("US_2016_February.xlsx",1)
US_2016_March <-read.xlsx("US_2016_March.xlsx",1)
US_2016_April <-read.xlsx("US_2016_April.xlsx",1)
US_2016_May <-read.xlsx("US_2016_May.xlsx",1)
US_2016_June <-read.xlsx("US_2016_June.xlsx",1)
US_2016_July <-read.xlsx("US_2016_July.xlsx",1)
US_2016_August <-read.xlsx("US_2016_August.xlsx",1)
US_2016_September <-read.xlsx("US_2016_September.xlsx",1)
US_2016_October <-read.xlsx("US_2016_October.xlsx",1)
US_2016_November <-read.xlsx("US_2016_November.xlsx",1)
US_2016_December <-read.xlsx("US_2016_December.xlsx",1)

US_2017_January <- read.xlsx("US_2017_January.xlsx",1)
US_2017_February <- read.xlsx("US_2017_February.xlsx",1)
US_2017_March <-read.xlsx("US_2017_March.xlsx",1)
US_2017_April <-read.xlsx("US_2017_April.xlsx",1)
US_2017_May <-read.xlsx("US_2017_May.xlsx",1)
US_2017_June <-read.xlsx("US_2017_June.xlsx",1)
US_2017_July <-read.xlsx("US_2017_July.xlsx",1)
US_2017_August <-read.xlsx("US_2017_August.xlsx",1)
US_2017_September <-read.xlsx("US_2017_September.xlsx",1)
US_2017_October <-read.xlsx("US_2017_October.xlsx",1)
US_2017_November <-read.xlsx("US_2017_November.xlsx",1)
US_2017_December <-read.xlsx("US_2017_December.xlsx",1)


##====Formatting the dataset====##
Total= data.frame(sum(US_2014_November$GrossCollection), sum(US_2014_December$GrossCollection),10536.55,sum(US_2015_February$GrossCollection),
              sum(US_2015_March$GrossCollection),sum(US_2015_April$GrossCollection),sum(US_2015_May$GrossCollection),sum(US_2015_June$GrossCollection),
              sum(US_2015_July$GrossCollection),sum(US_2015_August$GrossCollection),sum(US_2015_September$GrossCollection),sum(US_2015_October$GrossCollection),
              sum(US_2015_November$GrossCollection),sum(US_2015_December$GrossCollection),sum(US_2016_January$GrossCollection),sum(US_2016_February$GrossCollection),
              sum(US_2016_March$GrossCollection),sum(US_2016_April$GrossCollection),sum(US_2016_May$GrossCollection),sum(US_2016_June$GrossCollection),
              sum(US_2016_July$GrossCollection),sum(US_2016_August$GrossCollection),sum(US_2016_September$GrossCollection),sum(US_2016_October$GrossCollection),
              sum(US_2016_November$GrossCollection),sum(US_2016_December$GrossCollection),sum(US_2017_January$GrossCollection),sum(US_2017_February$GrossCollection),
              sum(US_2017_March$GrossCollection),sum(US_2017_April$GrossCollection),sum(US_2017_May$GrossCollection),sum(US_2017_June$GrossCollection),
              sum(US_2017_July$GrossCollection),sum(US_2017_August$GrossCollection),sum(US_2017_September$GrossCollection),sum(US_2017_October$GrossCollection),
              sum(US_2017_November$GrossCollection), sum(US_2017_December$GrossCollection))
Gross_collection_vector = as.numeric(Total)
print(Gross_collection_vector)


##====Making numbers into time-series value====##
ecommerce_series <- ts(Gross_collection_vector, frequency = 12, start=c(2014,11))

print(ecommerce_series)


##====Plotting time-series====##
Historical_Gross_Collection <- ts.plot(ecommerce_series,
        main = "Gross Collection",
        ylab = "USD($)",
        ylim = c(-10000, 45000),
        xlab = "Year",
        lwd=2)
Historical_Gross_Collection_log <- ts.plot(log(ecommerce_series),
                                      main = "Gross Collection",
                                      ylab = "USD($)",
                                      xlab = "Year",
                                      lwd=2)

##====trends and seasonal patterns====##
# additive as the amount of variation is independent from the trend
D2D_decompose <- decompose(ecommerce_series, type = "additive")
plot(D2D_decompose)
summary(D2D_decompose$trend)

##==De-seasonalize the time-series==##
D2D_decompose_2 = stl(ecommerce_series, s.window = "periodic") 
seasadj(D2D_decompose_2)
seasonplot(seasadj(D2D_decompose_2),12,col=rainbow(9), year.labels=TRUE, main = "Seasonal Plot")




plot(D2D_decompose_2)
D2D_decompose_2_trend <- D2D_decompose_2$time.series[,2];D2D_decompose_2_trend
D2D_decompose_2_remainder <-D2D_decompose_2$time.series[,3];D2D_decompose_2_remainder


##====seasonally-adjusted sales====##
plot(D2D_decompose_2_trend+D2D_decompose_2_remainder, main = "Seasonally adjusted graph", xlab = "Time", ylab = "Gross Collection($)")






## Forecasting by month ##


##====Augmented Dickey-Fuller Test====##
adf.test(ecommerce_series, alternative = "stationary")
adf.test(diff(aa$GrossCollection))

## Because it is not stationary, we need to differentiate
nsdiffs(ecommerce_series)
ndiffs(ecommerce_series)
stationary_series <- diff(ecommerce_series,differences= 1)
plot(stationary_series, type="l", main="stationary time series") 


##====Fitting an ARIMA model====## (values go below 0 as it has decreasing sales trend)
deseasonal_cnt <- seasadj(D2D_decompose_2) #subtracting seasonal component from origianl series
fcast_des = auto.arima(deseasonal_cnt, seasonal=FALSE)
fcast1 = forecast(fcast_des);fcast1
plot(fcast1)

## This more accurate forecast ## 
fcast = auto.arima(ecommerce_series)
fcast2 = forecast(fcast, h=20);fcast2
plot(fcast2)


##====improve the forecast and iterate on this model(?)====##
fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
fit_w_seasonality

seas_fcast <- forecast(fit_w_seasonality, h=20);seas_fcast 
plot(seas_fcast)

##====exponential forecasting====##
m_ets = ets(ecommerce_series)
f_ets = forecast(m_ets, h=5) # forecast 24 months into the future
plot(f_ets)
