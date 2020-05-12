require(forecast)
GTA = read.csv("GTAA.csv")
GTAA = GTA %>% gather(Date, revenue, Charge.Amount) %>% group_by(date) %>% summarise(Total = sum(revenue)) %>% gather(ttotal,) %>% summarise(Gross.Collection = sum())
plot(GTAA, type = "p")
abline(reg = lm(Total ~ time(Total), data = GTAA), col = "red")
mean(GTAA$Total) ## $124/day for our 48 days of GTA 5 sales


GTAAA = read.csv("GTAAA.csv")

count_ma = ts(GTAAA$Total,frequency = 12, start = c(2016,11), end = c(2017,10))
count_ma
decomp = stl(count_ma, s.window = "period") 
adf.test(count_ma, alternative = "stationary")
ggtsdisplay(count_ma)


aa = auto.arima(count_ma)

fcast = forecast(aa, h=10)
plot(fcast)



a = glm(Total ~ date, data = GTAA);a
plot(a)
ggplot(data = GTA, aes(date,Charge.Amount)) + geom_freqpoly()
junk = GTA 
table(GTA$date)

##==================================================================================================##
## ARIMA forecasting method
require(tseries)
aa = read.csv("aa.csv")
aa$GrossCollection = ts(aa$GrossCollection, frequency = 53, start = c(2016,46), end = c(2017,46))

##decomp = stl(aa$GrossCollection, s.window = "periodic") 

plot(aa$GrossCollection)
abline(reg = lm(aa$GrossCollection~time(aa$GrossCollection)))
## Becuase mean is constantly changing, we need to differentiate
plot(log(aa$GrossCollection))

plot(diff(log(aa$GrossCollection)))

boxplot(aa$GrossCollection~cycle(aa$GrossCollection))

##Dickey Fuller test

adf.test(aa$GrossCollection)

adf.test(diff(aa$GrossCollection))

adf.test(aa$GrossCollection~cycle(aa$GrossCollection))
##adf.test(aa$GrossCollection, k=2)  It is likely to be cyclical beyound 2 weeks


##================================================================================
## AR I MA
#  p  d  q

acf(diff(log(aa$GrossCollection))) #Determines the value of q

pacf(diff(log(aa$GrossCollection))) #Determines the value of p


# d = number of times I want to do differentiation


ggtsdisplay(aa$GrossCollection) # Do ACF, PACF 

ggseasonplot(aa$GrossCollection)

##########################################################
fit2 = arima(log(aa$GrossCollection),c(4,1,0))
summary(fit2)

pred = predict(fit2,n.ahead = 30);pred

pred1 = 2.718^pred$pred;pred1

ts.plot(aa$GrossCollection,2.718^pred$pred, lty = c(1,3))

########################################
##Test model 
## use back data to test whether its matched with original valeu!
datawide = ts(aa$GrossCollection, frequency = 53, start = c(2014,46), end = c(2015,46))


##########################################################
pp = auto.arima(aa$GrossCollection)
pp2 = arima(aa$GrossCollection,c(4,1,0))

summary(pp)
summary(pp2)
summary(aa)

#dd = diff(aa$Gross.Collection)
#plot(dd)
         

fit_forecast = forecast(pp, h=15)
fit_forecast2 = forecast(pp2, h=15)

print(fit_forecast)
print(fit_forecast2)

autoplot(fit_forecast)
autoplot(fit_forecast2)


# check
plot.ts(pp$residuals) ## shouldn't have any pattern!
qqnorm(pp$residuals)
acf(pp$residuals)

##==================================================================================================##

##Beautiful time series plot using xts
require(xts)
require(ggplot2)
##as.date nono
bb = xts(x = aa$Gross.Collection, order.by = as.Date(paste0(aa$Date,"-01")));bb

temp = data.frame(index(bb),stack(as.data.frame(coredata(bb))))

names(temp)[1] = "Date"
names(temp)[2] = "Gross_Collection"
temp = temp[,-3]

ggplot(temp, aes(Date, Gross_Collection)) + geom_line()


##==================================================================================================##
## Time Series using regrssion model!!

## XGboost
require(xgboost)
aaa = xgbts(aa$GrossCollection)
