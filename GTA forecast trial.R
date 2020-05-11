setwd("C:/Users/Benson/Desktop")
data = read.csv("Total.csv")
data = data[,-c(14,15)]
data$date = as.Date(data$Payment.Date)
ggplot(data,aes(date,Gross.Revenue)) + geom_line()


require(dplyr)
require(tidyr)
data1 = data %>% gather(Date,revenue,Charge.Amount) %>%  group_by(date) %>% summarise(Average = mean(revenue))
data2 = data %>% gather(Date,revenue,Charge.Amount) %>%  group_by(date) %>% summarise(Total = sum(revenue))
a = ets(data1)






tss = ts(data1$Average, start = 1, frequency = 12)
tss2 = ts(data2$Total,start = c(2016,1), end = c(2017,1), frequency =365)
plot(tss)
ts.plot(tss2)
 tss = ets