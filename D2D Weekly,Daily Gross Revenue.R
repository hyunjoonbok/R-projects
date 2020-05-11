setwd("C:/Users/Benson/Desktop")
require(xlsx)
require(lubridate)
require(ggplot2)
require(dplyr)
require(tidyr)

D2D = read.xlsx("D2D_YTD.xlsx",1)
D2D$date = as.Date(D2D$Payment.Date)

## Weekly Breakdown
D2D$week = week(D2D$date)
D2D = D2D[ , -c(1,2,5,6,7,8,9,11,14,15,16,17)]

## Daily Breakdown

D2D$day = as.Date(D2D$Payment.Date)


## Weekly Breakdown graph
D2D_weekly = D2D %>% gather(week_num, revenue, Total.Amount) %>% group_by(week) %>% summarise(Total = sum(revenue))
  
ggplot(D2D_weekly,aes(week,Total)) + geom_line(lwd = 2)

ggsave(Weekly_ggplot,filename = "Weekly_breakdown", device = "pdf")

## Daily Breakdown graph
D2D_daily = D2D %>% gather(day_num, revenue, Total.Amount) %>% group_by(day) %>% summarise(Total = sum(revenue))

ggplot(D2D_daily,aes(day,Total)) + geom_line()




##
# D2D[c(6481:6487),]$week = -8
# D2D[c(6451:6480),]$week = -7
# D2D[c(5994:6450),]$week = -6
# D2D[c(5688:5993),]$week = -5
# D2D[c(5635:5687),]$week = -4
# D2D[c(5564:5634),]$week = -3
# D2D[c(4979:5563),]$week = -2
# D2D[c(4703:4978),]$week = -1
# D2D[c(4684:4702),]$week = 0
