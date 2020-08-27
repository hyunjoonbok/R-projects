require(scales)
require(readxl)
require(tidyverse)
require(lubridate)
require(tidyquant)
require(ggplot2)
require(data.table)
require(dplyr)
require(plotly)
#require(bbplot)
require(stringr)
require(ggthemes)
library(viridis)
library(hrbrthemes)
library(magick)
library(webshot)
library(kableExtra)
library(flextable)
require(timetk)     # Toolkit for working with time series in R
require(tidyquant) 
library(data.table)
require(readr)

setwd("C:/Users/bokhy/Desktop/")
D2D <- read_csv("D2D.csv")
WMT <- read_csv("WMT.csv")


D2D$`Product Title` <- gsub("\\<U+0099>s","",D2D$`Product Title`)
D2D$`Product Title` <- gsub("\\<U+393C>","",D2D$`Product Title`)
D2D$`Product Title` <- gsub("\\<U+3E39>s","",D2D$`Product Title`)
D2D$`Product Title` <- gsub("\\â„¢","",D2D$`Product Title`)
D2D$`Product Title` <- gsub("\\?","",D2D$`Product Title`)
D2D$`Product Title` <- gsub("\\??","",D2D$`Product Title`)
D2D$`Product Title` <- gsub("\\{EU}","",D2D$`Product Title`)
D2D$`Product Title` <- gsub("\\{UK}","",D2D$`Product Title`)
D2D$`Product Title` <- gsub("\\®","",D2D$`Product Title`)
D2D$`Product Title` <- gsub("[[:blank:]]","",D2D$`Product Title`)


WMT$Products <- gsub("\\<U+0099>s","",WMT$Products)
WMT$Products <- gsub("\\<U+393C>","",WMT$Products)
WMT$Products <- gsub("\\<U+3E39>s","",WMT$Products)
WMT$Products <- gsub("\\â„¢","",WMT$Products)
WMT$Products <- gsub("\\?","",WMT$Products)
WMT$Products <- gsub("\\??","",WMT$Products)
WMT$Products <- gsub("\\{EU}","",WMT$Products)
WMT$Products <- gsub("\\{UK}","",WMT$Products)
WMT$Products <- gsub("\\®","",WMT$Products)
WMT$Products <- gsub("[[:blank:]]","",WMT$Products)

D2D$Publisher[D2D$Publisher == "HandyGames, THQ Nordic"] <- "HandyGames"

# Daily Revenue

dd <- D2D %>% select(`Product Title`, Publisher, Email, `Final Gross collection`) %>% 
  group_by(`Product Title`,Publisher) %>% tally() %>% arrange(desc(n))

ww <- WMT %>% select(Products, Publishers) %>% 
  group_by(Products,Publishers) %>% tally() %>% arrange(desc(n))


D2D <- D2D %>% separate(`Date Purchased`,
                        c("Date","a",'timezone'), sep = ' ')
D2D$Time <- gsub("\\..*","",D2D$a)
D2D$Date <- as.Date(D2D$Date, "%Y-%m-%d")
D2D$Weekdays <- weekdays(D2D$Date)
D2D$month <- month(D2D$Date)
D2D$date <- date(D2D$Date)
D2D$year <- year(D2D$Date)
D2D$hour <- as.numeric(gsub("\\:.*$", "", D2D$Time))
D2D$timeoftheday<- with(D2D, ifelse(hour >= 5 & hour<=11, "morning",
                                      ifelse(hour>11 & hour<=16, "afternoon",
                                             ifelse(hour>16 & hour<=21, "evening" ,"night"))))
D2D$month <- month.abb[D2D$month]

WMT <- WMT %>% separate(`Payment Date`,
                        c("Date","a"), sep = ' ')
WMT$Time <- gsub("\\..*","",WMT$a)
WMT$Date <- as.Date(WMT$Date, "%Y-%m-%d")
WMT$Weekdays <- weekdays(WMT$Date)
WMT$month <- month(WMT$Date)
WMT$date <- date(WMT$Date)
WMT$year <- year(WMT$Date)
WMT$hour <- as.numeric(gsub("\\:.*$", "", WMT$Time))
WMT$timeoftheday<- with(WMT, ifelse(hour >= 5 & hour<=11, "morning",
                                    ifelse(hour>11 & hour<=16, "afternoon",
                                           ifelse(hour>16 & hour<=21, "evening" ,"night"))))
WMT$month <- month.abb[WMT$month]


d2d_users <- D2D %>% 
  group_by(date) %>% 
  summarise(value =  sum(`Final Gross collection`)) %>%
  pad_by_time(date, .by = "auto") %>% 
  mutate(Service = 'D2D') %>% 
  replace_na(list(value = 0)) %>% 
  set_names(c("date", "value", "Service"))

wmt_users <- WMT %>% 
  group_by(date) %>% 
  summarise(value =  sum(`Gross Collection(Cost)`)) %>% 
  mutate(Service = 'WMT') %>% 
  replace_na(list(value = 0)) %>% 
  set_names(c("date", "value","Service"))

all_users <- rbind(d2d_users,wmt_users)

all_users %>% 
  ggplot(aes(x = date, y = value,  color = Service)) +
  geom_line(size = 1) +
  geom_rect(xmin = as.numeric(ymd("2020-08-01")),
            xmax = as.numeric(ymd("2020-08-02")),
            ymin = 0, ymax = 8000,
            fill = palette_light()[[1]], 
            alpha = 0.007,color="gray") +
  geom_rect(xmin = as.numeric(ymd("2020-08-08")),
            xmax = as.numeric(ymd("2020-08-09")),
            ymin = 0, ymax = 8000,
            fill = palette_light()[[1]], 
            alpha = 0.007,color="gray") +
  geom_rect(xmin = as.numeric(ymd("2020-08-15")),
            xmax = as.numeric(ymd("2020-08-16")),
            ymin = 0, ymax = 8000,
            fill = palette_light()[[1]], 
            alpha = 0.007,color="gray") +
  geom_rect(xmin = as.numeric(ymd("2020-08-22")),
            xmax = as.numeric(ymd("2020-08-23")),
            ymin = 0, ymax = 8000,
            fill = palette_light()[[1]], 
            alpha = 0.007,color="gray") +
  theme_tq() + 
  scale_y_continuous(expand = c(0, 0), position = "right",
                     label = label_number(suffix = " ($)")) +
  labs(title = "Daily Revenue - Aug.2020",
       subtitle = 'Highlighted Area - Weekend',
       x = "", y = "", color = "Service")

# Weekly Revenue
d2d_users_weekly <- d2d_users %>%
  tq_transmute(select     = value,
               mutate_fun = apply.weekly,
               FUN        = sum,
               na.pad = TRUE) %>% 
  mutate(Service = 'D2D') 

wmt_users_weekly <- wmt_users %>%
  tq_transmute(select     = value,
               mutate_fun = apply.weekly,
               FUN        = sum,
               na.pad = TRUE) %>% 
  mutate(Service = 'WMT') 

  
all_users_weekly <- rbind(d2d_users_weekly,wmt_users_weekly)  
all_users_weekly$date <- as.factor(all_users_weekly$date)
all_users_weekly %>% 
ggplot(aes(fill=Service, y=value, x=date)) + 
  geom_bar(colour="black", stat = 'identity',position="stack") +
  theme_tq() + 
  scale_y_continuous(expand = c(0, 0), position = "right",
                     label = label_number(suffix = " ($)")) +
  labs(title = "Weekly Revenue - Aug.2020",
       x = "", y = "", color = "Service")


# Revenue Cooupation by publisher
d2d_rev_occupation <- D2D %>% group_by(Publisher) %>% 
  summarise(value = round(sum(`Final Gross collection`))) %>% 
  arrange(desc(value)) %>% 
  mutate(Service = 'D2D') %>% 
  set_names(c("Publisher", "value", "Service")) %>% 
  mutate(lab.ypos = cumsum(value) - 0.5*value) 

wmt_rev_occupation <- WMT %>% group_by(`Publisher Display`) %>% 
  summarise(value = round(sum(`Gross Collection(Cost)`))) %>% 
  arrange(desc(value)) %>% 
  mutate(Service = 'WMT') %>% 
  set_names(c("Publisher", "value", "Service")) %>% 
  mutate(lab.ypos = cumsum(value) - 0.5*value) 