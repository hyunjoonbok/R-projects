# ====  PowerBI ====

library(modeltime)
require(scales)
require(readxl)
require(tidyverse)
require(lubridate)
require(tidyquant)
require(ggplot2)
require(data.table)
require(dplyr)
require(plotly)
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

# 1. Streaming ====

# Read data
setwd("C:/Users/bokhy/Desktop/ATG/Power BI")
b <- read.csv("session_byuser_export.csv")
d <- read.csv("employee.csv")

## Exclude Employee
b <- b %>% filter(!Email %in% d$Email)

b$Service.Duration <- as.character(b$Service.Duration)
b$Service.Duration <- ifelse(nchar(b$Service.Duration) > 20, as.character(b$Service.Start.Time), b$Service.Duration)

b$Service.Start.Time <- as.character(b$Service.Start.Time)
b$Service.Start.Time <- ifelse(nchar(b$Service.Start.Time) < 10, as.character(b$temp1), b$Service.Start.Time)

b$Service.Duration <-  as.numeric(b$Service.Duration)

b <- b %>% separate(Service.Start.Time,
                    c("Date","a",'timezone'), sep = ' ')

b$Time <- gsub("\\..*","",b$a)

# Change to PST (UTC - 8 hours)
b$Time <- as.POSIXct(b$Time, format = "%H:%M:%S")
b$Time <- as_datetime(b$Time, tz = "America/Los_Angeles") - (3600*8)
b$Time <- as.ITime(b$Time)

b$Date <- as.Date(b$Date, "%Y-%m-%d")

b$Weekdays <- weekdays(b$Date)
b$month <- month(b$Date)
b$date <- date(b$Date)
b$year <- year(b$Date)

b$hour <- as.numeric(gsub("\\:.*$", "", b$Time))
b$timeoftheday<- with(b, ifelse(hour >= 5 & hour<=11, "morning",
                                ifelse(hour>11 & hour<=16, "afternoon",
                                       ifelse(hour>16 & hour<=21, "evening" ,"night"))))

b$month <- month.abb[b$month]
b$month <- factor(b$month,levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", 
                                     "Apr",'May','Jun','Jul','Aug','Sep'))
b$weeknum <- WEEKNUM(b$date)
b$quarter <- QUARTER(b$date)

b <- b[,-1]

write.csv(b,"session_byuser_export.csv", row.names = FALSE)


#2. Cumulative Unique User ==== 
setwd("C:/Users/bokhy/Desktop/ATG")
c <- read.csv('Production Arcade Logs_new.csv')
c %>% group_by(machine_uuid) %>% count()


#2. Third-Party Data ==== (not using)

# Read Data
setwd("C:/Users/bokhy/Desktop/ATG/Power BI")
thrid_party <- read.csv('[New] 3rd Party Data.csv')

thrid_party <- thrid_party %>% filter(!account.email %in% d$Email)

thrid_party <- thrid_party %>% separate(X.timestamp,
                    c("Date",'Time'), sep = '@')

thrid_party$Time <- gsub("\\..*","",thrid_party$Time)

write.csv(thrid_party,"[New] 3rd Party Data.csv", row.names = FALSE)


#3. Active Users (Daily,Weekly, Monthly) ====
users <- b %>% 
  select(User, Date, Service.Duration, Service.Type, month) %>% 
  filter(!Service.Type == "MGR") %>% 
  filter(!User %in% c("2c3a79f1-e644-4b0d-a384-ebda19a749d1",
                      "37cea104-cc49-459f-84ce-548bb6d1a54e",
                      "741fd86f-9b6e-44e6-8407-68ce2cbbf7a3",
                      "instant01571898606570",
                      "instant01571906403420",
                      "3fda3fc0-7d5c-4b06-8dd7-ea2cfb4d2a2f",
                      "75b8ccd3-2202-44be-9b9d-65e7fe083600",
                      "d3b9de81-9ada-41f7-8019-03c2b0881054",
                      "16fff7ac-06d7-425b-84ad-9d17113fce5a",
                      "136308a8-5696-4e82-909c-0e8b901e90b9",
                      "153e9ab1-a3d2-4ce4-aafc-ad01b32bd035",
                      "dde7faf3-2ffa-42c0-b133-d78ec81006d0",
                      "38a7cfef-81d4-42cd-9ae2-57ed99331dc6",
                      "1957f50a-3831-48a4-99fe-27c4d04f8f18",
                      "156ae6f6-b76c-4128-9a6e-4d92e3cc7477",
                      "00d6091a-6ac8-4156-910c-8994303ce0e8",
                      "2faad4cf-0927-490e-910e-1078894e2254",
                      "instant01571991043043",
                      "instant01571970793132",
                      "instant01571927166160",
                      "instant01571926879050",
                      "instant01571924112161",
                      "instant01571924037842")) %>%
  group_by(Service.Type, Date, User) %>% 
  summarise(Session_opened = n())%>% 
  distinct()

users <- users %>% 
  group_by(Service.Type, Date) %>% 
  summarise(value =  n()) %>% 
  set_names(c("Service","date", "value"))

setwd("C:/Users/bokhy/Desktop/ATG")

prod <- read.csv("Production Arcade Logs_new.csv")
prod$activity.game_id <- as.character(prod$activity.game_id)
prod1 <- prod %>% filter(nchar(activity.game_id) < 10)
prod1$activity.game_id <- as.numeric(prod1$activity.game_id)
prod2 <- prod %>% filter(nchar(activity.game_id) >= 10)


prod1 <- prod1 %>%
  mutate(
    service = case_when(
      activity.game_id < 10000 ~ 'Built-in 350',
      activity.game_id < 10000000 ~ 'BYOG',
      activity.game_id >= 10000000 ~ 'ArcadeNet',
      TRUE ~ 'AddOn'
    )
  ) 

prod2$service <- "AddOn" 

prod1$activity.game_id <- as.character(prod1$activity.game_id)
prod2$activity.game_id <- as.character(prod2$activity.game_id)

prod <- bind_rows(prod1, prod2)


positions <- c("4.10.0","4.11.1","4.12.0","4.13.0",
               "4.14.0","4.14.1","4.16.0","4.17.0",
               "4.18.0","4.19.0","4.20.0","4.21.0",
               "4.22.0","4.22.0","4.23.0","4.24.0",
               "4.25.0","4.26.0","4.26.1","4.27.0",
               "4.28.0","4.29.0","4.30.0","4.31.0",
               "4.32.0","4.33.0","4.34.0","5.0.0",
               "5.1.0","5.2.0","5.3.0","5.4.0")

# Excluding bad entries (4.15.0) and people before 4.11.0

prod <- prod %>% 
  #  filter(!service == "AddOn") %>% 
  filter(!activity.platform == "Byog") %>%
  filter(activity.display_firmware %in% positions)   # Filter out 4.15.0 in Firmware version 
#  filter(!activity.display_firmware == "4.15.0") # Filter out Blank

prod$activity.game_title <- gsub("\\??","",prod$activity.game_title)
prod$activity.game_title <- gsub("\\?","",prod$activity.game_title)
prod$activity.game_title <- gsub("\\®","",prod$activity.game_title)
prod$activity.game_title <- gsub("[[:blank:]]","",prod$activity.game_title)


prod$activity.platform <- as.character(prod$activity.platform)
prod$service <- as.character(prod$service)

prod$activity.platform <- ifelse(prod$activity.platform == "", prod$service,prod$activity.platform)

prod$activity.platform[prod$activity.platform == 'BuildIn'] <- 'Built-in 350'
prod$activity.platform[prod$activity.platform == 'Byog'] <- 'BYOG'


prod$activity.platform <- as.factor(prod$activity.platform)

prod <- prod %>% 
  filter(!activity.platform == "BYOG")

table(prod$activity.platform)


data <- prod

# Pre-processing
data$activity.play_duration <- gsub(",","",data$activity.play_duration)
data <- transform(data, activity.play_duration = abs(as.numeric(activity.play_duration))) # Change negative value to positive (absolute)
nrow(data[data$activity.play_duration<0,]) # see if there are any negative record remaining
data$activity.play_duration <- (data$activity.play_duration)/3600

data <-  data %>% 
  filter(activity.play_duration < 12)

data <- data %>% separate(log_at,
                          c("Date","a"), sep = '@')

data$Time <- gsub("\\..*","",data$a)

data$Date <- as.Date(data$Date, "%B %d, %Y")

data$Weekdays <- weekdays(data$Date)
data$month <- month(data$Date)
data$date <- date(data$Date)
data$year <- year(data$Date)

data$hour <- as.numeric(gsub("\\:.*$", "", data$Time))
data$timeoftheday<- with(data, ifelse(hour >= 5 & hour<=11, "morning",
                                      ifelse(hour>11 & hour<=16, "afternoon",
                                             ifelse(hour>16 & hour<=21, "evening" ,"night"))))

data$month <- month.abb[data$month]

# Make sure to add month
data$month <- factor(data$month,levels = data("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", 
                                              "Apr",'May','Jun','Jul','Aug','Sep'))

data <- data %>% filter(!year == 1969)

builtin350 <- data %>% 
  select(machine_uuid, date, activity.play_duration, 
         activity.platform, activity.display_firmware, month,
         hour, year, Weekdays, timeoftheday) %>% 
  filter(activity.platform == "Built-in 350") %>% 
  group_by(activity.platform, machine_uuid, date) %>% 
  summarise(Session_opened = n()) %>% 
  distinct()

builtin350 <- builtin350 %>% 
  group_by(activity.platform, date) %>% 
  tally() %>% 
  set_names(c("Service","date", "value"))

# AddOn Users
AddOn <- data %>% 
  select(machine_uuid, date, activity.play_duration, 
         activity.platform, activity.display_firmware, month,
         hour, year, Weekdays, timeoftheday) %>% 
  filter(activity.platform == "AddOn") %>% 
  group_by(activity.platform, machine_uuid, date) %>% 
  summarise(Session_opened = n()) %>% 
  distinct()

AddOn <- AddOn %>% 
  group_by(activity.platform, date) %>% 
  tally() %>% 
  set_names(c("Service","date", "value"))

## Selecting Dates per each service
ArcadeNet <- users %>% 
  filter(between(date, "2020-09-07", "2020-10-04")) %>%
  filter(Service == "ArcadeNet")

BYOG <- users %>% 
  filter(between(date, "2020-09-07", "2020-10-04")) %>%
  filter(Service == "Cloud BYOG")

builtin350 <- builtin350 %>% 
  filter(between(date, "2020-09-07", "2020-10-04")) 

AddOn <- AddOn %>% 
  filter(between(date, "2020-09-07", "2020-10-04")) 

All_services <- rbind(builtin350,ArcadeNet,BYOG,AddOn)

setwd("C:/Users/bokhy/Desktop/ATG/Power BI")
write.csv(All_services, "All_services.csv", row.names = FALSE)

## At this moment, add [All_services] value to 
## 'users_daily.csv' 