# All files and working directory
setwd("C:/Users/bokhy/Desktop/ATG")

a <- read.csv("export_all_total_serving_time_report.csv")
b <- read.csv("session_byuser_export.csv")
c <- read.csv("Streaming_payments.csv")

d <- read.csv("employee.csv")

##=============================================================##
# Load packages
require(scales)
require(readxl)
require(tidyverse)
require(lubridate)
require(tidyquant)
require(ggplot2)
require(data.table)
require(dplyr)
require(plotly)
require(bbplot)
require(ggplot2)
require(stringr)
require(ggthemes)
library(viridis)
library(hrbrthemes)
library(magick)
library(webshot)
library(kableExtra)
library(flextable)
require(ggfittext)
require(timetk)     # Toolkit for working with time series in R
require(tidyquant) 


# Read data
# a <- read.csv("export_all_total_serving_time_report.csv")

# filter out atgames emplyee emails
a <- a %>% 
  filter(!Email %in% d$Email)
b <- b %>% 
  filter(!Email %in% d$Email)

# change datetime

a$Total.Serving.Time <- hms(a$Total.Serving.Time)
a$Total.Serving.Time <- as.difftime(a$Total.Serving.Time, "%H:%M:%S")

# Number of Unique Users
a_0 <- a %>% group_by(Email) %>% select(Email) %>% distinct() %>% count()
a_0

# Total Usage
a_1 <- a %>% 
  select(Email,Total.Serving.Time, Service.Type) %>% 
  group_by(Service.Type) %>% 
  summarise(total_hours = sum(Total.Serving.Time)/3600)
sum(a_1$total_hours)
a_1

# Avg. minutes per user (per service)

temp <- a %>% group_by(Service.Type, Email) %>% distinct() %>% tally() 

sum(a_1$total_hours)/3439

# Usage bucket per service

a_2 <- a %>% mutate(playtime = Total.Serving.Time/3600) %>% arrange(desc(playtime)) 

# together
a_2 %>%
  rename("Service" = Service.Type) %>% 
  ggplot(aes(x = playtime, fill = Service)) +
  geom_histogram(binwidth = 0.5,color = "#e9ecef" , position = "identity") +
  scale_color_brewer(palette = "Blues") +
  labs(y = "User Count", x = "Hours played", subtitle = "Each bin represents 30 min") +
  theme(legend.position ="top", legend.box = "horizontal",
        legend.background = element_rect(fill = "lightblue",
                                         size = 0.5, linetype = "solid", 
                                         colour = "#e9ecef")) +
  xlim(0, 15) 

# separate
a_2 %>%
  ggplot(aes(x = playtime)) +
  geom_histogram(binwidth = 0.5,color = "#e9ecef" , position = "identity") +
  scale_color_brewer(palette = "Blues") +
  labs(y = "User Count", x = "Hours played", subtitle = "Each bin represents 30 min") +
  facet_wrap(~ Service.Type)



# Avg. Usage per user per session(graph)

# read data
#setwd("C:/Users/bokhy/Desktop/ATG")
# b <- read.csv("session_byuser_export.csv")
# b <- b %>% filter(!Email %in% d$Email)

b$Service.Duration <- as.character(b$Service.Duration)
b$Service.Duration <- ifelse(nchar(b$Service.Duration) > 20, as.character(b$Service.Start.Time), b$Service.Duration)

b$Service.Start.Time <- as.character(b$Service.Start.Time)
b$Service.Start.Time <- ifelse(nchar(b$Service.Start.Time) < 10, as.character(b$temp1), b$Service.Start.Time)

b$Service.Start.Time <- as.Date(b$Service.Start.Time)
b$month <- month(b$Service.Start.Time)

b$Service.Duration <-  as.numeric(b$Service.Duration)

b$month <- month.abb[b$month]
b$month <- factor(b$month,levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr","May","Jun"))


b_new <- b %>% 
  select(User, Service.Duration, Service.Type, month) %>% 
  group_by(User) %>%
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
                      "instant01571924037842"))
  
# Avg.Usage per Session per Service (graph)
b_1 <- b_new %>% 
  group_by(User, Service.Type) %>% 
  filter(Service.Duration != 0) %>% 
  summarise(total_hours = round(sum(Service.Duration)/3600,2), count = n()) %>% 
  mutate(usage_per_session = (total_hours / count)*60) %>% # in minutes
  arrange(desc(usage_per_session)) 

b_1 %>% 
  filter(usage_per_session < 50) %>% 
  ggplot(aes(x = usage_per_session)) +
  geom_histogram(binwidth = 2, fill = "#69b3a2",color = "#e9ecef" , position = "identity") +
  scale_color_brewer(palette = "Blues") +
  labs(y = "User Count", x = "Minutes played per session", subtitle = "Each bin represents 2 min") +
  facet_wrap(~ Service.Type)

# Total Session count per Month
b_2 <- b_new %>% filter(Service.Duration != 0) %>% 
  group_by(User,month, Service.Type) %>% 
  select(User,month,Service.Type) %>% 
  tally()

b_2_new <- b_2 %>%
  filter(month != 0) %>%
  group_by(month, Service.Type)%>% 
  summarise(Count = sum(n))%>% 
  mutate(highlight_flag = ifelse(month == 'Jun', T, F))

b_2_new %>% 
  filter(!Service.Type == 'MGR') %>% 
  ggplot(aes(x = month , y = Count, fill = highlight_flag)) +
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c('grey', 'red')) +
  labs(y = "Total Session Count", x = "Month") +
  facet_wrap(~Service.Type) +
  theme(legend.position = 'none') +
  geom_text(aes(x= month, label=round(Count,0)), vjust=-0.25)

# Total number of users last week was : 301
b_temp <- read.csv("session_byuser_export.csv")
b_temp <- b_temp %>% 
  filter(!Email %in% d$Email)
b_temp$Service.Duration <- as.character(b_temp$Service.Duration)
b_temp$Service.Duration <- ifelse(nchar(b_temp$Service.Duration) > 20, as.character(b_temp$Service.Start.Time), b_temp$Service.Duration)
b_temp$Service.Start.Time <- as.character(b_temp$Service.Start.Time)
b_temp$Service.Start.Time <- ifelse(nchar(b_temp$Service.Start.Time) < 10, as.character(b_temp$temp1), b_temp$Service.Start.Time)
b_temp$Service.Start.Time <- as.Date(b_temp$Service.Start.Time)

# Change the period we want to see for unique streaming user count per service
b_temp <- b_temp %>%  
  filter(between(Service.Start.Time, "2020-06-22", "2020-06-28"))

b_temp_0 <- b_temp %>% 
  group_by(Email,Service.Type) %>%  
  summarise(Session_opened = n()) %>% 
  distinct()
# Total weekly users
b_temp_0 %>% group_by(Service.Type) %>% tally()
# number of sessions
b_temp_0 %>% group_by(Service.Type) %>% summarise(total = sum(Session_opened))


# Avg. Minutes played per user (chart C above)
b_temp$Service.Duration <-  as.numeric(b_temp$Service.Duration)
b_temp %>% group_by(Service.Type) %>% summarise(total = sum(Service.Duration))
b_temp %>% select(Email, Service.Type) %>% distinct() %>% group_by(Service.Type) %>% summarise(count = n())
#ArcadeNet: 
# (playtime in hours / total weekly users)
267183/3600/239
#BYOG: 
# (playtime in hours / total weekly users)
204596/3600/94


# Hours played per month (chart)
b_3 <- b_new %>% 
  filter(!month == "") %>% 
  group_by(month) %>% 
  summarise(total_hours = format(round(sum(Service.Duration/3600),1), nsmall = 1, big.mark = ',' )) 

ft <- flextable(b_3)
ft <- autofit(ft)
plot(ft)

# by service type?
b_4 <- b_new %>% 
  group_by(Service.Type,month) %>% 
  summarise(total_hours = format(round(sum(Service.Duration/3600),1), nsmall = 1, big.mark = ',' ))


# save as on image
#flextable::save_as_image(b_3, path = "test.png")


# Hours Consumed per month (graph)
b_new %>% 
  rename("Service" = Service.Type) %>% 
  filter(!Service == 'MGR') %>% 
  filter(month != "") %>% 
  group_by(month, Service) %>% 
  summarise(Hours_consumed = sum(Service.Duration/3600)) %>%
  mutate(percentage = round(prop.table(Hours_consumed),2)) %>% 
  ggplot(aes(x = month , y = Hours_consumed, fill = Service, label = percent(percentage))) +
  geom_bar(position="dodge", stat = "identity") +
  labs(x= "Month", y = "Streaming Hours Consumed") +
  theme_ipsum() +
  xlab("") +
  ylab("") +
  theme(legend.box = "horizontal",
        legend.background = element_rect(fill = "lightblue",
                                         size = 0.5, linetype = "solid", 
                                         colour = "#e9ecef")) +
  geom_text(position=position_dodge(width=0.9), vjust=-0.2, size = 3.2) +
  geom_text(aes(x= month, label=round(Hours_consumed,0)), position=position_dodge(width=0.9), vjust=2) 




#==== BYOG Hour Purchase stats ====#

# read data
#setwd("C:/Users/bokhy/Desktop/ATG")
c <- read.csv("Streaming_payments.csv")

# [1] Total # of Users
c %>% group_by(Customer.Email) %>% count()

# [2] Total Purchase Amount $
sum(c$Amount)

# [3] Purchase Type
c_1 <- c %>% group_by(Description) %>% count() %>% rename("Type" = Description, "# of purchases" = n)

ft <- flextable(c_1)
ft <- autofit(ft)
plot(ft)

c_1_1 <- c %>% group_by(Customer.Email) %>% tally() %>% arrange(desc(n)) %>% 
  rename(Number_of_purchases = n) %>% top_n(5)# any mutiple purchase user?

# Processing
c <- c %>%
  mutate(
    purchase_term = case_when(
      Days.Passed.since.Log.in.Creation >= 8 & Days.Passed.since.Log.in.Creation < 16 ~ '1 wk to \n 2 wks',
      Days.Passed.since.Log.in.Creation >= 16 & Days.Passed.since.Log.in.Creation < 60 ~ '2 wks to \n 2 month',
      Days.Passed.since.Log.in.Creation >= 60 ~ 'Over \n 2 month',
      TRUE ~ 'NA'
    )
  ) %>% 
  mutate(
    New_customer = case_when(
      Days.Passed.since.Log.in.Creation >= 30 ~ 'N',
      TRUE ~ 'Y'
    )
  )

c$purchase_term <- ifelse(c$purchase_term == 'NA', paste0(c$Days.Passed.since.Log.in.Creation," ","days"), c$purchase_term)
c$month <- month(as.POSIXlt(c$Created..UTC., format="%m/%d/%Y %H:%M"))
c$month <- month.abb[c$month]

# Make sure to add month
c$month <- factor(c$month,levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", 
                                     "Apr",'May','Jun','Jul','Aug','Sep'))

# Monthly Purchase history (per option)
c %>% 
  ggplot(aes(x = month, fill = Description)) +
  geom_histogram(stat = 'count',color = "#e9ecef")+
  labs(y = "# of Purchase", x = "Month") +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() +
  xlab("") +
  theme(legend.position ="top", legend.box = "horizontal",
        legend.background = element_rect(fill = "lightblue",
                                         size = 0.5, linetype = "solid", 
                                         colour = "#e9ecef")) +
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5), color = "white", size = 5) 



# How many are new customer at the moment of purchase?  
c %>% 
  ggplot(aes(x = month, fill = New_customer)) +
  geom_histogram(stat = 'count',color = "#e9ecef")+
  labs(y = "# of Purchase", x = "Month") +
  theme_ipsum() +
  xlab("") +
  theme(legend.position ="top", legend.box = "horizontal",
        legend.background = element_rect(fill = "lightblue",
                                         size = 0.5, linetype = "solid", 
                                         colour = "#e9ecef")) +
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5), size = 5) 



c %>% group_by(New_customer) %>% tally()


# Consolidated Purchase Term
c_1 <- c %>% 
  mutate(purchase_term = factor(purchase_term, levels = c("0 days", "1 days", "2 days", "3 days", "4 days", "5 days", "6 days", "7 days", 
                                                          "1 wk to \n 2 wks", "2 wks to \n 2 month", "Over \n 2 month"))) %>% 
  group_by(purchase_term,New_customer) %>%
  summarise(Count = n())

c_1 %>% 
  ggplot(aes(x = purchase_term, y = Count)) +
  geom_bar(aes(fill = New_customer), stat="identity", alpha=.9, width=.7) +
  labs(y = "# of Purchases", size = 1) +
  theme_economist() +
  geom_text(aes(x= purchase_term, label=round(Count,0)), vjust = -0.5)


# Purchase Term Breakdown per month
c %>% 
  mutate(purchase_term = factor(purchase_term, levels = c("0 days", "1 days", "2 days", "3 days", "4 days", "5 days", "6 days", "7 days", 
                                                          "1 wk to \n 2 wks", "2 wks to \n 2 month", "Over \n 2 month"))) %>% 
  ggplot(aes(x = purchase_term, fill = New_customer)) +
  geom_bar(stat="count", position = 'identity', alpha=.9, width=.7) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") + 
  labs(x= "Purchase term", y = "# of Purchases", size = 1) +
  theme_economist() +
  xlab("") +  
  theme(axis.text=element_text(size=7)) +
  facet_wrap(month ~.)


# Last week Top 5 Titles (from Opt-in User) - Chart J
setwd("C:/Users/bokhy/Desktop")
data <- read.csv("weekly_cleaned.csv")

data$activity.play_duration <- gsub(",","",data$activity.play_duration)
data <- transform(data, activity.play_duration = abs(as.numeric(activity.play_duration))) # Change negative value to positive (absolute)
nrow(data[data$activity.play_duration<0,]) # see if there are any negative record remaining
data$activity.play_duration <- (data$activity.play_duration)/3600

data <-  data %>% 
  filter(activity.play_duration < 12) %>% 
  distinct(activity.play_duration, .keep_all = TRUE)  
data <- data %>% separate(log.timestamp,
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


last_week_top_5 <- data %>% select(activity.platform,activity.play_duration, 
                                   activity.game_title, activity.play_start, 
                                   activity.play_end, month, geoip.city_name, account.email) %>% 
  
  filter(!activity.platform == "AddOn") %>% 
  filter(!activity.platform == "BYOG") %>% 
  filter(!activity.game_title == "") %>% 
  distinct() %>%
  group_by(activity.platform, activity.game_title) %>%
  summarise(total_hours = round(sum(activity.play_duration),1)) %>% 
  select(activity.game_title,activity.platform, total_hours) %>% 
  arrange(desc(total_hours)) %>% 
  mutate(percentage = percent(prop.table(total_hours))) %>% 
  slice(1:5) %>% 
  rename("Title" = activity.game_title, "Service" = activity.platform, 
         "Playtime(hrs)" = total_hours, "%_of_total_lastweek" = percentage)

ft <- flextable(last_week_top_5)
ft <- autofit(ft)
plot(ft)

  


# ==== Top 10 for each month ==== #

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


positions <- c("4.11.0","4.11.1","4.12.0","4.13.0",
               "4.14.0","4.14.1","4.16.0","4.17.0",
               "4.18.0","4.19.0","4.20.0","4.21.0",
               "4.22.0","4.22.0","4.23.0","4.24.0",
               "4.25.0")

# Excluding bad entries (4.15.0) and people before 4.11.0

prod <- prod %>% 
  filter(!service == "AddOn") %>% 
  filter(!activity.platform == "Byog") %>%
  filter(activity.display_firmware %in% positions)   # Filter out 4.15.0 in Firmware version 
#  filter(!activity.display_firmware == "") # Filter out Blank

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




# Chart L. Cumulative Top 5 Titles per service  

consol_top_5 <- data %>% 
  select(activity.platform,activity.play_duration, activity.game_title, activity.play_start, activity.play_end, month, geoip.city_name, account.email) %>% 
  distinct() %>% # remove dupplicate
  filter(!activity.game_title == "") %>% 
  group_by(activity.platform, activity.game_title) %>% 
  summarise(total_hours = round(sum(activity.play_duration),1)) %>% 
  select(activity.game_title,activity.platform, total_hours) %>% 
  arrange(desc(total_hours)) %>% 
  mutate(percentage = percent(prop.table(total_hours))) %>% 
  slice(1:5) %>% 
  rename("Title" = activity.game_title, "Service" = activity.platform, 
         "Playtime(hrs)" = total_hours, "%_of_total" = percentage)

ft <- flextable(consol_top_5)
ft <- autofit(ft)
plot(ft)



# Change Month filter in below graphs to check data for specifc months

# Top 5 per month [Built-in 350]
Built_in_top_5 <- data %>% 
  select(activity.platform,activity.play_duration, activity.game_title, activity.play_start, activity.play_end, month, geoip.city_name, account.email) %>% 
  distinct() %>% # remove dupplicate
  filter(!activity.game_title == "") %>% 
  group_by(month, activity.game_title) %>% 
  filter(activity.platform == 'Built-in 350') %>% 
  filter(month == 'Apr' | month == 'May' | month == 'Jun') %>% 
  summarise(total_hours = round(sum(activity.play_duration),1))%>% 
  select(activity.game_title,month, total_hours) %>% 
  arrange(desc(total_hours))%>% 
  mutate(percentage = percent(prop.table(total_hours))) %>% 
  slice(1:5) %>%
  top_n(15) %>% 
  rename("Title" = activity.game_title, "Playtime(hrs)" = total_hours, "%_of_total" = percentage)

ft <- flextable(Built_in_top_5)
ft <- autofit(ft)
plot(ft)



# Top 5 per month [ArcadeNet]
ArcadeNet_top_5 <- data %>% 
  select(activity.platform,activity.play_duration, activity.game_title, activity.play_start, activity.play_end, month, geoip.city_name, account.email) %>% 
  distinct() %>% # remove dupplicate
  filter(!activity.game_title == "") %>% 
  group_by(month, activity.game_title) %>% 
  filter(activity.platform == 'ArcadeNet') %>% 
  filter(month == 'Jan' | month == 'Feb' | month == 'Mar')%>% 
  summarise(total_hours = round(sum(activity.play_duration),1)) %>% 
  select(activity.game_title,month, total_hours) %>% 
  arrange(desc(total_hours))%>% 
  mutate(percentage = percent(prop.table(total_hours))) %>% 
  slice(1:5) %>%
  top_n(15) %>% 
  rename("Title" = activity.game_title, "Playtime(hrs)" = total_hours, "%_of_total" = percentage)

ft <- flextable(ArcadeNet_top_5)
ft <- autofit(ft)
plot(ft)
