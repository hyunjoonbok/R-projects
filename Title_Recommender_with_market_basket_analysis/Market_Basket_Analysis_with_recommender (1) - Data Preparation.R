# recommenderlab package to build game recommendation based on play data
# It's a convenient framework to evaluate and compare various recommendation algorithms 
# and quickly establish the best suited approach.
require(scales)
require(readxl)
require(tidyverse)
require(lubridate)
require(tidyquant)
require(ggplot2)
require(data.table)
require(dplyr)
require(plotly)
require(ggplot2)
require(stringr)
require(ggthemes)
require(ggfittext)
require(recommenderlab)
require(skimr)
require(treemap)

# Set Local Working Directly
setwd("C:/Users/bokhy/Desktop/ATG")
# From KPI Metrics cleaned weekly data

prod <- read.csv("Production Arcade Logs_new.csv")
prod <- prod %>% filter(!log_type == "AttractMode")
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


#positions <- c("4.11.0","4.11.1","4.12.0","4.13.0",
               #"4.14.0","4.14.1","4.16.0","4.17.0",
               #"4.18.0","4.19.0","4.20.0")
prod <- prod %>% 
  filter(!service == "AddOn") %>% 
  filter(!activity.platform == "Byog") %>% 
  filter(!activity.display_firmware == "4.15.0")

#  filter(activity.display_firmware %in% positions)   # Filter out 4.15.0 in Firmware version 
#  filter(!activity.display_firmware == "") # Filter out Blank


prod$activity.platform <- as.character(prod$activity.platform)
prod$service <- as.character(prod$service)

prod$activity.platform <- ifelse(prod$activity.platform == "", prod$service,prod$activity.platform)

prod$activity.platform[prod$activity.platform == 'BuildIn'] <- 'Built-in 350'
prod$activity.platform[prod$activity.platform == 'Byog'] <- 'BYOG'

table(prod$activity.platform)

prod$activity.platform <- as.factor(prod$activity.platform)

positions <- c("Built-in 350", "ArcadeNet")

prod <- prod %>% 
  mutate(Game = as.factor(activity.game_title)) %>% 
  mutate(State = as.factor(geoip.region_name)) %>% 
  mutate(Platform = as.factor(activity.platform)) %>% 
  mutate(Email = as.factor(account.email)) %>% 
  filter(Platform %in% positions) %>% 
  filter(Game != "") %>% 
  filter(Email != "") %>% 
  filter(!grepl('pc',Game, ignore.case = TRUE)) %>% 
  filter(!grepl('desk',Game, ignore.case = TRUE)) %>% 
  filter(!grepl('laptop',Game, ignore.case = TRUE)) %>%
  filter(!grepl('coin',Game, ignore.case = TRUE)) %>% 
  filter(!grepl('Steam',Game, ignore.case = TRUE))  %>%
  filter(!grepl('battlenet',Game, ignore.case = TRUE)) %>%
  filter(!grepl('origin',Game, ignore.case = TRUE)) %>%
  filter(!grepl('uplay',Game, ignore.case = TRUE)) %>% 
  filter(!grepl('cloud',Game, ignore.case = TRUE)) %>% 
  filter(!grepl('byog',Game, ignore.case = TRUE)) %>% 
  filter(!grepl('VR',Game, ignore.case = FALSE)) %>% 
  filter(!grepl('server',Game, ignore.case = TRUE)) %>% 
  filter(!grepl('computer',Game, ignore.case = TRUE)) %>% 
  filter(!grepl('mgr',Game, ignore.case = TRUE)) %>% 
  filter(!grepl('dell',Game, ignore.case = TRUE)) %>% 
  filter(!grepl('tv',Game, ignore.case = TRUE)) %>% 
  filter(!grepl('home',Game, ignore.case = TRUE)) %>% 
  filter(!grepl('window',Game, ignore.case = TRUE)) %>% 
  filter(!grepl('Epic',Game, ignore.case = TRUE)) %>% 
  filter(!grepl('basement',Game, ignore.case = TRUE)) %>% 
  filter(!grepl('zacs',Game, ignore.case = TRUE)) %>% 
  filter(!grepl('timsy',Game, ignore.case = TRUE)) %>% 
  filter(!grepl('Alienware',Game, ignore.case = TRUE)) %>% 
  filter(!grepl('CEO',Game, ignore.case = TRUE)) %>% 
  filter(!grepl('Surface',Game, ignore.case = TRUE)) %>% 
  filter(!grepl('Showtime',Game, ignore.case = TRUE)) %>% 
  filter(!grepl('Gafford',Game, ignore.case = TRUE)) %>% 
  filter(!grepl('Chocolate',Game, ignore.case = TRUE)) %>% 
  filter(!grepl('Bryan',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('Kentana',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('Sison',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('BGottschalk',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('Gator',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('Gaming-Rig',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('edev',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('DeathStar',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('envy',Game, ignore.case = TRUE)) %>% 
  filter(!grepl('Trident',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('Dan',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('Q32',Game, ignore.case = TRUE)) %>%
  filter(!grepl('Ryzen',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('krylz3',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('david',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('bennet',Game, ignore.case = TRUE)) %>%  
  filter(!grepl('Area51',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('TK421',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('Perrons',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('samsclub',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('walmart',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('NCC-1701-D',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('Klein',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('orion',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('JayC',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('jzmain',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('Facemaker',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('SuppaDuppa',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('TeeMax',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('Anti',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('ARTS-TOWER',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('arcade',Game, ignore.case = FALSE)) %>%   
  filter(!grepl('Stalfred',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('SF-LT-126',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('BoomstickBax',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('Stivers',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('main',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('dmm1138',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('BlackSite',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('MSI',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('SIM',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('Xerxes',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('Wookiee',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('Rod-Land',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('ALL in one',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('LGHTANDDRK',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('DevCan',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('ReadyPlayerOne',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('BALLA',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('ideapad',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('IT030',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('Will',Game, ignore.case = FALSE)) %>%   
  filter(!grepl('engle',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('Main',Game, ignore.case = FALSE)) %>%   
  filter(!grepl('GilsL',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('Hyuu',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('SATAN',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('Central',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('owner',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('tracker',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('Nelly',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('Nelson',Game, ignore.case = TRUE)) %>%   
  filter(!grepl('Mike',Game, ignore.case = FALSE))    
  

prod$Game <- as.character(prod$Game)
prod$Game[prod$Game == "Fix-It Felix, Jr."] <- "Fix-It Felix Jr."
prod$Game <- as.factor(prod$Game)  

data <- prod

# Pre-processing
data$activity.play_duration <- gsub(",","",data$activity.play_duration)
data <- transform(data, activity.play_duration = abs(as.numeric(activity.play_duration))) # Change negative value to positive (absolute)
nrow(data[data$activity.play_duration<0,]) # see if there are any negative record remaining
data$activity.play_duration <- (data$activity.play_duration)/3600

data <-  data %>% 
  filter(activity.play_duration < 12)


data <- data %>% separate(X.NAME.,
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
data$year <- as.factor(data$year)
data$hour <- as.factor(data$hour)
data$timeoftheday <- as.factor(data$timeoftheday)
data$Weekdays <- as.factor(data$Weekdays)

data_final <- data %>%
  select(machine_uuid, Email,Game, activity.play_duration,Platform,
         State,geoip.city_name,activity.display_firmware,geoip.timezone,
         date,Time,year,month,hour,Weekdays,timeoftheday) %>% 
  filter(!Game == "") %>% 
  mutate(Time = hms(Time))

glimpse(data_final)

data_final %>% skim()
write.csv(tmp , 'tmp.csv')

# Save dataset for later use
saveRDS(data_final, "C:/Users/bokhy/Documents/R-projects/Title_Recommender_with_market_basket_analysis/data_final.rds")
#write.csv(data_final, 'data_final.csv')

# Exploratory Data Analysis

# [1] What items do user buy most often?
data_final %>% 
  group_by(Game) %>% 
  summarize(total_hours = sum(activity.play_duration), count = n())%>%
  top_n(10, wt = total_hours) %>%
  arrange(desc(total_hours)) %>% 
  ggplot(aes(x = reorder(Game, total_hours), y = total_hours)) +
  geom_bar(stat = "identity", fill = "royalblue", colour = "blue") +
  labs(x = "", y = "Total Hours Played", title = "What titles do users play the most?") +
  coord_flip() +
  theme_grey(base_size = 12)

# [2] What time of day do people play more often (UTC)?
data_final %>% 
  group_by(hour) %>% 
  summarise(total_hours = sum(activity.play_duration)) %>% 
  ggplot(aes(x = hour, y = total_hours)) + 
  geom_bar(stat="identity", 
           position="identity", 
           fill="#E69F00",
           colour = "red") +
  labs(x = "Hour of Day (PST)", y = "Total Hours played",
       title = "What time of day do people play more often (PST)?") +
  theme_grey(base_size = 15)


# [3] What day of the week do people play more often?
data_final %>% 
  group_by(Weekdays) %>% 
  summarise(total_hours = sum(activity.play_duration)) %>% 
  ggplot(aes(x = Weekdays, y = total_hours)) + 
  geom_col(fill = "forest green", colour = "dark green") +
  labs(x = "Day of Week", y = "Total Hours Played", title = "What day of the week do people play more often?") +
  scale_x_discrete(labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")) +
  theme_grey(base_size = 14) +
  geom_text(aes(x= Weekdays, label=round(total_hours,0)), position=position_dodge(width=0.9), vjust=-0.25) 

# [4] Number of session per title / user

data_final %>% 
  group_by(Game) %>% 
  summarize(number_of_session_per_title = n()) %>% 
  ggplot(aes(number_of_session_per_title)) + 
  geom_bar(fill = "orange", color = "grey20", width = 1) + coord_cartesian(c(0,100))

data_final %>% 
  group_by(machine_uuid) %>% 
  summarize(total_play_time = sum(activity.play_duration), number_of_sessions_per_user = n()) %>% 
  ggplot(aes(number_of_sessions_per_user)) + 
  geom_bar(fill = "cadetblue3", color = "grey20") +
  labs(x = "Number of session per user", y = "Total Hours Played", title = "Number of session per user") +
  coord_cartesian(c(1, 100))

# [5] Users in which states played the most?
treemap(data_final %>% filter(!State == ""),
        index      = c("State"),
        vSize      = "activity.play_duration",
        title      = "",
        palette    = "Set2",
        border.col = "grey40")
