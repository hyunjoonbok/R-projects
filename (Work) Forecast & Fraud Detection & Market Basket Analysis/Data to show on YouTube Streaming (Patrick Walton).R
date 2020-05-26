#### Data to show on YouTube Streaming (Patrick Walton) ####

# Load Pacakages
require(tidyverse)
require(lubridate)
require(purrr)
require(readxl)
require(tidyquant)
require(ggplot2)
require(data.table)
require(dplyr)
require(plotly)
require(ggplot2)
require(stringr)
library(ggthemes)
require(scales)

# Data Pre-Processing 
setwd("C:/Users/bokhy/Desktop")
prod <- read.csv("Production Arcade Logs.csv") 
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

## Exclude 4.15.0 and people before 4.11.0?
prod <- prod %>% 
  filter(!activity.display_firmware == "4.15.0") %>% 
  filter(!activity.display_firmware == "") 

prod$activity.platform <- as.character(prod$activity.platform)
prod$service <- as.character(prod$service)
prod$activity.platform <- ifelse(prod$activity.platform == "", prod$service,prod$activity.platform)
prod$activity.platform[prod$activity.platform == 'BuildIn'] <- 'Built-in 350'
prod$activity.platform[prod$activity.platform == 'Byog'] <- 'BYOG'

table(prod$activity.platform)

prod$activity.platform <- as.factor(prod$activity.platform)
data <- prod

data$activity.play_duration <- gsub(",","",data$activity.play_duration)
data <- transform(data, activity.play_duration = abs(as.numeric(activity.play_duration))) # Change negative value to positive (absolute)
data$activity.1p_spinner <- gsub(",","",data$activity.1p_spinner)
data$activity.2p_spinner <- gsub(",","",data$activity.2p_spinner)
data$activity.1p_joystick <- gsub(",","",data$activity.1p_joystick)
data$activity.1p_buttons <- gsub(",","",data$activity.1p_buttons)
data$activity.2p_joystick <- gsub(",","",data$activity.2p_joystick)
data$activity.2p_buttons <- gsub(",","",data$activity.2p_buttons)
data$activity.trackball <- gsub(",","",data$activity.trackball)

data$activity.1p_spinner <- as.numeric(data$activity.1p_spinner)
data$activity.2p_spinner <- as.numeric(data$activity.2p_spinner)
data$activity.1p_joystick <- as.numeric(data$activity.1p_joystick)
data$activity.1p_buttons <- as.numeric(data$activity.1p_buttons)
data$activity.2p_joystick <- as.numeric(data$activity.2p_joystick)
data$activity.2p_buttons <- as.numeric(data$activity.2p_buttons)
data$activity.trackball <- as.numeric(data$activity.trackball)

nrow(data[data$activity.play_duration<0,]) # see if there are any negative record remaining
data$activity.play_duration <- (data$activity.play_duration)/3600

data <- data %>% mutate(activity.trackball = coalesce(activity.trackball, 0))

# Exclude data that's over 12hr recorded in a single log
data <- data %>% filter(activity.play_duration < 12.0)


# 1. Top 10 Most Played
data %>% 
  filter(!activity.platform == "AddOn") %>% 
  filter(!activity.platform == "BYOG") %>% 
  select(machine_uuid, activity.platform, activity.play_duration, activity.game_title, activity.play_start, activity.play_end) %>% 
  distinct() %>% # remove dupplicate
  filter(!activity.game_title == "") %>% 
  group_by(activity.platform, activity.game_title) %>% 
  summarise(total_hours = round(sum(activity.play_duration),1)) %>% 
  select(activity.game_title,activity.platform, total_hours) %>% 
  arrange(desc(total_hours)) %>% 
  mutate(percentage = percent(prop.table(total_hours))) %>% 
  slice(1:10) %>% 
  rename("Title" = activity.game_title, "Service" = activity.platform, 
         "Playtime(hrs)" = total_hours, "%_of_total" = percentage)


### == Not giving this metric == ###
# 2. Top 10 Most Active User (Super Users of the Week)
data %>% 
  filter(!activity.platform == "AddOn") %>% 
  filter(!activity.platform == "BYOG") %>% 
  select(machine_uuid, account.email, activity.play_duration, activity.game_title) %>% 
  distinct() %>%
  group_by(machine_uuid) %>% 
  summarise(total_hours = round(sum(activity.play_duration),1)) %>% 
  arrange(desc(total_hours)) %>%
  slice(1:10)
### == Not giving this metric == ###


# 3. Top 10 Games by Input
# (Use the fimware above 4.17.0 -- Trackball fixed)
positions <- c("4.17.0", "4.18.0","4.19.0","4.20.0")

c <- data %>% 
  filter(activity.display_firmware %in% positions) %>% 
  filter(!activity.platform == "AddOn") %>% 
  filter(!activity.platform == "BYOG") %>% 
  filter(!activity.1p_buttons == "") %>% 
  filter(!activity.game_title == "") %>% 
  distinct() %>% 
  select(machine_uuid, activity.game_title, activity.play_duration, 
         activity.1p_spinner, activity.2p_spinner,activity.trackball)%>% 
  group_by(activity.game_title) %>% 
  summarise(total_hours = round(sum(activity.play_duration),1),
            total_spinner = sum(activity.1p_spinner,activity.2p_spinner),
            total_trackball = sum(activity.trackball))
  
## (1) By Spinner
c %>% arrange(desc(total_spinner)) %>%
  slice(1:5)

## (2) By Trackball
c %>% arrange(desc(total_trackball)) %>%
  slice(1:5)


# 4. Most-Played 2nd Player game
d <- data %>% 
  filter(!activity.platform == "AddOn") %>% 
  filter(!activity.platform == "BYOG") %>% 
  filter(!activity.game_title == "") %>% 
  filter(!activity.2p_buttons == 0) %>% 
  filter(!activity.2p_buttons == "") %>%   
  distinct() %>% 
  select(machine_uuid, activity.game_title, activity.play_duration, 
         activity.2p_buttons, activity.2p_spinner, activity.2p_joystick) %>% 
  group_by(activity.game_title) %>% 
  summarise(total_hours = round(sum(activity.play_duration),1),
            total_2P_buttons = sum(activity.2p_buttons),
            total_2P_spinner = sum(activity.2p_spinner),
            total_2P_joystick = sum(activity.2p_joystick)) %>% 
  arrange(desc(total_2P_joystick)) 
d

# 5. Top 10 State
data %>% 
  filter(!activity.platform == "AddOn") %>% 
  filter(!activity.platform == "BYOG") %>% 
  distinct() %>% 
  select(geoip.region_name, machine_uuid, activity.game_title, activity.play_duration) %>% 
  group_by(geoip.region_name) %>% 
  summarise(total_hours = round(sum(activity.play_duration),1)) %>% 
  arrange(desc(total_hours))

# 6. Top 10 City  
data %>% 
  filter(!activity.platform == "AddOn") %>% 
  filter(!activity.platform == "BYOG") %>% 
  distinct() %>% 
  select(geoip.city_name, geoip.region_name,machine_uuid, activity.game_title, activity.play_duration) %>% 
  group_by(geoip.city_name,geoip.region_name) %>% 
  summarise(total_hours = round(sum(activity.play_duration),1)) %>% 
  arrange(desc(total_hours))
