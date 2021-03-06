require(tidyverse)
require(lubridate)
require(purrr)
require(readxl)
require(tidyquant)
require(ggplot2)
require(hrbrthemes)
require(data.table)
require(dplyr)
require(plotly)
#require(bbplot)
require(stringr)
library(ggthemes)
require(scales)
setwd("C:/Users/bokhy/Desktop/")
prod <- read.csv("Production Arcade Logs.csv") 

# Remove duplicates
prod <- prod %>% distinct(activity.play_start, activity.play_end, activity.play_duration, .keep_all= TRUE)

# write.csv(prod, 'Production Arcade Logs1.csv')
# prod <- read.csv("Production Arcade Logs.csv") 

# 1. Pre-processing ====
# Adding "service" column

prod <- prod %>% 
  filter(!log_type == "AttractMode") %>% 
  filter(!log_type == "ConnectDevice") %>% 
  filter(!log_type == "Notification") 
  
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


## We are excluding 4.15.0 and people before 4.11.0 ##
prod <- prod %>% 
  filter(!activity.display_firmware == "4.15.0")%>% # Filter out 4.15.0 in Firmware version 
  filter(!activity.display_firmware == "") # Filter out Blank

## Title formatting
prod$activity.game_title <- gsub("\\??","",prod$activity.game_title)
prod$activity.game_title <- gsub("\\?","",prod$activity.game_title)
prod$activity.game_title <- gsub("\\®","",prod$activity.game_title)
prod$activity.game_title <- gsub("[[:blank:]]","",prod$activity.game_title)

prod$activity.platform <- as.character(prod$activity.platform)
prod$service <- as.character(prod$service)

prod$activity.platform <- ifelse(prod$activity.platform == "", prod$service,prod$activity.platform)

prod$activity.platform[prod$activity.platform == 'BuildIn'] <- 'Built-in 350'
prod$activity.platform[prod$activity.platform == 'Byog'] <- 'BYOG'

table(prod$activity.platform)

prod$activity.platform <- as.factor(prod$activity.platform)

data <- prod

write.csv(data, "weekly_cleaned.csv")

## ============================ ##

#==== Class 1 ====#

setwd("C:/Users/bokhy/Desktop/Python/Python-Projects/ATG_work")
fw_count <- read.csv("fw_count.csv") ## Get file from Python ##
glimpse(fw_count)

# Add positions each release
positions <- c("4.1.0", "4.2.0","4.3.0","4.4.0","4.5.0","4.6.0",
               "4.7.0","4.8.0","4.9.0","4.10.0","4.11.0","4.11.1",
               "4.12.0","4.13.0","4.14.0","4.14.1","4.15.0","4.16.0",
               "4.17.0","4.18.0","4.19.0","4.20.0","4.21.0","4.22.0",
               "4.23.0","4.24.0","4.25.0","4.26.0","4.26.1","4.27.0",
               "4.28.0","4.29.0","4.30.0","4.31.0","4.32.0","4.33.0",
               "4.34.0")

# 4.1.0 and over count
temp <- fw_count %>% 
  filter(display_version_number %in% positions) %>% 
  group_by(display_version_number) %>%
  tally()
sum(temp$n)  

# Update success count
temp2 <- fw_count %>%
  group_by(display_version_number) %>% 
  filter(State == "Success") %>% # Verify other Status 
  filter(display_version_number != "") %>% 
  tally()
sum(temp2$n)

# Latest Firmware count (2)
sum(temp2$n[temp2$display_version_number %in% tail(positions,2)])  
# others
sum(temp2$n[temp2$display_version_number %in% tail(positions,3)]) 
sum(temp2$n[temp2$display_version_number %in% tail(positions,4)]) 
sum(temp2$n[temp2$display_version_number %in% tail(positions,5)]) 

table(fw_count$State)
fw_count$State[fw_count$State == "DOWNLOADED"] <- "CHECKING"
fw_count$State[fw_count$State == "CHECKING"] <- "Failed"
fw_count$State[fw_count$State == "Success"] <- "Updated"

# All Users Firmware Version Count 
p <- fw_count %>%
  filter(display_version_number %in% positions) %>%
  group_by(display_version_number) %>% 
  filter(display_version_number != "") %>% 
  droplevels() %>% 
  ggplot(aes(x = display_version_number, fill = State)) +
  geom_bar(colour="black", stat = 'count') +
  scale_x_discrete(limits = positions) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#  coord_flip() +
  labs(x = "Firmware Version", y = "User Count")
p

# Date Taken to the firmware update
# Early adaptors' firmware term vs 'non-early adptors' firmware term 
# (need to find the user who updates often)
# (We could have a differen strategy to approach to two groups)

fw_count$date_taken_date <- substr(fw_count$date_taken, 1, 2)
fw_count <- transform(fw_count, date_taken_date = as.numeric(date_taken_date))
fw_count$early_adopt <- ifelse(fw_count$display_version_number %in% tail(positions,2), "Early Adopter", "Non Early-Adopter" )


barlines <- "#1F3552"

p3 <- fw_count %>%
  mutate(date_taken_date = factor(date_taken_date)) %>% 
  drop_na(date_taken_date) %>% 
  ggplot(aes(x = date_taken_date, fill = early_adopt)) + 
  geom_histogram(stat='count', colour = barlines, alpha=.9, width=.7) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
#  theme_economist() +
  labs(x = "Days taken", y = "User Count",
       subtitle = "How long does the user take to update the firmware after its release? ") +
  stat_count(color = "black", geom="text", aes(label=..count..),  position=position_stack(vjust = 0.5))

p3
sum(fw_count$date_taken_date)
table(fw_count$date_taken_date)

#==== Class 2 ====#

# From above cleaned weekly data

# Pre-processing
data$activity.play_duration <- gsub(",","",data$activity.play_duration)
data <- transform(data, activity.play_duration = abs(as.numeric(activity.play_duration))) # Change negative value to positive (absolute)
nrow(data[data$activity.play_duration<0,]) # see if there are any negative record remaining
data$activity.play_duration <- (data$activity.play_duration)/3600


# Weekly Total Playtime
a <- data %>% 
  filter(activity.play_duration < 12) %>% 
  distinct(activity.play_duration, .keep_all = TRUE) %>% 
  group_by(machine_uuid, activity.platform) %>% 
  summarise(total_hours = sum(activity.play_duration)) %>% 
  arrange(desc(total_hours))
sum(a$total_hours)

# Weekly Playtime per service
a %>% group_by(activity.platform) %>% summarise(Playtimeperservice = sum(total_hours)) %>% arrange(desc(Playtimeperservice))

# Weekly User Count
a %>% count(machine_uuid)

# Weekly User Count per service
a %>% group_by(activity.platform) %>% tally()

# Cumulative Unique User
setwd("C:/Users/bokhy/Desktop/ATG")
c <- read.csv('Production Arcade Logs_new.csv')
c %>% group_by(machine_uuid) %>% count()

# Top 10 Titles 
# make sure to filter out outliers #
data %>% 
  filter(!machine_uuid == "88001911B0017348") %>% 
  group_by(activity.game_title) %>% 
  filter(activity.platform == 'Built-in 350') %>% 
  filter(activity.play_duration < 12) %>%
  summarise(total_hours = sum(activity.play_duration)) %>% 
  select(activity.game_title, total_hours) %>% 
  arrange(desc(total_hours)) %>% 
  slice(1:10) %>% 
  ggplot(aes(x = reorder(activity.game_title,total_hours) , y = total_hours)) +
  geom_bar(stat="identity", 
           position="identity", 
           fill="#E69F00") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") + 
  coord_flip() +
  labs(x= "Title", y = "Hours Played") +
  theme_classic()


# Weekly All users
p4 <- b %>% ggplot(aes(x = reorder(machine_uuid, -total_hours), y = total_hours)) +
  geom_bar(stat="identity", 
           position="identity", 
           fill="#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(title="Weekly Total Users ",
       subtitle = "Hours spent for each user",
       x = "All User", y = "Hours Spent") 
p4

# Weekly Top 10 heavy user
p5 <- data %>% 
  mutate(make_lumped = fct_lump(machine_uuid,10)) %>%  
  group_by(make_lumped) %>% 
  summarise(total_hours = sum(activity.play_duration)) 
p5[-nrow(p5),] %>%  ggplot(aes(x = reorder(make_lumped, -total_hours), y = total_hours)) +
  geom_bar(stat="identity", 
           position="identity", 
           fill="#1380A1") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  theme(axis.text.x = element_text(angle=80, vjust=0.6),
        axis.ticks.x=element_blank()) +
  labs(title="Weekly Heavy Users",
       x = "Heavy User", y = "Hours Spent")
p5



#==== Leaderboard ====#

# Load the master logs
# c <- read.csv('Production Arcade Logs_new.csv')

# leaderboard competition participant
setwd("C:/Users/bokhy/Desktop/Python/Python-Projects/ATG_work")
d <- read.csv('account.csv')

d$opted_in <- purrr::map_dfr(d, ~ .x %in% c$account.email)

# by email
d_1 <- d %>% 
  # Check for specific competition
  filter(name == "Apocalypse II, D-Day, Rod-Land, and Pinbo competition") %>% 
  group_by(email) %>% 
  count(opted_in$email)
# by Initials
d_2 <- d %>% 
  # Check for specific competition
  filter(name == "Apocalypse II, D-Day, Rod-Land, and Pinbo competition") %>% 
  group_by(signature,email) %>% 
  count(opted_in$email)



# Weekly Leaderboard Competition Total Entry
sum(d_2$n)

# Weekly Leaderboard Competition Participant (by email)
nrow(d_1)

# Weekly Leaderboard Competition Participant (by Initials)
nrow(d_2)

# Opted-in users out of weekly participant
nrow(d_1 %>% filter(`opted_in$email` == TRUE))

## === Optional == ##
# Participant for each game
# for selected competition
d %>% 
  filter(name == "Crystal Castles, Darwin 4078, Midnight Resistance, and Millipede Competition") %>%
  group_by(name, GAME) %>% 
  count()


#==== Usage Buckets ====#

setwd("C:/Users/bokhy/Desktop/ATG")
t <- read.csv("Production Arcade Logs_new.csv")
head(t)

# ArcadeNet Bin (Each bin is 30 min)
t %>% filter(activity.platform =="ArcadeNet") %>% arrange(desc(total_hours)) %>%
  ggplot(aes(x = total_hours)) +
  geom_histogram(binwidth = 0.5, fill = "#69b3a2",color = "#e9ecef" , position = "identity") +
  scale_color_brewer(palette = "Blues") +
  labs(title= "ArcadeNet Usage", y = "User Count", x = "Hours played", subtitle = "Each bin represents 30 min") +
  bbc_style()
# BYOG Bin
t %>% filter(activity.platform =="BYOG") %>% arrange(desc(total_hours)) %>%
  ggplot(aes(x = total_hours)) +
  geom_histogram(binwidth = 0.5, fill = "#69b3a2",color = "#e9ecef" , position = "identity") +
  scale_color_brewer(palette = "Blues") +
  bbc_style() +
  labs(title= "BYOG Usage", y = "User Count", x = "Hours played", subtitle = "Each bin represents 30 min")

#filter out outliers
t %>% filter(activity.platform =="Built-in 350") %>% filter(total_hours < 60) %>% arrange(desc(total_hours)) %>%  
  ggplot(aes(x = total_hours)) +
  geom_histogram(binwidth = 0.5, fill = "#69b3a2",color = "#e9ecef" , position = "identity") +
  scale_color_brewer(palette = "Blues") +
  bbc_style() +
  labs(title= "Built-in 350 Usage", x = "Hours played",y = "User Count", subtitle = "Each bin represents 30 min")
# Highest in 60, most in less than 30 range
# 95$ in less than 10 hours total