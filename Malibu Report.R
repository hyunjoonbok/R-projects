
# Set working directory
setwd("C:/Users/Benson/Desktop")
require(xlsx)
require(caret)
data <- read.xlsx("Phase2.xlsx",1)
data$total_time_played <-  gsub("1899-12-30 ","",data$total_time_played)

# Clean up the variables 
# Only grap generic usage
data <- data[!grepl("T'ai-pei", data$geoip.real_region_name),]
data <- data[!grepl("Minnesota", data$geoip.real_region_name),]
data <- data[!grepl("demo", data$session_node_name),]
data <- data[!grepl("demo", data$session_user_name),]
data <- data[!grepl("Nelson", data$session_user_name),]
data <- data[!grepl("f77a1d77", data$session_user_name),]
data <- data[!grepl("741fd86f", data$session_user_name),]
data <- data[!grepl("Kelvin", data$session_node_name),]
data <- data[!grepl("00:00:00", data$total_time_played),]
require(stringr)
ttime <- "[-.][0-9]{3}"
data$Time <- str_replace(data$Time,ttime, "")



# Labeling two different usages
data$session_service_type <- as.character(data$session_service_type)

for (i in (1:nrow(data))){
  if (grepl("instant",data$session_service_type)[i] == TRUE){
    data$session_service_type[i] <- "Instant_Play"
  }else data$session_service_type[i] <- "Cloud_Play"
}


## 1. 
# Finding out the # of times they started instance / total time spent for each person
require(dplyr)
Total_Time_Spent<- data %>% group_by(session_user_name, session_service_type, geoip.real_region_name) %>% summarise(n = n(),sum = sum(session_serving_time)) %>% arrange(desc(sum))


## 2.

# Cloud Play 
cloud <- data %>% filter(session_service_type == "Cloud_Play")
cloud <- cloud %>% group_by(session_user_name, Date) %>% summarise(n = n()) %>% arrange(desc(Date))

# In table
cloud %>% group_by(Date)%>%summarise(sum = sum(n)) 
ggplot(data =cloud, aes(x= cloud$Date, y = cloud$n)) + geom_count() +
  labs(title="Cloud Play" , x="Date", y = "# of instances started")

# Instant Play
instant <- data %>% filter(session_service_type == "Instant_Play")
instant <- instant %>% group_by(session_user_name, Date) %>% summarise(n = n()) %>% arrange(desc(Date))
# In table
instant %>% group_by(Date)%>%summarise(sum = sum(n)) 


# Change seconds into periods
require(hms)
Total_Time_Spent$time <- as.hms(Total_Time_Spent$sum)

# By Location
table(droplevels(Total_Time_Spent$geoip.real_region_name))

# 
data$Time <- as.hms(data$Time)
ggplot(data=data , aes(x = data$session_user_name, y = data$Time)) + geom_point()




# Graphical representation

# Clearing outliers with coordinate limitation
# and removing admin_user 
require(ggplot2)
ggplot(data = Total_Time_Spent, aes(x = Total_Time_Spent$n, y = Total_Time_Spent$time),outlier.shape=NA) +
  geom_count(size=2, shape = 24, fill = "red", color = "black") + theme_bw(base_family = "Helvetica") + geom_smooth(method = "loess" ,se = FALSE) +
  labs(title="Total User Graph" , x="# of times instance started", y = "Total Playing time") +
  coord_cartesian(xlim = NULL, ylim = c(0,20000), expand = TRUE)

# By Location
table(droplevels(Total_Time_Spent$geoip.real_region_name))

# Playing time-hour
data$Time <- as.hms(data$Time)
ggplot(data=data , aes(x = data$session_user_name, y = data$Time)) + geom_point() +
  labs(title = "Hour of the day", x= "User", y = "Time") + coord_cartesian(xlim = NULL, ylim = NULL, expand = TRUE)





# Write in .csv file
write.csv(data, file = "Cloudplay_Streaming_Analysis")
sum(Total_Time_Spent$time)

