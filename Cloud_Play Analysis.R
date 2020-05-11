# Setting-up Environment
setwd("C:/Users/Benson/Desktop")
require(xlsx)
dd <- read.xlsx("dd.xlsx",1)
timestamp(dd$Created..UTC.)

# Cleaning the Datetime variable
dd$Created..UTC.<- gsub("UTC","",dd$Created..UTC.)
dd$Finished..UTC. <- gsub("UTC","",dd$Finished..UTC.)

# Chaning strings into date objects
Start <- strptime(dd$Created..UTC.,"%Y-%m-%d %H:%M:%S", tz = "UTC")
End <- strptime(dd$Finished..UTC.,"%Y-%m-%d %H:%M:%S", tz = "UTC" )

# Calculating time difference
Timediff <- difftime(End, Start, units = c("mins"))
Timediff_new <- round(as.numeric(Timediff),2)

#Adding a new variable 
dd$Timediff_min <- Timediff_new

# Finding out the # of times they started instance / total time spent for each person
require(dplyr)
Total_Time_Spent<- dd %>% group_by(as.numeric(Session.Node.Id)) %>% summarise(n = n(),sum = sum(Timediff_min)) %>% arrange(desc(sum))

# Chaning the variable name and write in .csv file
colnames(Total_Time_Spent)[1] <- "User_Session_ID"
colnames(Total_Time_Spent)[2] <- "times instances started"
colnames(Total_Time_Spent)[3] <- "Total Time Played in Minute"
write.csv(dd, file = "Cloudplay_User_Analysis")
