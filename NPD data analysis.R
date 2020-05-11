require(openxlsx)
require(tidyverse)
require(dplyr)
require(tidyr)
require(tidyquant)
require(reshape2)
require(ggplot2)
setwd("C:/Users/bokhy/Desktop/")
# Load Data
NPD <- read.xlsx("NPD_Sep_2017.xlsx", sheet = 1, startRow = 1, colNames = TRUE)
NPD <- NPD[,-c(87:89)]
glimpse(NPD)

# Change Excel-formatted date into Date 
colnames(NPD)[c(16:84)] <- as.character(as.Date(as.numeric(colnames(NPD[,c(16:84)])), origin = "1899-12-30"))
NPD$Introduction.Date <- as.character(as.Date(as.numeric(NPD$Introduction.Date), origin = "1899-12-30"))
head(NPD)

# Select out every third row to only extract average price 
NPD_new <- NPD[(seq(3,length(row.names(NPD)),3)),]


# Filter variables
NPD_new <- NPD_new[grepl("Full Game", NPD_new$`Digital.Type.-.Primary`),]
NPD_new <- NPD_new[!grepl("All Other", NPD_new$Item.Description),]

## For PC and PS4
NPD_new_PC <- NPD_new[grepl("PC", NPD_new$Platforms),]
NPD_new_PS4 <- NPD_new[grepl("PS4", NPD_new$Platforms),]


NPD_new_PC_tidy <- NPD_new_PC %>% gather(date,avg.price,16:84)
NPD_new_PS4_tidy <- NPD_new_PS4 %>% gather(date,avg.price,16:84)


NPD_new_PC_tidy_59 <- NPD_new_PC_tidy[NPD_new_PC_tidy$avg.price >= 59.99,]

write.csv(NPD_new_PC,file = "NPD_new_PC.csv")
write.csv(NPD_new_PC_tidy, file = "NPD_new_PC_tidy.csv")
write.csv(NPD_new_PS4_tidy, file = "NPD_new_PS4_tidy.csv")
write.csv(NPD_new_PC_tidy_59, file = "NPD_new_PC_tidy_59.csv")


## For PC 

# Pull out price change data 
for (i in length(row.names(NPD_new_PC))) {
  price <- NPD_new_PC[c(i),c(16:84)]
  data.frame(price)
}


new <- 

plot(NPD_new_PC$`Title.(Item.Rollup)`[3], NPD_new_PC[,16:84][7,])

a <- as.data.frame(t(NPD_new_PC[,16:84][3,]))

NPD_new_PC$`Title.(Item.Rollup)`[1]
ts.plot(NPD_new_PC[,16:84][7,])



NPD_new_PC$`Title.(Item.Rollup)[1:5,]

NPD_new_PC_tidy <- NPD_new_PC %>% gather(date,avg.price,16:84)
NPD_new_PC_tidy <- NPD_new_PC_tidy

NPD_new_PC_tidy %>% ggplot(aes(x = date, y = avg.price)) + geom_line() + facet_wrap(~ NPD_new_PC$Physical.or.Digital)


NPD_new_PC_tidy %>% subset(date = "2012-01-01")


NPD_new_PC_tidy %>% select(`Title.(Item.Rollup)`, date, avg.price) %>%  
  ggplot(aes(x= NPD_new_PC_tidy$Genre, y = NPD_new_PC_tidy$avg.price) + 
           geom_line()
         
         ggplot(NPD_new_PC_tidy, aes(x= NPD_new_PC_tidy$`Title.(Item.Rollup)`[1], y = NPD_new_PC_tidy$avg.price)) + 
           geom_line()
         NPD_new_PC_tidy$`Title.(Item.Rollup)`[1]
         
         
         ## For PS4
         
         
         
         
         ## +========================================================+ ##
         head(melt(NPD_new_PC, id.vars = "Item.Description", measure.vars = c("2012-01-01", "2012-02-01")))
         
         # Make Time-series
         head(NPD_new_PC[16])
         ggplot(NPD_new_PC, aes(x = ))