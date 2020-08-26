# Market Basket Analyis

# Association between games played
library(arules)
library(arulesViz)
library(tidyverse)
library(readxl)
library(knitr)
library(lubridate)
library(RColorBrewer)

# Set working directory
setwd("C:/Users/bokhy/Desktop/")

# Read data
prod <- read.csv("production_new.csv")

# Data pre-processing
prod <- prod %>% separate(activity.play_start,
                          c("Date","a"), sep = '@')

prod$Time <- gsub("\\..*","",prod$a)

prod$Date <- as.Date(prod$Date, "%B %d, %Y")

prod$Weekdays <- weekdays(prod$Date)

prod$hour <- as.numeric(gsub("\\:.*$", "", prod$Time))
prod$timeoftheday<- with(prod, ifelse(hour >= 5 & hour<=11, "morning",
                                      ifelse(hour>11 & hour<=16, "afternoon",
                                             ifelse(hour>16 & hour<=21, "evening" ,"night"))))
prod$a <- NULL
prod <- prod %>% select(Date,Time,Weekdays,hour,timeoftheday, everything())


positions <- c("Built-in 350", "ArcadeNet")
prod <- prod %>% 
  mutate(Game = as.factor(activity.game_title)) %>% 
  mutate(State = as.factor(geoip.region_name)) %>% 
  mutate(Platform = as.factor(activity.platform)) %>% 
  mutate(Email = as.factor(account.email)) %>% 
  mutate(hour = as.factor(hour)) %>%
  mutate(timeoftheday = as.factor(timeoftheday)) %>%
  filter(Platform %in% positions) %>% 
  filter(Game != "") %>% 
  filter(Email != "") %>% 
  filter(!grepl('pc',Game, ignore.case = TRUE)) %>% 
  filter(!grepl('desk',Game, ignore.case = TRUE)) %>% 
  filter(!grepl('laptop',Game, ignore.case = TRUE)) %>%
  filter(!grepl('coin',Game, ignore.case = TRUE)) 

prod$Game <- as.character(prod$Game)
prod$Game[prod$Game == "Fix-It Felix, Jr."] <- "Fix-It Felix Jr."
prod$Game <- as.factor(prod$Game)  
  


# Change dataset into basket format
# combine all products from one e-mail and date into one row 
transactionData <- ddply(prod,c("Email","Date"), 
                         function(df1)paste(df1$Game,collapse = ","))  
# not-using columns
transactionData$Email <- NULL
transactionData$Date <- NULL

#Rename column to items
colnames(transactionData) <- c("items")

# Save and read the file
write.csv(transactionData,"market_basket_transactions.csv", quote = FALSE, row.names = FALSE)
tr <- read.transactions('market_basket_transactions.csv', rm.duplicates= TRUE, format = 'basket', sep=',', cols = NULL, skip = 1)
# tr@itemInfo$labels <- gsub("\"","",tr@itemInfo$labels)

summary(tr)
inspect(tr[1:10])
# Density tells the percentage of non-zero cells in a sparse matrix
# 22191x7876x0.001930725 (density)
itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
# type can be type="absolute" or type="relative"


# mine the rules using the APRIORI algorithm!
# Min Support as 0.001, confidence as 0.8.
# The default values  are minimum support of 0.1, the minimum confidence of 0.8, maximum of 10 items (maxlen)
association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8, maxlen=4))
# Remove redundant rule    
association.rules <- association.rules[!is.redundant(association.rules)]

# Lets say we wanted to have the most likely rules. 
# We can easily sort by confidence by executing the following code.
association.rules <- sort(association.rules, by="confidence", decreasing=TRUE)
# or 
association.rules <- sort(association.rules, by="lift", decreasing=TRUE)


## === Not Using === ##
subset.rules <- which(colSums(is.subset(association.rules, association.rules)) > 1) # get subset rules in vector
length(subset.rules)
subset.association.rules <- association.rules[-subset.rules] # remove subset rules.
## === Not Using === ##


# Support: The fraction of which our item set occurs in our dataset.
# Confidence: probability that a rule is correct for a new transaction with items on the left.
# (if confience is 1, if LHS is done, they are 100% likely that RHS is done)
# Lift: The ratio by which by the confidence of a rule exceeds the expected confidence.
# (how many times LHS and RHS are likely to be purchased together, compared to treated separtely)
# (if Lift is 1, they are independent)

# look at summary
summary(association.rules)

# Print top 10
inspect(association.rules[1:10])

# Find relationship of speficic games
# (1)
# For example, to find what customers play before 'Centipede'
association.rules_sample1 <- apriori(tr, parameter = list(supp=0.001, conf=0.8),appearance = list(default="lhs",rhs="Centipede¢ç (Arcade)"))
inspect(head(association.rules_sample1))
inspect(association.rules_sample1)
# (2)
# or Customers who played Burgertime also played...
# Set the confidence to 0.15 since we get no rules with 0.8
# Set a minimum length of 2 to avoid empty left hand side items
association.rules_sample2 <- apriori(tr, parameter = list(supp=0.001, conf=0.1, maxlen=2),appearance = list(lhs="BurgerTime¢â (Arcade)",default="rhs"))
inspect(head(association.rules_sample2))

write(association.rules,
      file = "association_rules.csv",
      sep = ",",
      quote = TRUE,
      row.names = FALSE)

# Make pretty table
association_data <- read.csv('association_rules.csv')

require(reactable)
reactable(association_data, filterable = TRUE, minRows = 10)






# Visualization
# Filter rules with confidence greater than 0.4 or 40%
subRules<-association.rules[quality(association.rules)$confidence>0.5]

# 0.
plot(association.rules, method = "grouped", control = list(k = 5))
plot(association.rules, method="graph", control=list(type="items"))
plot(association.rules, method="paracoord",  control=list(alpha=.5, reorder=TRUE))
plot(association.rules,measure=c("support","lift"),shading="confidence",interactive=T)

# 1.
plotly_arules(subRules)

# 2. 
# Select 10 rules from subRules having the highest confidence.
top10subRules <- head(subRules, n = 10, by = "confidence")
plot(top10subRules, method = "graph",  engine = "htmlwidget")

# 3. 
# As mentioned above, the RHS is the Consequent or the item we propose the customer will buy; 
# the positions are in the LHS where 2 is the most recent addition to our basket 
# and 1 is the item we previously had.
# Filter top 20 rules with highest lift
subRules2<-head(subRules, n=20, by="lift")
plot(subRules2, method="paracoord")


# Save graph. For example, the 1000 rules with the highest lift are exported by:
saveAsGraph(head(subRules, n = 1000, by = "lift"), file = "rules.graphml")

