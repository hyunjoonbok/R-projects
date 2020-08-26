#### User Spending Bucket code ####

## Purpose: To find a grouping of users in a certain $ bucket we preset

# Enviornment
setwd("C:/Users/Benson/Desktop")
require(openxlsx)
US = read.csv("us-customers-active_new.csv", stringsAsFactors = FALSE)
UK_EU = read.csv("uk_eu-customers-active_new.csv", stringsAsFactors = FALSE)

# Setting up
for (i in (1:nrow(US))){
  if (US$Amount[i] == "#N/A"){
    US$Amount[i] <- 0
  }
}
for (i in (1:nrow(UK_EU))){
  if (UK_EU$Amount[i] == "#N/A"){
  UK_EU$Amount[i] <- 0
  }
}

# Make values numeric
US$Amount = as.numeric(US$Amount)
UK_EU$Amount = as.numeric(UK_EU$Amount)


## Pre-set Amount Bucket for US
for (i in (1:nrow(US))){
  if(US$Amount[i] < 9.99){
    US$Tier[i] <- "D2D"
  }else if( 9.99 >= US$Amount[i] & US$Amount[i] < 19.99){
    US$Tier[i] <- "Bronze"
  }else if( 19.99 >= US$Amount[i] & US$Amount[i]< 29.99){
    US$Tier[i] <- "Silver"
  }else if( 29.99 >= US$Amount[i] & US$Amount[i]< 59.99){
    US$Tier[i] <- "Gold"
  }else if(59.99 >= US$Amount[i] & US$Amount[i]){
    US$Tier[i] <- "Platinum"
  }else US$Tier[i] <- "None"
}

## Pre-set Amount Bucket for US UK_EU
for (i in (1:nrow(UK_EU))){
  if(UK_EU$Amount[i] < 9.99){
    UK_EU$Tier[i] <- "D2D"
  }else if( 9.99 >= UK_EU$Amount[i] & UK_EU$Amount[i] < 19.99){
    UK_EU$Tier[i] <- "Bronze"
  }else if( 19.99 >= UK_EU$Amount[i] & UK_EU$Amount[i]< 29.99){
    UK_EU$Tier[i] <- "Silver"
  }else if( 29.99 >= UK_EU$Amount[i] & UK_EU$Amount[i]< 59.99){
    UK_EU$Tier[i] <- "Gold"
  }else if(59.99 >= UK_EU$Amount[i] & UK_EU$Amount[i]){
    UK_EU$Tier[i] <- "Platinum"
  }else UK_EU$Tier[i] <- "None"
}


## Tier Division for US
for (i in (1:nrow(US))){
  if(US$Tier[i] == "D2D"){
    US$Discount[i] <- 5
  }else if( US$Tier[i] =="Bronze"){
    US$Discount[i] <- 8
  }else if( US$Tier[i] =="Silver"){
    US$Discount[i] <- 12
  }else if( US$Tier[i] =="Gold"){
    US$Discount[i] <- 15
  }else if(US$Tier[i] =="Platinum"){
    US$Discount[i] <- 20
  }else US$Discount[i] <- 0
}

## Tier Division for UK_EU
for (i in (1:nrow(UK_EU))){
  if(UK_EU$Tier[i] == "D2D"){
    UK_EU$Discount[i] <- 5
  }else if( UK_EU$Tier[i] =="Bronze"){
    UK_EU$Discount[i] <- 8
  }else if( UK_EU$Tier[i] =="Silver"){
    UK_EU$Discount[i] <- 12
  }else if( UK_EU$Tier[i] =="Gold"){
    UK_EU$Discount[i] <- 15
  }else if(UK_EU$Tier[i] =="Platinum"){
    UK_EU$Discount[i] <- 20
  }else UK_EU$Discount[i] <- 0
}

# Write in .csv file
write.csv(US, file = "US_customers.csv", row.names = TRUE)
write.csv(UK_EU, file = "UK_EU_customers.csv", row.names = TRUE)
