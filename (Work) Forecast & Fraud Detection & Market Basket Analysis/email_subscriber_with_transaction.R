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

setwd("C:/Users/bokhy/Desktop/ATG/")

transaction <- read_csv("ddd.csv")
us_email <- read_csv("us-customers(gdpr)-all.csv")
uk_eu_email <- read_csv("uk_eu-customers(gdpr)-all.csv")
d <- read.csv("employee.csv")


# transaction -------------------------------------------------------------

transaction$order_state[transaction$order_state == "CHARGED_BACK"] <- "REFUNDED"
transaction$order_state[transaction$order_state == "DUPLICATED_REFUND"] <- "REFUNDED"
transaction$order_state[transaction$order_state == "DUPLICATED_REFUNDED"] <- "REFUNDED"
transaction$order_state[transaction$order_state == "SUBMITTED"] <- "CHARGED"

transaction$currency_full_name[transaction$currency_full_name == "Euro Member Countries"] <- "EUR"
transaction$currency_full_name[transaction$currency_full_name == "United Kingdom Pound"] <- "GBP"
transaction$currency_full_name[transaction$currency_full_name == "United States Dollar"] <- "USD"

transaction$company_name[transaction$company_name == "1C Company"] <- "1C Entertainment"
transaction$company_name[transaction$company_name == "Axis Game Factory, Inc."] <- "Axis Game Factory LLC"
transaction$company_name[transaction$company_name == "Electronic Arts UK"] <- "Electronic Arts"
transaction$company_name[transaction$company_name == "Koei Tecmo"] <- "Koei"
transaction$company_name[transaction$company_name == "Private Division"] <- "TakeTwo Private Division"
transaction$company_name[transaction$company_name == "THQ Nordic"] <- "THQ Nordic GmbH"
transaction$company_name[transaction$company_name == "Ubisoft Entertainment"] <- "Ubisoft"
transaction$company_name[transaction$company_name == "Team 17"] <- "Team17"


transaction$payment_date <- as.POSIXct(strptime(transaction$payment_date, "%Y-%m-%d %H:%M:%S"))

# Filter-out incomplete transactions
transaction <- transaction %>% 
  # Only past 1 year transactions
  filter(between(payment_date, as.Date("2019-06-15"), as.Date("2020-06-15"))) %>% 
  # incomplete transactions
  filter(order_state == 'CHARGED' | order_state == 'REFUNDED') %>% 
  filter(!company_name == 'AtGames Test Publisher 1') %>% 
  filter(!company_name == 'AtGames Test Publisher 2') %>% 
  filter(!company_name == 'AtGames Test Publisher 3') %>% 
  filter(!company_name == 'Ravenscourt - DUPLICATE') %>% 
  select(payment_date, title, company_name, email, currency_full_name, country_full_name,
         msrp, order_state, type, gross_revenue, vip_discounts)

# Create uniform title name for all regions (Remove UK,EU specification)
transaction$title <- gsub("\\â„¢","",transaction$title)
transaction$title <- gsub("\\??","",transaction$title)
transaction$title <- gsub("\\{EU}","",transaction$title)
transaction$title <- gsub("\\{UK}","",transaction$title)
transaction$title <- gsub("\\®","",transaction$title)
transaction$title <- gsub("[[:blank:]]","",transaction$title)

transaction$payment_Year <- as.factor(lubridate::year(transaction$payment_date))
transaction$payment_Quarter <- as.factor(lubridate::quarter(transaction$payment_date))
transaction$payment_Month <- as.factor(lubridate::month(transaction$payment_date))

# Factorize a few other variables
transaction$title <- as.factor(transaction$title)
transaction$company_name <- as.factor(transaction$company_name)
transaction$currency_full_name <- as.factor(transaction$currency_full_name)
transaction$country_full_name <- as.factor(transaction$country_full_name)
transaction$order_state <- as.factor(transaction$order_state)
transaction$type <- as.factor(transaction$type)
transaction$gross_revenue <- as.numeric(transaction$gross_revenue)


# email -------------------------------------------------------------------
  

# Email list  
email_consol <- rbind(us_email,uk_eu_email) %>% 
  distinct() %>% 
  filter(!email %in% d$Email)

# Data cleaning
email_consol <- email_consol[!grepl("atg", email_consol$email),]
email_consol <- email_consol[!grepl("chun", email_consol$email),]
email_consol <- email_consol[!grepl("iscreen", email_consol$email),]

# Email consol
bucket_data <- email_consol %>% 
  mutate(
    purchase_history = case_when(
      email_consol$email %in% transaction$email ~ 'Yes',
      !email_consol$email %in% transaction$email ~ 'No',
      TRUE ~ 'N/A'
    )
  )

bucket_data_1 <- bucket_data %>% filter(purchase_history == "Yes")
bucket_data_2 <- bucket_data %>% filter(purchase_history == "No")

# add the number of purcahses
tmp <- transaction %>% group_by(email) %>% count(email)
bucket_data_1 <- merge(bucket_data_1, tmp, by= 'email', all = TRUE)
bucket_data_1 <- bucket_data_1 %>% 
  drop_na(purchase_history) %>% 
  select(name, email, everything()) %>% 
  rename("n_of_purchase" = n)

# add the total amount
tmp2 <-  transaction %>% group_by(email, order_state) %>% summarise(total_spent = sum(gross_revenue))
bucket_data_1 <- merge(bucket_data_1, tmp2, by= 'email', all = TRUE)
bucket_data_1 <- bucket_data_1 %>% 
  drop_na(purchase_history) %>% 
  select(name, email, everything()) 


# Add average spent per purchase
bucket_data_1 <- bucket_data_1 %>% mutate(avg_spent_per_purchase = total_spent/n_of_purchase)

# Merging
bucket_data_2$n_of_purchase <- NA
bucket_data_2$total_spent <- NA
bucket_data_2$order_state <- NA
bucket_data_2$avg_spent_per_purchase <- NA
bucket_data <- rbind(bucket_data_1,bucket_data_2) %>% 
  distinct()

bucket_data <- bucket_data %>% select(-name)
  
bucket_data$purchase_history <- as.factor(bucket_data$purchase_history)
bucket_data$n_of_purchase <- as.factor(bucket_data$n_of_purchase)
bucket_data$order_state <- as.character(bucket_data$order_state)
bucket_data$order_state[bucket_data$order_state == "CHARGED"] <- "COMPLETE"
bucket_data$order_state[bucket_data$order_state == "REFUNDED"] <- "FRAUD"
bucket_data$order_state <- as.factor(bucket_data$order_state)

write.csv(bucket_data , 'emails-map-with-transactions.csv')




# Visulaization
# [1] How many purchases did each user make?
bucket_data %>% 
  drop_na(n_of_purchase) %>% 
  group_by(n_of_purchase) %>% 
  tally() %>% 
  ggplot(aes(x = n_of_purchase, y = n)) +
  geom_bar(stat = "identity", fill = "royalblue", colour = "blue") +
  geom_text(aes(x= n_of_purchase, label=n), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(x = 'Number of Purchases', y = 'Number of Customers',
       title = "Number of purchases that each user make")


bucket_data %>% 
  drop_na(n_of_purchase) %>%
  ggplot(aes(x = total_spent, fill = order_state)) +
  geom_histogram(colour = "grey20") +
  scale_x_continuous(breaks = seq(0, 500, 50),
                     limits=c(0, 500)) +
  scale_y_continuous() +
  labs(x = 'Purchase Amount ($)', y = 'Number of Customers',
       title = "How much did customers spend in total?")

tmp <- bucket_data %>% 
  drop_na(n_of_purchase) %>% 
  group_by(order_state) %>% 
  summarise(total_dollar_spent = round(sum(total_spent),0), number_of_purchase = sum(as.numeric(n_of_purchase)) , number_of_users = n()) %>% 
  mutate(avg_spent_per_purchase = round(total_dollar_spent/number_of_purchase,1),
         avg_spent_per_user = round(total_dollar_spent/number_of_users,1)
  )

ft <- flextable(tmp)
ft <- autofit(ft)
plot(ft)


bucket_data %>% 
  drop_na(n_of_purchase) %>%
  ggplot(aes(x = avg_spent_per_purchase, fill = order_state)) +
  geom_histogram(colour = "#1F3552") +
  labs(x = 'Purchase Amount ($)', y = 'Number of Customers',
       title = "How much did customers spend on average?")
  

