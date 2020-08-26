require(tidyverse)
require(lubridate)
require(caret)
require(h2o)
require(corrplot)
require(correlationfunnel)
library(GGally)
require(Boruta)
require(skimr)
require(mice)
require(tidyquant)
require(rsample)
require(recipes)
require(iml)
require(DALEX)
require(DataExplorer)
require(xgboost)
require(lime)
require(yardstick)
require(corrr)
require(knitr)
require(Matrix)
library(tidymodels) 
library(beeswarm)
library(ggbeeswarm)
setwd("C:/Users/bokhy/Desktop/")
transaction <- read_csv("a.csv",col_names = TRUE)

table(transaction$currency_full_name)
transaction$currency_full_name[transaction$currency_full_name == "Euro Member Countries"] <- "EUR"
transaction$currency_full_name[transaction$currency_full_name == "United Kingdom Pound"] <- "GBP"
transaction$currency_full_name[transaction$currency_full_name == "United States Dollar"] <- "USD"
transaction <- transaction %>% 
  filter(currency_full_name == 'USD' | currency_full_name == 'GBP' | currency_full_name == 'EUR')

table(transaction$company_name)
transaction$company_name[transaction$company_name == "1C Company"] <- "1C Entertainment"
transaction$company_name[transaction$company_name == "Axis Game Factory, Inc."] <- "Axis Game Factory LLC"
transaction$company_name[transaction$company_name == "Electronic Arts UK"] <- "Electronic Arts"
transaction$company_name[transaction$company_name == "Koei Tecmo"] <- "Koei"
transaction$company_name[transaction$company_name == "Private Division"] <- "TakeTwo Private Division"
transaction$company_name[transaction$company_name == "THQ Nordic"] <- "THQ Nordic GmbH"
transaction$company_name[transaction$company_name == "Ubisoft Entertainment"] <- "Ubisoft"
transaction$company_name[transaction$company_name == "Team 17"] <- "Team17"



# Filter-out incomplete transactions
transaction <- transaction %>% 
  # Too old transactions
  filter(!type == 'LivegamerPayment') %>% 
  filter(!country_full_name == 'Taiwan, Province of China') %>% 
  # incomplete transactions
#  filter(order_state == 'CHARGED' | order_state == 'REFUNDED') %>% 
  filter(!company_name == 'AtGames Test Publisher 1') %>% 
  filter(!company_name == 'AtGames Test Publisher 2') %>% 
  filter(!company_name == 'AtGames Test Publisher 3') %>% 
  filter(!company_name == 'Ravenscourt - DUPLICATE') %>% 
  select(title, company_name, currency_full_name, country_full_name,
         msrp,type, gross_revenue, account_id,
         vip_discount, vip_discounts, payment_date 
  ) 

# transaction$account_id <- as.factor(transaction$account_id)
# Create uniform title name for all regions (Remove UK,EU specification)
transaction$title <- gsub("\\<U+0099>s","",transaction$title)
transaction$title <- gsub("\\<U+393C>","",transaction$title)
transaction$title <- gsub("\\<U+3E39>s","",transaction$title)
transaction$title <- gsub("\\â„¢","",transaction$title)
transaction$title <- gsub("\\?","",transaction$title)
transaction$title <- gsub("\\??","",transaction$title)
transaction$title <- gsub("\\{EU}","",transaction$title)
transaction$title <- gsub("\\{UK}","",transaction$title)
transaction$title <- gsub("\\®","",transaction$title)
transaction$title <- gsub("[[:blank:]]","",transaction$title)

transaction <- transaction %>% 
  filter(!company_name == 'Sony Interactive Entertainment')

# check game list for any duplicates
game_list <- transaction %>% 
  select(title) %>% 
  unique()

# Create datetime columns
transaction$payment_date <- as.POSIXct(strptime(transaction$payment_date, "%m/%d/%Y %H:%M"))
transaction$payment_Year <- as.factor(lubridate::year(transaction$payment_date))
transaction$payment_Quarter <- as.factor(lubridate::quarter(transaction$payment_date))
transaction$payment_Month <- as.factor(lubridate::month(transaction$payment_date))
transaction$payment_Date <- as.factor(lubridate::day(transaction$payment_date))
transaction$payment_Hour <- as.numeric(lubridate::hour(transaction$payment_date))
transaction$payment_Minute <- as.factor(lubridate::minute(transaction$payment_date))
transaction$payment_Second <- as.factor(lubridate::second(transaction$payment_date))
transaction$payment_Weeknum <- as.factor(lubridate::week(transaction$payment_date))
transaction$payment_Weekdays <- as.factor(weekdays(transaction$payment_date))
transaction$timeoftheday<- with(transaction, ifelse(payment_Hour >= 5 & payment_Hour<=11, "morning",
                                                    ifelse(payment_Hour>11 & payment_Hour<=16, "afternoon",
                                                           ifelse(payment_Hour>16 & payment_Hour<=21, "evening" ,"night"))))
transaction$payment_Hour <- as.factor(transaction$payment_Hour)

# Factorize a few other variables
transaction$title <- as.factor(transaction$title)
transaction$company_name <- as.factor(transaction$company_name)
transaction$currency_full_name <- as.factor(transaction$currency_full_name)
transaction$country_full_name <- as.factor(transaction$country_full_name)
#transaction$order_state <- as.factor(transaction$order_state)
transaction$type <- as.factor(transaction$type)
#transaction$rev_share_1 <- as.factor(transaction$rev_share_1)
transaction$gross_revenue <- as.numeric(transaction$gross_revenue)
#transaction$net_revenue <- as.numeric(transaction$net_revenue)
transaction$vip_discount <- as.factor(transaction$vip_discount)
transaction$timeoftheday <- as.factor(transaction$timeoftheday)
transaction$account_id <- as.factor(transaction$account_id)

# Adding newly created variables
transaction$sold_price <- transaction$gross_revenue + transaction$vip_discounts
transaction <- transaction %>% 
  #  select(-payment_date) %>% 
  filter(gross_revenue >= 0) %>% 
  filter(sold_price >= 0) 

transaction$ddate <- as.Date(transaction$payment_date)


glimpse(transaction)

# Setting up the theme
library(extrafont)
font_import()
loadfonts(device = "win")

base_theme <- theme(
  plot.margin = unit(rep(1, 4), "cm"),
  plot.title = element_text(size = 18, 
                            face = "bold",
                            color = "#22292F", 
                            lineheight = 1.1,
                            margin = margin(b = 8)),
  plot.subtitle = element_text(size = 15, 
                               lineheight = 1.1,
                               color = "#22292F",
                               margin = margin(b = 25)),
  plot.caption = element_text(size = 12,
                              margin = margin(t = 25), 
                              color = "#3D4852"),
  panel.grid.minor = element_blank(),
  legend.text = element_text(size = 12),
  legend.title = element_text(size = 14),
  axis.title.x = element_text(margin = margin(t = 15), 
                              size = 16),
  axis.title.y = element_text(margin = margin(r = 15, l = 35), 
                              size = 16),
  axis.text = element_text(color = "#22292F", size = 14),
  axis.ticks = element_line(size = .07),
  axis.line = element_line(color = "black", size = .07)
) 

set_base_theme <- function() {
  theme_set(theme_minimal(base_size = 18,
                          base_family = "Source Sans Pro") +
              base_theme)
}

set_base_theme()

# ====== Summary Statistics ========
##[1] Unique Customer
transaction %>% group_by(account_id) %>% tally()

##[2] 
tmp <- transaction %>% group_by(title, vip_discount) %>% tally() %>% arrange(desc(n))
tmp
##[3] 
library(flextable)
t1 <- transaction %>% group_by(currency_full_name) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  set_names(c("Currency","# of Purchases")) 
ft <- flextable(t1)
ft <- autofit(ft)
plot(ft)

t2 <- transaction %>% group_by(country_full_name) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  top_n(10) %>% 
  set_names(c("Country","# of Purchases")) 
ft <- flextable(t2)
ft <- autofit(ft)
plot(ft)

##[4]
t3 <- transaction %>% 
  group_by(ddate) %>% 
  summarise(total_rev = sum(gross_revenue))
t3 %>% 
  ggplot(aes(x = ddate, y =total_rev)) +
  geom_line(color="#69b3a2") +
  ylim(0, 1000) +
  theme_tq() + 
  scale_color_tq() +
  labs(title = "Daily Revenue Trend",
       x = "", y = "Gross Revenue", color = "")

# ====== Visulaization ========
#[1] Number of Purchases per discount level (cumulative)
transaction %>% 
  drop_na(title) %>% 
  filter(!vip_discount == '0') %>% 
  group_by(vip_discount) %>% 
  tally() %>% 
  ggplot(aes(x = vip_discount, y = n)) +
  geom_bar(stat = "identity", fill = "royalblue", colour = "blue") +
  geom_text(aes(x= vip_discount, label=n), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(x = 'Discount Level', y = 'Number of Purchases',
       title = 'Number of Purchases per discount level (cumulative)')

#[2] Number of Purchases per discount level (last 1 year)
transaction %>% 
  drop_na(title) %>% 
  filter(!vip_discount == '0') %>% 
  filter(ddate >= as.Date("2019-08-17")) %>%
  group_by(vip_discount) %>% 
  tally() %>% 
  ggplot(aes(x = vip_discount, y = n)) +
  geom_bar(stat = "identity", fill = "pink") +
  geom_text(aes(x= vip_discount, label=n), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(x = 'Discount Level', y = 'Number of Purchases',
       title = 'Number of Purchases per discount level (past 1 year)')

# [3]Accumulation of revenuw
accumulated_rev <- transaction %>% 
  group_by(payment_Weeknum, payment_Year) %>% 
  summarise(sum = sum(gross_revenue)) %>% 
  ungroup() %>% 
  arrange(payment_Year, payment_Weeknum) %>% 
  mutate(cumsum = cumsum(sum)) %>% 
  complete(payment_Weeknum, nesting(payment_Year)) %>% 
  arrange(payment_Year, payment_Weeknum) %>% 
  fill(cumsum)

accumulated_rev[c(1:138),] %>% 
  ggplot(aes(as.numeric(payment_Weeknum), cumsum)) +
  geom_area(fill = "#84254a", alpha = .6, position = 'stack') +
  facet_grid(cols = vars(payment_Year))+
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(10, 50, by = 10)) +
  scale_y_continuous(expand = c(0, 0), position = "right",
                     label = label_number(suffix = " ($)")) +
  theme(
    strip.background = element_rect(fill = "#e5e5e5", color = "#e5e5e5"),
    strip.text = element_text(size = 13),
    panel.spacing = unit(0.15, "cm"),
    panel.background = element_rect(fill = "#f4f3f3", color = "#f4f3f3")
  ) +
  labs(
    title = 'Revenue Accumulation after D2D Rewards',
    subtitle = '($)106,704 gross revenue earned as of Aug.15 2020',
    x = "Week of the year",
    y = "Cumulative Revenue\n"
  )


# Revnue per Discount level
transaction %>%
  filter(!vip_discount == '0') %>% 
  ggplot(aes(x = vip_discount, y = gross_revenue)) +
  geom_quasirandom(alpha = 0.05, width = 0.2, color = "#eba487") +
  stat_summary(fun = "median", geom = "point", size = 2, color = "#abdcf1") +
  stat_summary(fun = "median", geom = "line", aes(group = 1),
               size = 1.1, color = "#abdcf1") +
  scale_y_continuous(limits = c(0, 70),
                     expand = c(0, 0),
                     labels = label_number(suffix = " ($)")) +
  labs(
    title = "Revenue by Discount Level",
    subtitle = paste0("Most customers received either 8%, 12%, 15% or 20% discount\n",
                      "where their average purchase amount is betwwen $10 - $25.\n"),
    x = "Discount Level",
    y = "Purchase Amount ($)"
  )

# Purchases per each price point
transaction %>% 
  ggplot(aes(x = gross_revenue, fill = payment_Year)) +
  geom_histogram(binwidth = 5, colour = "white") +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  labs(
    title = "Purchases per each price point",
    subtitle = paste0("Each Bar represents $5 bucket (First bar is $0 - $4.99)\n",
                      "We see most of the purchased titles are betwwen $5-$10.\n",
                      "Only 3.2% of total transactions is over $59.99\n "),
    x = "Product Price Point",
    y = "Number of Purchases"
  )
