## Google Analytis + BigQuery 
# Conversion Funnel Analysis (with SQL and R)

# GA Enterprise version's (Google analyitcs 360) data can be captured in BigQuery
# and we can anlayze it using R

# Key Concept
# Hit No. 1 is the first interaction
# Hit No.20 is the 20th interaction

#html_doument
  #theme:flatly
 # df_print: paged
#  toc: true
  
require(bigquery)
require(DBI)
require(connections)
require(jsonlite)

require(furrr)
require(tictoc)

require(tidyverse)
require(tidyquant)
require(lubridate)
require(plotly)

## 2.0 Connection

# connecti with Bigqurey
bigquery:bq_auth()

con <- dbConnect(
  bigrquery:bigquery(),
  project: "bigquery-public-data"
  dataset: "google-analytics-sample"
  billing: "ga-sample-data-258815"
)
con

connection_view(con)

# 3.0 EDA
# INspect the BigQuery GA 
dbListTables(con) %>% length()
# check one
tbl(con, "ga_sessions_20160801") %>% glimpse()

# view as JSON
# - take a look at "totals" list column
# - Each visitor session has
totals_tbl <- tbl(con, "ga_sessions_20160801") %>% 
  head(1) %>%  # first row 
  select(visitId, totals) %>% 
  collect()
totals_tbl

totals_tbl %>% toJSON() %>% prettify()

# view as Nested Tibble
hits_tbl <- tbl(con, "ga_sessions_20160801") %>% 
  filter(visitId == 12345678) %>% 
  select(visitId, hits) %>% 
  collect()
hits_tbl %>% pluck("hits",1)

# Get into hits > pages for that visitID 1234567 aobve
hits_tbl %>% 
  pluck("hits", 1, "page") %>% 
  toJSON() %>% 
  prettify() %>% 
  fromJSON() %>% 
  unnest(cols = pagePath:pagePathLevel4)


 ## =========================== ##
# Q1. How many visitors per day?
plan("multisessions")

unique_visitors_tbl <- tbl(con, "ga_sessions_*") %>% 
  distinct(date, fullVisitorId) %>% 
  count(date) %>% 
  collect()
unique_visitors_tbl

# Q2. Total Number of Transactinos Over Time

# Q3. Transactinons by Referral / Medium

# Q4. Which Page Path brings to transactions

