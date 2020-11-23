# Simple Tip - Forecasting ARIMA Models with Nest + Modeltime
# content drived from https://www.youtube.com/watch?v=3znQUrREUC8&feature=youtu.be&utm_source=Business+Science+-+Combined+List&utm_campaign=47bfb14ed3-TUES_TIPS_012_nest_modeltime&utm_medium=email&utm_term=0_a4e5b7c52f-47bfb14ed3-68045131&mc_cid=47bfb14ed3&mc_eid=cee6dece6f&ab_channel=BusinessScience

# 1.0 Load Libraries ----
#install.packages("remotes")
#remotes::install_github("business-science/modeltime")
#remotes::install_github("business-science/modeltime.ensemble")
#remotes::install_github("business-science/modeltime.resample")
#remotes::install_github("business-science/timetk")

require(modeltime)
require(tidymodels)
require(timetk)
require(tidyverse)

# 2.0 Load Data ----

timetk::walmart_sales_weekly

walmart_sales_weekly %>% 
  group_by(id) %>% 
  plot_time_series(Date,Weekly_Sales, .facet_ncol = 2)

# 3.0 Nesting Time-series Data ----
# nest() can create a new 'nested' column taking except for 
# the column specified ('id' in our case)
# Nesting is useful when we are dealing with multiple groups of time-series data together

data_nested <- walmart_sales_weekly %>% 
  select(id, Date, Weekly_Sales) %>% 
  nest(nested_column = -id)
# We see that time-seires data is grouped for each (7) ids
# Each tibble in nested_column contatins correspoinding rows for each id
data_nested$nested_column

# Unnesting -- getting the data back to original
data_nested %>% 
  unnest(nested_column)


# 4.0 Mutiple Arima Forecasts Models ----

# Making many models at the same time
model_table <- data_nested %>% 
  
  # 4.1 create a new 'fitted model' column for each 'nested_column' with a blank function
  # each of 'df' would be a tibble for each group
  mutate(fitted_model = map(nested_column, .f = function(df) { 
  # 52 - because it's weekly data  
    arima_reg(seasonal_period = 52) %>% 
      set_engine('auto_arima') %>% 
      fit(Weekly_Sales ~ Date, data = df)
    
  })) %>% 
  
  # 4.2 Map Forecasts
  # now we take both 'nested_column' and 'fitted_model' column to forecast forward
  mutate(nested_forecast = map2(fitted_model, nested_column, .f = function(arima_model, df){
    modeltime_table(
      arima_model
    ) %>% 
      modeltime_forecast(h = 52, actual_data = df)
  }))

model_table


# 5.0 Unnest to view the raw number and visualize ----

model_table %>% 
  select(id, nested_forecast) %>% 
  unnest(nested_forecast) %>% 
  group_by(id) %>% 
  plot_modeltime_forecast(.facet_ncol = 2, .conf_interval_show = FALSE)
