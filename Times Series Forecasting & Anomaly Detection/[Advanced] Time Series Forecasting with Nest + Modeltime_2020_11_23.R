# Forecasting with Nest + Modeltime [Advanced]

# 1.0 Load Libraries ----
#install.packages("remotes")
#remotes::install_github("business-science/modeltime")
#remotes::install_github("business-science/timetk")

require(modeltime)
require(modeltime.ensemble)
require(modeltime.resample)
require(tidymodels)
require(timetk)
require(tidyverse)
require(kernlab)

# 2.0 Load Data ----

timetk::walmart_sales_weekly

walmart_sales_weekly %>% 
  group_by(id) %>% 
  plot_time_series(Date,Weekly_Sales, .facet_ncol = 3, .interactive = FALSE)

# 3.0 Data setup ----

forecast_horizon <- 52 # how many period forward you want to forecast 

## Prepare Full data = Training(past) region + Forecast(future) region
full_data_tbl <- walmart_sales_weekly %>% 
  select(id, Date, Weekly_Sales) %>% 
  
  # Apply grouped time-seires manipulation
  group_by(id) %>% 
  future_frame(
    .date_var = Date,
    .length_out = forecast_horizon, # it increase each id by 52 timestamps into the future
    .bind_data = TRUE
    ) %>% 
  ungroup() %>% 
  
  # Consolidate all IDs
  mutate(id = fct_drop(id))

full_data_tbl

# We must do this once you make a full dataset,
# Now we split the full_data to training and forecast data

# Training Data (original data in this case)
data_prepared_tbl <- full_data_tbl %>% 
  filter(!is.na(Weekly_Sales)) 

## Check for summary of timeseries data for training set
## We want to make sure that each group has the same "time range" and "scale"
data_prepared_tbl %>% 
  group_by(id) %>% 
  tk_summary_diagnostics()
 
# Future Data 
future_tbl <- full_data_tbl %>% 
  filter(is.na(Weekly_Sales))
future_tbl

future_tbl %>% 
  group_by(id) %>% 
  tk_summary_diagnostics()

# 4.0 Data Splitting ----

## Now we set aside the future data (we would only need that later when we make forecast)
## And focus on training data


## * 4.1 Panel Data Splitting ----

## Split the dataset into analyis/assessment set
splits <- data_prepared_tbl %>% 
  time_series_split(
    date_var = Date,
    assess = forecast_horizon,
    cumulative = TRUE
  )
splits


# 5.0 Create Preprocessor ----

recipe_spec_1 <- recipe(Weekly_Sales ~ ., training(splits)) %>% 
  # break 'date' to many different calendar features
  step_timeseries_signature(Date) %>% 
  # remove some that are not important
  step_rm(matches("(.iso$)|(.xts$)|(day)|(hour)|(minute)|(second)|(am.pm)")) %>% 
  # normalize some larger varialbes like Date index numeric and year
  step_normalize(Date_index.num, Date_year) %>% 
  # remove feature that contains only one variable
  step_zv(all_predictors()) %>% 
  # convert weekly feature into a factor
  step_mutate(Date_week = factor(Date_week, ordered = TRUE)) %>% 
  # dummy all factors
  step_dummy(all_nominal(), one_hot =  TRUE)

# Now a "DESIGN MATRIX" is made
recipe_spec_1 %>% prep() %>% juice() %>% glimpse()

# give "Date" column a new role called 'ID'
# this it because the recipe_spec_1 needs to have a Date feature  
# for non-modeltime algorithm, we need to be able to use ID
recipe_spec_2 <- recipe_spec_1 %>% 
  update_role(Date, new_role = 'ID')

# previously "Date" is a predictor in 'recipe_spec_1' but now it's 'ID' in 'recipe_spec_2'
recipe_spec_1 %>% prep() %>% summary()

recipe_spec_2 %>% prep() %>% summary()

# 6.0 MODELS ----
# [MAKE SURE TO CREATE MUTIPLE MODELS FOR EXPRIMENT]

# * Prophet with Regressors ----
# ** 1. workflow() ----
wflw_fit_prophet <- workflow() %>% 
# ** 2. add_model() ---- 
  add_model(
    # 'prophet_reg()' is from modeltime package so we have to use "recipe_spec_1"
    modeltime::prophet_reg() %>% set_engine('prophet') 
  ) %>% 
# ** 3. add_recipe() ----  
  add_recipe(recipe_spec_1) %>% 
# ** 4. fit() ----   
  fit(training(splits))

wflw_fit_prophet

# * XGBoost ----
wflw_fit_xgboost <- workflow() %>% 
  add_model(
    # 'boost_tree()' is from parsnip package so we have to use "recipe_spec_2"
    parsnip::boost_tree() %>% set_engine('xgboost')
  ) %>% 
  add_recipe(recipe_spec_2) %>% 
  fit(training(splits))

wflw_fit_xgboost

# * Random Forest ----
wflw_fit_rf <- workflow() %>% 
  add_model(
    # 'rand_forest()' is from parsnip package so we have to use "recipe_spec_2"
    parsnip::rand_forest() %>% set_engine('ranger')
  ) %>% 
  add_recipe(recipe_spec_2) %>% 
  fit(training(splits))

wflw_fit_rf

# * Support Vector Machine (SVM) ----
wflw_fit_svm <- workflow() %>% 
  add_model(
    # 'svm_rbf()' is from parsnip package so we have to use "recipe_spec_2"
    parsnip::svm_rbf() %>% set_engine('kernlab')
  ) %>% 
  add_recipe(recipe_spec_2) %>% 
  fit(training(splits))

wflw_fit_svm

# * Prophet Boost (Xgboost version of Prophet)----
wflw_fit_prophet_boost <- workflow() %>% 
  add_model(
    modeltime::prophet_boost(
      seasonality_daily = FALSE,
      seasonality_weekly = FALSE,
      seasonality_yearly = FALSE
    ) %>% 
      set_engine('prophet_xgboost')
  ) %>% 
  add_recipe(recipe_spec_1) %>% 
  fit(training(splits))

wflw_fit_prophet_boost

# 7.0 MODELTIME FORECASTING WORKFLOW ----

# * 7.1 Create Modeltime Table ----
## At this step, include all models that we created above 
## We can see here all organized
submodels_tbl <- modeltime_table(
  wflw_fit_prophet,
  wflw_fit_xgboost,
  wflw_fit_rf,
  wflw_fit_svm,
  wflw_fit_prophet_boost
)
submodels_tbl

# * 7.2 Calibrate with Testing Data ----
submodels_calibrated_tbl <- submodels_tbl %>% 
  modeltime_calibrate(testing(splits))

submodels_calibrated_tbl

# * 7.3 Measure Testing Accuracy ----
submodels_calibrated_tbl %>% modeltime_accuracy()

# * 7.4 Visualize Test Forecast ----
# calibrated table can be piped into forecast with my new_data with testing data
# and actual data is for comparison purpose
# keep_data attaches previous data in the same tbl for easy comparison purpose
submodels_calibrated_tbl %>% 
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = data_prepared_tbl,
    keep_data = TRUE
  ) %>% 
  group_by(id) %>% 
  plot_modeltime_forecast(.facet_ncol = 2)

# * 7.5 Refit on Full Dataset ----
## Take the model that was trained on training set and re-train on Full-dataset
submodels_refit_tbl <- submodels_calibrated_tbl %>% 
  modeltime_refit(data_prepared_tbl)

submodels_refit_tbl

# Visualize it
submodels_refit_tbl %>% 
  modeltime_forecast(
    new_data = future_tbl,
    actual_data = data_prepared_tbl,
    keep_data = TRUE
  ) %>% 
  group_by(id) %>% 
  plot_modeltime_forecast(.facet_ncol = 2)

# 8.0 ENSEMBLE MODELS ----
## We want to improve the forecast by mixing different models together

# * 8.1 Make Ensemble ----
## (this case we ensemble on 'mean' value)
ensemble_fit_mean <- submodels_tbl %>% 
  # we don't have to include certain ones 
  filter(!.model_id %in% c(1)) %>% 
  ensemble_average(type = 'mean')

ensemble_fit_mean

# * 8.2 Modeltime Table ----
ensemble_tbl <- modeltime_table(
  ensemble_fit_mean
)

# * 8.3 Ensemble Test Accuracy ----
ensemble_tbl %>% 
  # combine it with previous submodels tbl for comparison
  combine_modeltime_tables(submodels_tbl) %>% 
  modeltime_accuracy(testing(splits))

# * 8.4 Ensemble Test Forecast ----
ensemble_tbl %>% 
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = data_prepared_tbl,
    keep_data = TRUE
  ) %>% 
  group_by(id) %>% 
  plot_modeltime_forecast(.facet_ncol = 2)

# * 8.5 Ensemble Refit on Full Dataset ----
ensemble_refit_tbl <- ensemble_tbl %>% 
  modeltime_refit(data_prepared_tbl)

ensemble_refit_tbl

# Visualize it
ensemble_refit_tbl %>% 
  modeltime_forecast(
    new_data = future_tbl,
    actual_data = data_prepared_tbl,
    keep_data = TRUE
  ) %>% 
  group_by(id) %>% 
  plot_modeltime_forecast(.facet_ncol = 2)