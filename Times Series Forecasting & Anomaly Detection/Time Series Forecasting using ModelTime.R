# Time Series Forecasting using ModelTime

### === This is a Replicate of ModelTime introduction example presented in ModelTime github === ###
### [Github](https://github.com/business-science/modeltime) ###
### [Github2](https://business-science.github.io/modeltime/articles/getting-started-with-modeltime.html)

Sys.which("make")
library(tidymodels)
library(modeltime)
library(timetk)   
library(lubridate)
library(tidyverse)
library(glmnet)
library(randomForest)

# 1. Load Data ---------------------------------------------------------------
## We’ll start with a bike_sharing_daily time series data set that includes bike transactions
bike_transactions_tbl <- bike_sharing_daily %>%
  select(dteday, cnt) %>%
  set_names(c("date", "value")) 

bike_transactions_tbl

bike_transactions_tbl %>%
  plot_time_series(date, value, .interactive = FALSE)
# .interactive = TRUE to get a plotly interactive plot


# 2. Train/Test Split -----------------------------------------------------

splits <- bike_transactions_tbl %>%
  # assess = "3 months": use the last 3-months of data as the testing set.
  # cumulative = TRUE : sample all of the prior data as the training set
  time_series_split(assess = "3 months", cumulative = TRUE)

splits %>%
  tk_time_series_cv_plan() %>% #Converts the splits to dataframe
  plot_time_series_cv_plan(date, value, .interactive = FALSE)



# 3. Create a Model --------------------------------------------------------

## Let's make different models that are famous for timeseries modeling
## leveraing modeltime package

# This includes “Auto ARIMA” and “Auto ETS” functions from forecast and the “Prophet” algorithm from prophet,
# And some Machine Learning powered models

# 3 basic steps to take are
# 1. Model Spec: Use a specification function (e.g. arima_reg(), prophet_reg()) to initialize the algorithm and key parameters
# 2. Engine: Set an engine using one of the engines available for the Model Spec.
# 3. Fit Model: Fit the model to the training data


## (1) Auto ARIMA model
model_fit_arima <- arima_reg() %>% # general ARIMA model algorithm and key parameters
  set_engine("auto_arima") %>% # specific package-function to use and you can add any function-level arguments here.
  fit(value ~ date, training(splits)) # All modeltime models require a date column to be a regressor.

model_fit_arima

## (2) Prophet
model_fit_prophet <- prophet_reg() %>% # general Facebook's Prophet model
  set_engine("prophet", yearly.seasonality = TRUE) %>%
  fit(value ~ date, training(splits))

model_fit_prophet

## (3) ML Models

# Machine learning models are more complex than the automated models. 
# The general process goes like this:

# 1. Create Preprocessing Recipe
# 2. Create Model Specifications
# 3. Use Workflow to combine Model Spec and Preprocessing, and Fit Model

## Before we choose which model to use, we need to preprocess the data 
## The process uses the “date” column to create 45 new features that I’d like to model. 
## These include time-series signature features and fourier series.
recipe_spec <- recipe(value ~ date, training(splits)) %>%
  step_timeseries_signature(date) %>%
  step_rm(contains("am.pm"), contains("hour"), contains("minute"),
          contains("second"), contains("xts")) %>%
  step_fourier(date, period = 365, K = 5) %>%
  step_dummy(all_nominal())

recipe_spec %>% prep() %>% juice()

#### [1] Elastic Net
model_spec_glmnet <- linear_reg(penalty = 0.01, mixture = 0.5) %>%
  set_engine("glmnet")

workflow_fit_glmnet <- workflow() %>%
  add_model(model_spec_glmnet) %>% # Add a Model Spec
  add_recipe(recipe_spec %>% step_rm(date)) %>% # Add preprocessing steps (Note that “date” column is removed since Machine Learning algorithms don’t typically know how to deal with date or date-time features)
  fit(training(splits))


#### [2] Random Forest
model_spec_rf <- rand_forest(trees = 500, min_n = 50) %>%
  set_engine("randomForest")

workflow_fit_rf <- workflow() %>%
  add_model(model_spec_rf) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>%
  fit(training(splits))

#### [3] Hybrid Model
## Prophet Boost
# The Prophet Boost algorithm combines Prophet with XGBoost to get the best of both worlds (i.e. Prophet Automation + Machine Learning). The algorithm works by:

# First modeling the univariate series using Prophet
# Using regressors supplied via the preprocessing recipe (remember our recipe generated 45 new features), and regressing the Prophet Residuals with the XGBoost model

model_spec_prophet_boost <- prophet_boost() %>%
  set_engine("prophet_xgboost", yearly.seasonality = TRUE) 

workflow_fit_prophet_boost <- workflow() %>%
  add_model(model_spec_prophet_boost) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits))

workflow_fit_prophet_boost


# 4. Create Modeltime Table -----------------------------------------------

## The Modeltime Table organizes the models with IDs and creates generic descriptions to help us keep track of our models. 
## Let’s add the models to a modeltime_table()

model_table <- modeltime_table(
  model_fit_arima, 
  model_fit_prophet,
  workflow_fit_glmnet,
  workflow_fit_rf,
  workflow_fit_prophet_boost
) 

model_table



# 5. Model Calibration ----------------------------------------------------

## Model Calibration is used to quantify error and estimate confidence intervals. 
## We’ll perform model calibration on the Testing Set
## Two new columns are generated (“.type” and “.calibration_data”), 
## the most important of which is the “.calibration_data”. 
## This includes the actual values, fitted values, and residuals for the testing set.

calibration_table <- model_table %>%
  modeltime_calibrate(testing(splits))

calibration_table



# 6. Start Forecasting ----------------------------------------------------

calibration_table %>%
  modeltime_forecast(actual_data = bike_transactions_tbl) %>% # generate the forecast data for the testing set as a tibble
  plot_modeltime_forecast(.interactive = FALSE) # visualize the results 



# 7. Evaluate the Model (Test data Accuracy check) ----------------------------------

calibration_table %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

# We see that
# Auto ARIMA model is not a good fit for this data.
# And The best model is 'Prophet + XGBoost'


# 8. Refit and Forecast Forward -------------------------------------------

# Refitting is a best-practice before forecasting the future.

calibration_table %>%
  # Remove ARIMA model with low accuracy
  filter(.model_id != 1) %>%
  
  # Refit and Forecast Forward
  modeltime_refit(bike_transactions_tbl) %>% # re-train on full data
  modeltime_forecast(h = "12 months", actual_data = bike_transactions_tbl) %>% # h = xx, next xx timeframe
  plot_modeltime_forecast(.interactive = FALSE)




