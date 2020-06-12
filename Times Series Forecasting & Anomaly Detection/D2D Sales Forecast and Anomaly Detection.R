# Anomaly Detection 
library(tidyquant)  # Used for business-ready ggplot themes
library(anomalize)  # Identify and clean time series anomalies
library(timetk)     # Time Series Machine Learning Features

library(workflows)
library(parsnip)
library(recipes)
library(yardstick)
library(glmnet)

library(knitr) 
library(tidyr)
require(anomalize)
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
require(tibbletime)
require(vctrs)

# Read data
setwd("C:/Users/bokhy/Desktop/ATG")

b <- read.csv("session_byuser_export.csv")


# Pre-Processing
b$Service.Duration <- as.character(b$Service.Duration)
b$Service.Duration <- ifelse(nchar(b$Service.Duration) > 20, as.character(b$Service.Start.Time), b$Service.Duration)

b$Service.Start.Time <- as.character(b$Service.Start.Time)
b$Service.Start.Time <- ifelse(nchar(b$Service.Start.Time) < 10, as.character(b$temp1), b$Service.Start.Time)

b$Service.Start.Time <- as.Date(b$Service.Start.Time)
b$Service.Duration <- as.numeric(b$Service.Duration)

# Change it to minutes
b_1 <- b %>% group_by(Service.Start.Time) %>% summarise(total_hours = sum(Service.Duration)/60)
b_1$Service.Start.Time <- as.Date(b_1$Service.Start.Time)

b_1 <- b_1 %>% 
  rename(
    date = Service.Start.Time,
    value = total_hours
  )


# Visualization
b_1 %>%
  ggplot(aes(x = date, y = value)) +
  geom_rect(xmin = as.numeric(ymd("2020-04-01")),
            xmax = as.numeric(ymd("2020-05-05")),
            ymin = 0, ymax = 8000,
            fill = palette_light()[[4]], alpha = 0.01) +
  annotate("text", x = ymd("2020-04-15"), y = 4000,
           color = palette_light()[[1]], label = "Test Region") +
  geom_point(alpha = 0.5, color = palette_light()[[6]]) +
  labs(title = "BYOG Usage: Daily Scale", x = "") +
  theme_tq()

# Split into training and test sets
train_tbl <- b_1 %>% filter(date < ymd("2020-04-01"))
test_tbl  <- b_1 %>% filter(date >= ymd("2020-04-01"))

# Add time series signature
recipe_spec_timeseries <- recipe(value ~ ., data = train_tbl) %>%
  step_timeseries_signature(date) 

bake(prep(recipe_spec_timeseries), new_data = train_tbl)

# Building Engineered Features on Top of pre-processed train dataset
# This improves modeling behabior by having 29 features to 225 engineered features! 

recipe_spec_final <- recipe_spec_timeseries %>%
  step_rm(date) %>%
  step_rm(contains("iso"), 
          contains("second"), contains("minute"), contains("hour"),
          contains("am.pm"), contains("xts")) %>%
  step_normalize(contains("index.num"), date_year) %>%
  step_interact(~ date_month.lbl * date_day) %>%
  step_interact(~ date_month.lbl * date_mweek) %>%
  step_interact(~ date_month.lbl * date_wday.lbl * date_yday) %>%
  step_dummy(contains("lbl"), one_hot = TRUE) 

bake(prep(recipe_spec_final), new_data = train_tbl)

# Model Specification
model_spec_glmnet <- linear_reg(mode = "regression", penalty = 5, mixture = 0.7) %>%
  set_engine("glmnet")

# Workflow
workflow_glmnet <- workflow() %>%
  add_recipe(recipe_spec_final) %>%
  add_model(model_spec_glmnet)

workflow_glmnet

# Training
workflow_trained <- workflow_glmnet %>% fit(data = train_tbl)

# Visualize the forecast with Validation data
prediction_tbl <- workflow_trained %>% 
  predict(test_tbl) %>%
  bind_cols(test_tbl) 

prediction_tbl

b_1 %>%
  ggplot(aes(x = date, y = value)) +
  geom_rect(xmin = as.numeric(ymd("2020-04-01")),
            xmax = as.numeric(ymd("2020-05-05")),
            ymin = 0, ymax = 8000,
            fill = palette_light()[[4]], alpha = 0.01) +
  annotate("text", x = ymd("2020-04-15"), y = 4000,
           color = palette_light()[[1]], label = "Test Region") +
  geom_point(alpha = 0.5, color = palette_light()[[6]]) +
  # Add predictions
  geom_point(aes(x = date, y = .pred), data = prediction_tbl, 
             alpha = 0.5, color = palette_light()[[2]]) +
  theme_tq() +
  labs(title = "GLM: Out-Of-Sample Forecast")

# Calculating forecast error
prediction_tbl %>% metrics(value, .pred)

# visualize the residuals of the test set. 
# The residuals show that the model predicts pretty well
prediction_tbl %>%
  ggplot(aes(x = date, y = value - .pred)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  geom_smooth(span = 0.5, color = "red") +
  geom_smooth(span = 1.00, se = FALSE) +
  theme_tq() +
  labs(title = "GLM Model Residuals, Out-of-Sample", x = "") +
  scale_y_continuous(limits = c(-1000, 1000))


# ====
# At this point you might go back to the model and try tweaking features using interactions or polynomial terms, 
# adding other features that may be known in the future (e.g. temperature of day can be forecasted relatively accurately within 7 days), 
# or try a completely different modeling technique with the hope of better predictions on the test set. 
# Once you feel that your model is optimized, move on the final step of forecasting.

# ====

# Forecasting future data

# create the date sequence
idx <- b_1 %>% tk_index()

# Get time series summary from index
usage_summary <- idx %>% tk_get_timeseries_summary()
usage_summary[1:6] # general summary
usage_summary[7:12] # periodicity information.

# From above, we know that the data is 100% regular 
# because the median and mean differences are 86400 seconds or 1 day. 
# We don’t need to do any special inspections when we use tk_make_future_timeseries().

idx_future <- idx %>% tk_make_future_timeseries(n_future = 60)

future_tbl <- tibble(date = idx_future) 
future_tbl

# Retrain the model specification on the full data set, then predict the next 1 month.
future_predictions_tbl <- workflow_glmnet %>% 
  fit(data = b_1) %>%
  predict(future_tbl) %>%
  bind_cols(future_tbl)

future_predictions_tbl


# Visualize the forecast
b_1 %>%
  ggplot(aes(x = date, y = value)) +
  geom_rect(xmin = as.numeric(ymd("2020-04-01")),
            xmax = as.numeric(ymd("2020-05-05")),
            ymin = 0, ymax = 8000,
            fill = palette_light()[[4]], alpha = 0.01) +
  geom_rect(xmin = as.numeric(ymd("2020-05-06")),
            xmax = as.numeric(ymd("2020-06-05")),
            ymin = 0, ymax = 8000,
            fill = palette_light()[[3]], alpha = 0.01) +
  annotate("text", x = ymd("2020-04-15"), y = 4000,
           color = palette_light()[[1]], label = "Test Region") +
  annotate("text", x = ymd("2020-5-20"), y = 5000,
           color = palette_light()[[1]], label = "Forecast Region") +
  geom_point(alpha = 0.5, color = palette_light()[[6]]) +
  # future data
  geom_point(aes(x = date, y = .pred), data = future_predictions_tbl,
             alpha = 0.5, color = palette_light()[[2]]) +
  geom_smooth(aes(x = date, y = .pred), data = future_predictions_tbl,
              method = 'loess',span = 0.5, color = "red",se = FALSE) + 
  labs(title = "Bikes Sharing Dataset: 6-Month Forecast", x = "") +
  theme_tq()

# Forecast Error
# We need prediction intervals to account for the variance from the model predictions to the actual data. 
# We’ll follow the prediction interval methodology from Forecasting: Principles and Practice.
test_resid_sd <- prediction_tbl %>%
  summarize(stdev = sd(value - .pred))

future_predictions_tbl <- future_predictions_tbl %>%
  mutate(
    lo.95 = .pred - 1.96 * test_resid_sd$stdev,
    lo.80 = .pred - 1.28 * test_resid_sd$stdev,
    hi.80 = .pred + 1.28 * test_resid_sd$stdev,
    hi.95 = .pred + 1.96 * test_resid_sd$stdev
  )

b_1 %>%
  ggplot(aes(x = date, y = value)) +
  geom_point(alpha = 0.5, color = palette_light()[[1]]) +
  geom_ribbon(aes(y = .pred, ymin = lo.95, ymax = hi.95), 
              data = future_predictions_tbl, 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(y = .pred, ymin = lo.80, ymax = hi.80, fill = key), 
              data = future_predictions_tbl,
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_point(aes(x = date, y = .pred), data = future_predictions_tbl,
             alpha = 0.5, color = palette_light()[[2]]) +
  geom_smooth(aes(x = date, y = .pred), data = future_predictions_tbl,
              method = 'loess', color = "white") + 
  labs(title = "Bikes Sharing Dataset: 6-Month Forecast with Prediction Intervals", x = "") +
  theme_tq()