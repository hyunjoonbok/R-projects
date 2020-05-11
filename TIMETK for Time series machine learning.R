# TIMETK # 
# Time series machine learning

# 1. LOAD LIBRARIES
require(timetk)
require(tidyquant)

# 2. Load Data
# We'll get data using the tq_get() function from tidyquant
beer_sales_tbl <- tq_get("S4248SM144NCEN", get = "economic.data", from = "2010-01-01", to = "2016-12-31")
beer_sales_tbl
# Basic plot
beer_sales_tbl %>%
  ggplot(aes(date, price)) +
  geom_line(col = palette_light()[1]) +
  geom_point(col = palette_light()[1]) +
  geom_ma(ma_fun = SMA, n = 12, size = 1) +
  theme_tq() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Beer Sales: 2010.01 through 2018.03")


# 3. Time series machine learning to forecast time series data
# Use tk_index() to extract the index and tk_get_timeseries_summary() to retrieve summary information of the index. 
# Use glimpse() to output in a nice format for review.
beer_sales_tbl %>%
  tk_index() %>%
  tk_get_timeseries_summary() %>%
  glimpse()


# 4. AUGMENT TIME SERIES SIGNATURE
# it expands out the timestamp information column-wise into a machine learning feature set, 
# adding columns of time series information to the original data frame.
beer_sales_tbl_aug <- beer_sales_tbl %>%
  tk_augment_timeseries_signature()
beer_sales_tbl_aug


# 5. MODEL TIME SERIES
# Apply any regression model to the data. We'll use lm(). Note that we drop the date and diff columns. 
# Most algorithms do not work with dates, and the diff column is not useful for machine learning
# You could try any other ! (like xgboost)
fit_lm <- lm(price ~ ., data = select(beer_sales_tbl_aug, -c(date, diff)))
summary(fit_lm)


# 6. BUILD FUTURE (NEW) DATA
future_idx <- beer_sales_tbl %>% 
  tk_index() %>%
  tk_make_future_timeseries(n_future = 12)
future_idx
# Then turn index into time signature data frame
new_data_tbl <- future_idx %>%
  tk_get_timeseries_signature()
new_data_tbl


# 5. FORECAST 
pred <- predict(fit_lm, newdata =  select(new_data_tbl, -c(index, diff)))
pred
# Try change the ordered classes to plain factor if does not work
# mutate_if(is.ordered, ~ as.character(.) %>% as.factor)

predictions_tbl <- tibble(
  date  = future_idx,
  value = pred
)
predictions_tbl


# (optional only when test) 6. COMPARE ACTUAL VS PREDICTIONS
actuals_tbl <- tq_get("S4248SM144NCEN", get = "economic.data", from = "2017-01-01", to = "2017-12-31")

# Plot Beer Sales Forecast
beer_sales_tbl %>%
  ggplot(aes(x = date, y = price)) +
  # Training data
  geom_line(color = palette_light()[[1]]) +
  geom_point(color = palette_light()[[1]]) +
  # Predictions
  geom_line(aes(y = value), color = palette_light()[[2]], data = predictions_tbl) +
  geom_point(aes(y = value), color = palette_light()[[2]], data = predictions_tbl) +
  # Actuals
  geom_line(color = palette_light()[[1]], data = actuals_tbl) +
  geom_point(color = palette_light()[[1]], data = actuals_tbl) +
  # Aesthetics
  theme_tq() +
  labs(title = "Beer Sales Forecast: Time Series Machine Learning",
       subtitle = "Using basic multivariate linear regression can yield accurate results")


# Investigate test error
error_tbl <- left_join(actuals_tbl, predictions_tbl) %>%
  rename(actual = price, pred = value) %>%
  mutate(
    error     = actual - pred,
    error_pct = error / actual
  ) 
error_tbl

# Calculate important residuals metrics
test_residuals <- error_tbl$error
test_error_pct <- error_tbl$error_pct * 100 # Percentage error

me   <- mean(test_residuals, na.rm = TRUE)
rmse <- mean(test_residuals^2, na.rm = TRUE)^0.5
mae  <- mean(abs(test_residuals), na.rm = TRUE)
mape <- mean(abs(test_error_pct), na.rm = TRUE)
mpe  <- mean(test_error_pct, na.rm = TRUE)

tibble(me, rmse, mae, mape, mpe) %>% glimpse()
# The MAPE error is approximately 3.8% from the actual value, which is pretty good






