# SWEEP for tidying forecast pacakge
#  we can finally use dates when forecasting as opposed to the regularly spaced numeric dates that the ts-system uses!

# 1. LOAD LIBRARIES
require(sweep)
require(forecast)
require(tidyquant)
require(timetk)

# 2. Load Data
beer_sales_tbl <- tq_get("S4248SM144NCEN", get = "economic.data", from = "2010-01-01", to = "2016-12-31")
beer_sales_tbl


# 3. TIDY FORECASTING WITH FORECAST + SWEEP
# Convert from tbl to ts
beer_sales_ts <- tk_ts(beer_sales_tbl, start = 2010, freq = 12)
beer_sales_ts


# 4. MODEL USING ARIMA
fit_arima <- auto.arima(beer_sales_ts)
fit_arima

# 5. TIDY THE MODEL
# sw_tidy(): Used to retrieve the model coefficients
sw_tidy(fit_arima)
# sw_glance(): Used to retrieve model description and training set accuracy metrics
sw_glance(fit_arima) %>%
  glimpse()
# sw_augment(): Used to get model residuals
sw_augment(fit_arima, timetk_idx = TRUE)
# Plot residuals 
# Make sure there is NO pattern !!
sw_augment(fit_arima, timetk_idx = TRUE) %>%
  ggplot(aes(x = index, y = .resid)) +
  geom_point() + 
  geom_hline(yintercept = 0, color = "red") + 
  labs(title = "Residual diagnostic") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_tq()


# 6. FORECAST
fcast_arima <- forecast(fit_arima, h = 12)
fcast_arima


# 7. TIDY THE FORECAST WITH SWEEP
fcast_tbl <- sw_sweep(fcast_arima, timetk_idx = TRUE)
fcast_tbl


# 8. (optional only when test) COMPARE ACTUAL VS PREDICTIONS
actuals_tbl <- tq_get("S4248SM144NCEN", get = "economic.data", from = "2017-01-01", to = "2017-12-31")

fcast_tbl %>%
  ggplot(aes(x = index, y = price, color = key)) +
  # 95% CI
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  # 80% CI
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color =  NA, size = 0, alpha = 0.8) +
  # Prediction
  geom_line() +
  geom_point() +
  # Actuals
  geom_line(aes(x = date, y = price), color = palette_light()[[1]], data = actuals_tbl) +
  geom_point(aes(x = date, y = price), color = palette_light()[[1]], data = actuals_tbl) +
  # Aesthetics
  labs(title = "Beer Sales Forecast: ARIMA", x = "", y = "Thousands of Tons",
       subtitle = "sw_sweep tidies the auto.arima() forecast output") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq()

# Investigate test error 
error_tbl <- left_join(actuals_tbl, fcast_tbl, by = c("date" = "index")) %>%
  rename(actual = price.x, pred = price.y) %>%
  select(date, actual, pred) %>%
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
# The MAPE error is approximately 3.7% from the actual value, which is better than previous 3.8% TimeTk example

