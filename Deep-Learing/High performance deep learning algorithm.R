## High performance deep learning algorithm that does an excellent job at predicting ten years of sunspots
## Taken from "https://www.business-science.io/timeseries-analysis/2018/04/18/keras-lstm-sunspots-time-series-prediction.html"

# Develop a Stateful LSTM Model with the keras package, which connects to the R TensorFlow backend.
# Apply a Keras Stateful LSTM Model to a famous time series, Sunspots.
# Perform Time Series Cross Validation using Backtesting with the rsample package rolling forecast origin resampling.
# Visualize Backtest Sampling Plans and Prediction Results with ggplot2 and cowplot.
# Evaluate whether or not a time series may be a good candidate for an LSTM model by reviewing the Autocorrelation Function (ACF) plot.

# Core Tidyverse
require(tidyverse)
require(glue)
require(forcats)

# Time Series
require(timetk)
require(tidyquant)
require(tibbletime)

# Visualization
require(cowplot)

# Preprocessing
require(recipes)

# Sampling / Accuracy
require(rsample)
require(yardstick) 

# Modeling
require(keras)

# 2. Load Data
sun_spots <- datasets::sunspot.month %>%
  tk_tbl() %>%
  mutate(index = as_date(index)) %>%
  as_tbl_time(index = index)
sun_spots

# 3. Exploratory Analysis
p1 <- sun_spots %>%
  ggplot(aes(index, value)) +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  theme_tq() +
  labs(
    title = "From 1749 to 2013 (Full Data Set)"
  )
p1
p2 <- sun_spots %>%
  filter_time("start" ~ "1800") %>%
  ggplot(aes(index, value)) +
  geom_line(color = palette_light()[[1]], alpha = 0.5) +
  geom_point(color = palette_light()[[1]]) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) +
  theme_tq() +
  labs(
    title = "1749 to 1800 (Zoomed In To Show Cycle)"
  )
p2
## Use cowplot
p_title <- ggdraw() + 
  draw_label("Sunspots", size = 18, fontface = "bold", colour = palette_light()[[1]])
p_title
plot_grid(p_title, p1, p2, ncol = 1, rel_heights = c(0.1, 1, 1))

# 3.2 EVALUATING THE ACF
# Determine whether or not an LSTM model may be a good approach

tidy_acf <- function(data, value, lags = 0:20) {
  
  value_expr <- enquo(value)
  
  acf_values <- data %>%
    pull(value) %>%
    acf(lag.max = tail(lags, 1), plot = FALSE) %>%
    .$acf %>%
    .[,,1]
  
  ret <- tibble(acf = acf_values) %>%
    rowid_to_column(var = "lag") %>%
    mutate(lag = lag - 1) %>%
    filter(lag %in% lags)
  
  return(ret)
}
max_lag <- 12 * 50

sun_spots %>%
  tidy_acf(value, lags = 0:max_lag)

sun_spots %>%
  tidy_acf(value, lags = 0:max_lag) %>%
  ggplot(aes(lag, acf)) +
  geom_segment(aes(xend = lag, yend = 0), color = palette_light()[[1]]) +
  geom_vline(xintercept = 120, size = 3, color = palette_light()[[2]]) +
  annotate("text", label = "10 Year Mark", x = 130, y = 0.8, 
           color = palette_light()[[2]], size = 6, hjust = 0) +
  theme_tq() +
  labs(title = "ACF: Sunspots")
# This is good news. We have autocorrelation in excess of 0.5 beyond lag 120 (the 10-year mark).
# We can theoretically use one of the high autocorrelation lags to develop an LSTM model.
sun_spots %>%
  tidy_acf(value, lags = 115:135) %>%
  ggplot(aes(lag, acf)) +
  geom_vline(xintercept = 120, size = 3, color = palette_light()[[2]]) +
  geom_segment(aes(xend = lag, yend = 0), color = palette_light()[[1]]) +
  geom_point(color = palette_light()[[1]], size = 2) +
  geom_label(aes(label = acf %>% round(2)), vjust = -1,
             color = palette_light()[[1]]) +
  annotate("text", label = "10 Year Mark", x = 121, y = 0.8, 
           color = palette_light()[[2]], size = 5, hjust = 0) +
  theme_tq() +
  labs(title = "ACF: Sunspots",
       subtitle = "Zoomed in on Lags 115 to 135")

# 4. BACKTESTING: TIME SERIES CROSS VALIDATION

## 4.1 DEVELOPING A BACKTESTING STRATEGY
periods_train <- 12 * 50
periods_test  <- 12 * 10
skip_span     <- 12 * 20

rolling_origin_resamples <- rolling_origin(
  sun_spots,
  initial    = periods_train,
  assess     = periods_test,
  cumulative = FALSE,
  skip       = skip_span
)

rolling_origin_resamples

## 4.2 VISUALIZING THE BACKTESTING STRATEGY
# Plotting function for a single split
plot_split <- function(split, expand_y_axis = TRUE, alpha = 1, size = 1, base_size = 14) {
  
  # Manipulate data
  train_tbl <- training(split) %>%
    add_column(key = "training") 
  
  test_tbl  <- testing(split) %>%
    add_column(key = "testing") 
  
  data_manipulated <- bind_rows(train_tbl, test_tbl) %>%
    as_tbl_time(index = index) %>%
    mutate(key = fct_relevel(key, "training", "testing"))
  
  # Collect attributes
  train_time_summary <- train_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
  test_time_summary <- test_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
  # Visualize
  g <- data_manipulated %>%
    ggplot(aes(x = index, y = value, color = key)) +
    geom_line(size = size, alpha = alpha) +
    theme_tq(base_size = base_size) +
    scale_color_tq() +
    labs(
      title    = glue("Split: {split$id}"),
      subtitle = glue("{train_time_summary$start} to {test_time_summary$end}"),
      y = "", x = ""
    ) +
    theme(legend.position = "none") 
  
  if (expand_y_axis) {
    
    sun_spots_time_summary <- sun_spots %>% 
      tk_index() %>% 
      tk_get_timeseries_summary()
    
    g <- g +
      scale_x_date(limits = c(sun_spots_time_summary$start, 
                              sun_spots_time_summary$end))
  }
  
  return(g)
}
## The plot_split() function takes one split (in this case Slice01), and returns a visual of the sampling strategy. 
## We expand the axis to the range for the full dataset using expand_y_axis = TRUE.
rolling_origin_resamples$splits[[1]] %>%
  plot_split(expand_y_axis = TRUE) +
  theme(legend.position = "bottom")


# Now Plotting function that scales to all splits 
plot_sampling_plan <- function(sampling_tbl, expand_y_axis = TRUE, 
                               ncol = 3, alpha = 1, size = 1, base_size = 14, 
                               title = "Sampling Plan") {
  
  # Map plot_split() to sampling_tbl
  sampling_tbl_with_plots <- sampling_tbl %>%
    mutate(gg_plots = map(splits, plot_split, 
                          expand_y_axis = expand_y_axis,
                          alpha = alpha, base_size = base_size))
  
  # Make plots with cowplot
  plot_list <- sampling_tbl_with_plots$gg_plots 
  
  p_temp <- plot_list[[1]] + theme(legend.position = "bottom")
  legend <- get_legend(p_temp)
  
  p_body  <- plot_grid(plotlist = plot_list, ncol = ncol)
  
  p_title <- ggdraw() + 
    draw_label(title, size = 18, fontface = "bold", colour = palette_light()[[1]])
  
  g <- plot_grid(p_title, p_body, legend, ncol = 1, rel_heights = c(0.05, 1, 0.05))
  
  return(g)
  
}
# all splits
rolling_origin_resamples %>%
  plot_sampling_plan(expand_y_axis = F, ncol = 3, alpha = 1, size = 1, base_size = 10, 
                     title = "Backtesting Strategy: Rolling Origin Sampling Plan")


## 5.0 MODELING THE KERAS STATEFUL LSTM MODEL

### To begin, we'll develop a single Keras Stateful LSTM model on a single sample from the Backtesting Strategy. 
### We'll then scale the model to all samples to investigate/validate the modeling performance.

split    <- rolling_origin_resamples$splits[[11]]
split_id <- rolling_origin_resamples$id[[11]]
plot_split(split, expand_y_axis = FALSE, size = 0.5) +
  theme(legend.position = "bottom") +
  ggtitle(glue("Split: {split_id}"))
