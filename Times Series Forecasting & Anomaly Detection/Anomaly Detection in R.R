##
## Anomaly Detection in R ##
require(anomalize)
require(tidyverse)

# Example Data in Tidyverse
tidyverse_cran_downloads

# ANOMALIZE
tidyverse_cran_downloads %>%
  time_decompose(count) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)


## OK, lets look at each step in detail

# 1. TIME SERIES DECOMPOSITION
# Frequency & trend is auto-selected
tidyverse_cran_downloads %>%
  time_decompose(count, method = "stl", frequency = "auto", trend = "auto", message = TRUE)

# 2. ANOMALY DETECTION OF REMAINDER
tidyverse_cran_downloads %>%
  time_decompose(count, method = "stl", frequency = "auto", trend = "auto", message = TRUE) %>%
  anomalize(remainder, method = "iqr", alpha = 0.05, max_anoms = 0.2, verbose = TRUE)


# 3. PLOT 

# Only One time-series can be selected at a time
# The trend is smooth, which is desirable to remove the central tendency without overfitting. 
# Finally, the remainder is analyzed for anomalies detecting the most significant outliers.
tidyverse_cran_downloads %>%
  
  # Select a single time series
  filter(package == "lubridate") %>%
  ungroup() %>%
  
  # Anomalize
  time_decompose(count, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "iqr", alpha = 0.05, max_anoms = 0.2) %>%
  
  # Plot Anomaly Decomposition
  plot_anomaly_decomposition() +
  ggtitle("Lubridate Downloads: Anomaly Decomposition")


# 4. ANOMALY LOWER AND UPPER BOUNDS

# It recomposes the lower and upper bounds of the anomalies around the observed values
#  "recomposed_l1" (lower limit) and "recomposed_l2" (upper limit).
tidyverse_cran_downloads %>%
  time_decompose(count, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "iqr", alpha = 0.05, max_anoms = 0.2) %>%
  time_recompose()

## Example
aa <- tidyverse_cran_downloads %>%
  # Select single time series
  filter(package == "lubridate") %>%
  ungroup() %>%
  # Anomalize
  time_decompose(count, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "iqr", alpha = 0.05, max_anoms = 0.2) %>%
  time_recompose() %>%
  # Plot Anomaly Decomposition
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("Lubridate Downloads: Anomalies Detected")
aa
