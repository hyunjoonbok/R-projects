# DataExplorer to perform a EDA in 10 sec

# install.packages("DataExplorer")

# Load Packages
library(nycflights13)
library(DataExplorer)
library(tidyverse)
library(kableExtra)
# Load Toy Data
data_list <- list(airlines, airports, flights, planes, weather)
plot_str(data_list)

# data merge to make it bigger
# Omit this step (if your data's already ready-to-go)
merge_airlines <- merge(flights, airlines, by = "carrier", all.x = TRUE)
merge_planes <- merge(merge_airlines, planes, by = "tailnum", all.x = TRUE, suffixes = c("_flights", "_planes"))
merge_airports_origin <- merge(merge_planes, airports, by.x = "origin", by.y = "faa", all.x = TRUE, suffixes = c("_carrier", "_origin"))
final_data <- merge(merge_airports_origin, airports, by.x = "dest", by.y = "faa", all.x = TRUE, suffixes = c("_origin", "_dest"))


# 1. Get a glimpse
introduce(final_data)
plot_intro(final_data)

# 2. Look at missing value
plot_missing(final_data)
## From the chart, speed variable is mostly missing, and plot suggests to remove it
final_data <- drop_columns(final_data, "speed")

# Important! store the missing data profile with "profile_missing(final_data)" for additional analysis


# 3. Visualize frequency distributions for all discrete features
plot_bar(final_data , nrow = 5L, ncol = 3L)

## Feature dst_origin and tzone_origin contains only 1 value, so we should drop them:

plot_bar(final_data, nrow = 5L, ncol = 3L, with = "arr_delay")

# 4. Plot Histogram
plot_histogram(final_data[,5])
#Set flight to categorical, since that is the flight number with no mathematical meaning:
final_data <- update_columns(final_data, "flight", as.factor)
# Remove year_flights and tz_origin since there is only one value:
final_data <- drop_columns(final_data, c("year_flights", "tz_origin"))


plot_boxplot(final_data[,c("origin", "dep_delay", "arr_delay", "air_time", "year_planes", "seats")], by = "name_carrier")


# 5. COrrelation Analysis
# Choose to visualize only discrete/continuous features with:
plot_correlation(na.omit(final_data), type = "c")
plot_correlation(na.omit(final_data), type = "d")


# 6. Principal Componant Anaylsis
pca_df <- na.omit(final_data[, c("origin", "dep_delay", "arr_delay", "air_time", "year_planes", "seats")])
plot_prcomp(pca_df, variance_cap = 0.9, nrow = 2L, ncol = 2L)
