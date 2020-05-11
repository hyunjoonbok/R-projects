### === USE H2O === ###
## Main Usage ##
# 1) Data Manipulation: Merging, grouping, pivoting, imputing, splitting into training/test/validation sets, etc.

# 2) Machine Learning Algorithms: Very sophisiticated algorithms in both supervised and unsupervised categories. Supervised include deep learning (neural networks), random forest, generalized linear model, gradient boosting machine, naive bayes, stacked ensembles, and xgboost. Unsupervised include generalized low rank models, k-means and PCA. There's also Word2vec for text analysis. The latest stable release also has AutoML: automatic machine learning, which is really cool as we'll see in this post!
  
# 3) Auxiliary ML Functionality Performance analysis and grid hyperparameter search

# 4) Production, Map/Reduce and Cloud: Capabilities for productionizing into Java environments, cluster deployment with Hadoop / Spark (Sparkling Water), deploying in cloud environments (Azure, AWS, Databricks, etc)

# 1. Load libraries
require(h2o)        # Awesome ML Library
require(timetk)     # Toolkit for working with time series in R
require(tidyquant)  # Loads tidyverse, financial pkgs, used to get data
require(ggplot2)
require(tidyverse)

# 2. Load Data
beer_sales_tbl <- tq_get("S4248SM144NCEN", get = "economic.data", from = "2010-01-01", to = "2018-03-31")
beer_sales_tbl

# Plot Beer Sales with train, validation, and test sets shown
beer_sales_tbl %>%
  ggplot(aes(date, price)) +
  # Train Region
  annotate("text", x = ymd("2012-01-01"), y = 7000,
           color = palette_light()[[1]], label = "Train Region") +
  # Validation Region
  geom_rect(xmin = as.numeric(ymd("2016-07-01")), 
            xmax = as.numeric(ymd("2017-06-31")),
            ymin = 0, ymax = Inf, alpha = 0.02,
            fill = palette_light()[[3]]) +
  annotate("text", x = ymd("2016-07-01"), y = 7000,
           color = palette_light()[[1]], label = "Validation\nRegion") +
  # Test Region
  geom_rect(xmin = as.numeric(ymd("2017-07-01")), 
            xmax = as.numeric(ymd("2018-02-31")),
            ymin = 0, ymax = Inf, alpha = 0.02,
            fill = palette_light()[[4]]) +
  annotate("text", x = ymd("2017-05-01"), y = 7000,
           color = palette_light()[[1]], label = "Test\nRegion") +
  # Data
  geom_line(col = palette_light()[1]) +
  geom_point(col = palette_light()[1]) +
  geom_ma(ma_fun = SMA, n = 12, size = 1) +
  # Aesthetics
  theme_tq() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Beer Sales: 2007 through 2017",
       subtitle = "Train, Validation, and Test Sets Shown") 



## TIME SERIES MACHINE LEARNING Start


# 1) Review the data
beer_sales_tbl %>% glimpse()



# 2) AUGMENT TIME SERIES SIGNATURE
beer_sales_tbl_aug <- beer_sales_tbl %>%
  tk_augment_timeseries_signature()

beer_sales_tbl_aug %>% glimpse()


# 3) PREP THE DATA FOR H2O
# Remove any unnecessary columns such as dates or those with missing values, 
# and change the ordered classes to plain factor
beer_sales_tbl_clean <- beer_sales_tbl_aug %>%
  select_if(~ !is.Date(.)) %>%
  select_if(~ !any(is.na(.))) %>%
  mutate_if(is.ordered, ~ as.character(.) %>% as.factor)

beer_sales_tbl_clean %>% glimpse()

# Split into training, validation and test sets
train_tbl <- beer_sales_tbl_clean %>% filter(year < 2016)
valid_tbl <- beer_sales_tbl_clean %>% filter(year == 2016)
test_tbl  <- beer_sales_tbl_clean %>% filter(year == 2017)


# 4) MODEL WITH H2O
h2o.init(nthreads = -1) # -1 to use all available thread

# change our data to an H2OFrame object
train_h2o <- as.h2o(train_tbl)
valid_h2o <- as.h2o(valid_tbl)
test_h2o  <- as.h2o(test_tbl)

# Set names for h2o
y <- "price"
x <- setdiff(names(train_h2o), y)

# linear regression model used, but can use any model
automl_models_h2o <- h2o.automl(
  x = x, 
  y = y, 
  training_frame = train_h2o, 
  validation_frame = valid_h2o, 
  leaderboard_frame = test_h2o, 
  max_runtime_secs = 60, 
  stopping_metric = "deviance") #logloss,auc?

# Extract leader model
automl_models_h2o@leaderboard
#head(automl_models_h2o@leaderboard,23)
automl_leader <- automl_models_h2o@leader



# Predict 
pred_h2o <- h2o.predict(automl_leader, newdata = test_h2o)

# EVALUATE PERFORMANCE
h2o.performance(automl_leader, newdata = test_h2o)

# Investigate test error (MAPE)
error_tbl <- beer_sales_tbl %>% 
  filter(lubridate::year(date) == 2017) %>%
  add_column(pred = pred_h2o %>% as.tibble() %>% pull(predict)) %>%
  rename(actual = price) %>%
  mutate(
    error     = actual - pred,
    error_pct = error / actual
  ) 
error_tbl

error_tbl %>%
  summarise(
    me   = mean(error),
    rmse = mean(error^2)^0.5,
    mae  = mean(abs(error)),
    mape = mean(abs(error_pct)),
    mpe  = mean(error_pct)
  ) %>%
  glimpse()
