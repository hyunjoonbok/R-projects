# DEEP LEARNING WITH KERAS TO PREDICT CUSTOMER CHURN #
# Load Data
setwd("C:/Users/bokhy/Desktop/R")
Telco <- read.csv("Telco-Customer-Churn.csv")
str(Telco)
# Load libraries
require(reticulate)
require(keras)
require(tensorflow)
require(lime)
require(tidyquant)
require(rsample)
require(recipes)
require(yardstick)
require(corrr)
require(caret)
require(knitr)



# PREPROCESS DATA
# 1. Prune (removing unnecessary columns and rows) the data 
Telco_pruned <- Telco %>%
  select(-customerID) %>%
  drop_na() %>%
  select(Churn, everything())
str(Telco_pruned)


# 2. SPLIT INTO TRAIN/TEST SETS
# Split test/training sets
set.seed(0623)
inTrainingSet <- createDataPartition(Telco_pruned$Churn, p = 0.75, list = FALSE)
train_tbl <- Telco_pruned[inTrainingSet,]
test_tbl <- Telco_pruned[-inTrainingSet,]



# 3. TRANSFORM THE "TOTALCHARGES" FEATURE
# What we don't like to see is when a lot of observations are bunched within a small part of the range.
# We want to see some cohort that would help the analysis
plot(Telco_pruned$TotalCharges)
# Determine if log-transforma improves correlation 
# between TotalCharges and Churn
train_tbl %>%
  select(Churn, TotalCharges) %>%
  mutate(
    Churn = Churn %>% as.factor() %>% as.numeric(),
    LogTotalCharges = log(TotalCharges)
  ) %>%
  correlate() %>%
  focus(Churn) %>%
  fashion()
# The correlation between "Churn" and "LogTotalCharges" is greatest in magnitude indicating the log transformation should improve the accuracy of the ANN model we build. 
# Therefore, we should perform the log transformation.



# 4. PREPROCESSING WITH RECIPES

# STEP 1: Create recipe
rec_obj <- recipe(Churn ~ ., data = train_tbl) %>%
  step_discretize(tenure, options = list(cuts = 6)) %>% # cut continuous variable to group customers into 6 groups
  step_log(TotalCharges) %>% # log-transfrom TotalCharges
  step_dummy(all_nominal(), -all_outcomes()) %>% # One-hot encode categorical data
  step_center(all_predictors(), -all_outcomes()) %>% # center data
  step_scale(all_predictors(), -all_outcomes()) %>% # scale data
  prep(data = train_tbl)

rec_obj
### Tip: We can save the recipe object as an RDS file using saveRDS(), and then use it to bake() (discussed next) future raw data into ML-ready data in production! ==##
### saveRDS(rec_obj)

# STEP 2: BAKING WITH YOUR RECIPE
# convert training and testing data to a machine learning dataset

# Predictors
x_train_tbl <- bake(rec_obj, newdata = train_tbl)
x_test_tbl  <- bake(rec_obj, newdata = test_tbl)

x_train_tbl <- x_train_tbl[,-1]
x_test_tbl <- x_test_tbl[,-1]

glimpse(x_train_tbl)

# STEP 3: DON'T FORGET THE TARGET
# we need to store the actual values (truth) as y_train_vec and y_test_vec, which are needed for modeling our ANN
# Response variables for training and testing sets
y_train_vec <- ifelse(pull(train_tbl, Churn) == "Yes", 1, 0)
y_test_vec  <- ifelse(pull(test_tbl, Churn) == "Yes", 1, 0)





# 5. MODELING
# Building our Artificial Neural Network
model_keras <- keras_model_sequential()

model_keras %>% 
  # First hidden layer
  layer_dense(
    units              = 16, # the number of nodes
    kernel_initializer = "uniform", 
    activation         = "relu", 
    input_shape        = ncol(x_train_tbl)) %>% 
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>% # remove weights below 10%.
  # Second hidden layer
  layer_dense(
    units              = 16, 
    kernel_initializer = "uniform", 
    activation         = "relu") %>% 
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%
  # Output layer
  layer_dense(
    units              = 1, 
    kernel_initializer = "uniform", 
    activation         = "sigmoid") %>% 
  # Compile ANN
  compile(
    optimizer = 'adam',
    loss      = 'binary_crossentropy',
    metrics   = c('accuracy')
  )
model_keras

# Run tensorboard

tensorboard("logs/run_a")

# Fit the keras model to the training data
fit_keras <- model_keras %>% fit(
  x                = as.matrix(x_train_tbl), 
  y                = y_train_vec,
  batch_size       = 50, # Typically we want to keep the batch size high since this decreases the error
  epochs           = 35, # We want epochs to be large, which is important in visualizing the training history
  validation_split = 0.30,
  callbacks = callback_tensorboard("logs/run_a")
)
fit_keras

# Plot the training/validation history of our Keras model
plot(fit_keras) +
  theme_tq() +
  scale_color_tq() +
  scale_fill_tq() +
  labs(title = "Deep Learning Training Results")



# 6. MAKING EVALUATION

# Evaluate the model's performance on the test data
score <- model_keras %>% evaluate(as.matrix(x_test_tbl), y_test_vec)
score




# 7. MAKING PREDICTIONS

# Predicted Class
yhat_keras_class_vec <- predict_classes(object = model_keras, x = as.matrix(x_test_tbl)) %>%
  as.vector()
yhat_keras_class_vec
# Predicted Class Probability
yhat_keras_prob_vec  <- predict_proba(object = model_keras, x = as.matrix(x_test_tbl)) %>%
  as.vector()



# 8. INSPECT PERFORMANCE WITH YARDSTICK

# Format test data and predictions for yardstick metrics
estimates_keras_tbl <- tibble(
  truth      = as.factor(y_test_vec) %>% fct_recode(yes = "1", no = "0"),
  estimate   = as.factor(yhat_keras_class_vec) %>% fct_recode(yes = "1", no = "0"),
  class_prob = yhat_keras_prob_vec
)
estimates_keras_tbl

options(yardstick.event_first = FALSE)
# the default is to classify 0 as the positive class instead of 1

# Confusion Table
estimates_keras_tbl %>% conf_mat(truth, estimate)

# IMPORTNAT METRICS #
# 1) Accuracy
estimates_keras_tbl %>% metrics(truth, estimate)

# 2) AUC
estimates_keras_tbl %>% roc_auc(truth, class_prob)

# 3) Precision
# Precision(true positive rate or specificity) & Recall
tibble(
  precision = estimates_keras_tbl %>% precision(truth, estimate),
  recall    = estimates_keras_tbl %>% recall(truth, estimate)
)



# 9. EXPLAIN THE MODEL WITH LIME

class(model_keras)
# We'll need to make two custom functions:
  # model_type: Used to tell lime what type of model we are dealing with. It could be classification, regression, survival, etc.
  # predict_model: Used to allow lime to perform predictions that its algorithm can interpret.

# Setup lime::model_type() function for keras
model_type.keras.models.Sequential <- function(x, ...) {
  return("classification")
}
# Setup lime::predict_model() function for keras
predict_model.keras.models.Sequential <- function(x, newdata, type, ...) {
  pred <- predict_proba(object = x, x = as.matrix(newdata))
  return(data.frame(Yes = pred, No = 1 - pred))
}
# Test our predict_model() function
predict_model.keras.models.Sequential(x = model_keras, newdata = x_test_tbl, type = 'raw') %>%
  tibble::as_tibble()

# Run lime() on training set
explainer <- lime(
  x              = x_train_tbl, 
  model          = model_keras, 
  bin_continuous = FALSE)
explainer

# Run explain() on explainer
explanation <- explain(
  x_test_tbl[1:10,], # This can take a minute, so limit it to just the first ten rows
  explainer    = explainer, 
  n_labels     = 1, # explaining a single class
  n_features   = 4, # the top four features that are critical to each case
  kernel_width = 0.5) # increase the "model_r2" value by shrinking the localized evaluation
explanation



# 10. FEATURE IMPORTANCE VISUALIZATION
# This allows us to visualize each of the first ten case
# and The top four features for each case are shown. 
# The green bars mean that the feature supports the model conclusion, and the red bars contradict!!
plot_features(explanation) +
  labs(title = "LIME Feature Importance Visualization",
       subtitle = "Hold Out (Test) Set, First 10 Cases Shown")

plot_explanations(explanation) +
  labs(title = "LIME Feature Importance Heatmap",
       subtitle = "Hold Out (Test) Set, First 10 Cases Shown")

# 11. CHECK EXPLANATIONS WITH CORRELATION ANALYSIS
# One thing we need to be careful with the LIME visualization is that we are only doing a sample of the data, in our case the first 10 test observations. 
# Therefore, we are gaining a very localized understanding of how the ANN works. 
# However, we also want to know on from a global perspective what drives feature importance.
# Feature correlations to Churn
corrr_analysis <- x_train_tbl %>%
  mutate(Churn = y_train_vec) %>%
  correlate() %>%
  focus(Churn) %>%
  rename(feature = rowname) %>%
  arrange(abs(Churn)) %>%
  mutate(feature = as_factor(feature)) 
corrr_analysis

# The correlation visualization helps in distinguishing which features are relavant to Churn
corrr_analysis %>%
  ggplot(aes(x = Churn, y = fct_reorder(feature, desc(Churn)))) +
  geom_point() +
  # Positive Correlations - Contribute to churn
  geom_segment(aes(xend = 0, yend = feature), 
               color = palette_light()[[2]], 
               data = corrr_analysis %>% filter(Churn > 0)) +
  geom_point(color = palette_light()[[2]], 
             data = corrr_analysis %>% filter(Churn > 0)) +
  # Negative Correlations - Prevent churn
  geom_segment(aes(xend = 0, yend = feature), 
               color = palette_light()[[1]], 
               data = corrr_analysis %>% filter(Churn < 0)) +
  geom_point(color = palette_light()[[1]], 
             data = corrr_analysis %>% filter(Churn < 0)) +
  # Vertical lines
  geom_vline(xintercept = 0, color = palette_light()[[5]], size = 1, linetype = 2) +
  geom_vline(xintercept = -0.25, color = palette_light()[[5]], size = 1, linetype = 2) +
  geom_vline(xintercept = 0.25, color = palette_light()[[5]], size = 1, linetype = 2) +
  # Aesthetics
  theme_tq() +
  labs(title = "Churn Correlation Analysis",
       subtitle = "Positive Correlations (contribute to churn), Negative Correlations (prevent churn)",
       y = "Feature Importance")

# We can see that the following features are highly correlated (magnitude > 0.25)




# 12. FEATURE INVESTIGATION
# We can investigate features that are most frequent in the LIME feature importance visualization + correlation analysis
churn_data_raw <- Telco_pruned
# Tenure
# it appears that customers with lower tenure (bin 1) are more likely to leave. 
# Opportunity: Target customers with less than 12 month tenure.
churn_data_raw %>%
  ggplot(aes(x = Churn, y = tenure)) +
  geom_jitter(alpha = 0.25, color = palette_light()[[6]]) +
  geom_violin(alpha = 0.6, fill = palette_light()[[1]]) +
  theme_tq() +
  labs(
    title = "Tenure",
    subtitle = "Customers with lower tenure are more likely to leave"
  )


# Contract
churn_data_raw %>%
  mutate(Churn = ifelse(Churn == "Yes", 1, 0)) %>%
  ggplot(aes(x = as.factor(Contract), y = Churn)) +
  geom_jitter(alpha = 0.25, color = palette_light()[[6]]) +
  geom_violin(alpha = 0.6, fill = palette_light()[[1]]) +
  theme_tq() +
  labs(
    title = "Contract Type",
    subtitle = "Two and one year contracts much less likely to leave",
    x = "Contract"
  )

# Internet Service
churn_data_raw %>%
  mutate(Churn = ifelse(Churn == "Yes", 1, 0)) %>%
  ggplot(aes(x = as.factor(InternetService), y = Churn)) +
  geom_jitter(alpha = 0.25, color = palette_light()[[6]]) +
  geom_violin(alpha = 0.6, fill = palette_light()[[1]]) +
  theme_tq() +
  labs(
    title = "Internet Service",
    subtitle = "Fiber optic more likely to leave",
    x = "Internet Service"
  )

# Payment Method
churn_data_raw %>%
  mutate(Churn = ifelse(Churn == "Yes", 1, 0)) %>%
  ggplot(aes(x = as.factor(PaymentMethod), y = Churn)) +
  geom_jitter(alpha = 0.25, color = palette_light()[[6]]) +
  geom_violin(alpha = 0.6, fill = palette_light()[[1]]) +
  theme_tq() +
  labs(
    title = "Payment Method",
    subtitle = "Electronic check more likely to leave",
    x = "Payment Method"
  )

# Senior Citizen
churn_data_raw %>%
  mutate(Churn = ifelse(Churn == "Yes", 1, 0)) %>%
  ggplot(aes(x = as.factor(SeniorCitizen), y = Churn)) +
  geom_jitter(alpha = 0.25, color = palette_light()[[6]]) +
  geom_violin(alpha = 0.6, fill = palette_light()[[1]]) +
  theme_tq() +
  labs(
    title = "Senior Citizen",
    subtitle = "Non-senior citizens less likely to leave",
    x = "Senior Citizen (Yes = 1)"
  )

# Online Security
churn_data_raw %>%
  mutate(Churn = ifelse(Churn == "Yes", 1, 0)) %>%
  ggplot(aes(x = OnlineSecurity, y = Churn)) +
  geom_jitter(alpha = 0.25, color = palette_light()[[6]]) +
  geom_violin(alpha = 0.6, fill = palette_light()[[1]]) +
  theme_tq() +
  labs(
    title = "Online Security",
    subtitle = "Customers without online security are more likely to leave"
  )




# 12. We need to implement K-Fold Cross Validation and Hyper Parameter Tuning if we want a best-in-class model @@@



