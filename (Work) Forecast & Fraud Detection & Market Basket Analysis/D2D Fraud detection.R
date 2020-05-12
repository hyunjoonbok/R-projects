setwd("C:/Users/bokhy/Documents/R-code-storage")
require(tidyverse)
require(lubridate)
require(caret)
require(h2o)
require(corrplot)
require(correlationfunnel)

# 1. EDA & Data cleaning====

transaction <- read.csv("transaction_product_item.csv")
glimpse(transaction)

transaction$publisher_id <- as.numeric(transaction$publisher_id)


# =======

transaction$vip_discount <- as.factor(transaction$vip_discount)
transaction <- transaction[,-c(3,6,17)]

transaction$payment_date <- as.POSIXct(strptime(transaction$payment_date, "%Y-%m-%d %H:%M:%S"))
transaction$payment_Year <- as.factor(lubridate::year(transaction$payment_date))
transaction$payment_Month <- as.factor(lubridate::month(transaction$payment_date))
transaction$payment_Date <- as.factor(lubridate::day(transaction$payment_date))
transaction$payment_Hour <- as.factor(lubridate::hour(transaction$payment_date))
transaction$payment_Minute <- as.factor(lubridate::minute(transaction$payment_date))
transaction$payment_Second <- as.factor(lubridate::second(transaction$payment_date))
transaction$payment_Weeknum <- as.factor(lubridate::week(transaction$payment_date))

transaction <- transaction[,-c(7,10,12,13)]

transaction$order_state[transaction$order_state == "CHARGED_BACK"] <- "REFUNDED"

# Checking only recent year's purchase (Xsolla, Stripe, PayPal)
transaction <- transaction %>% filter(type != "LivegamerPayment")

# Make one consolidated title name for all three regions (delte UK,EU specification)
transaction$title <- gsub("\\{EU}","",transaction$title)
transaction$title <- gsub("\\{UK}","",transaction$title)

# Adding newly created variables

transaction$sold_price <- transaction$gross_revenue + transaction$vip_discounts



# data cleaning
 #.1 At_Games_price 
# It's right skewed --> sqaure-root transformation
ggplot(transaction, aes(x = at_games_price)) +
       geom_histogram() 

transaction$at_games_price <- sqrt(transaction$at_games_price)

#.2 gross_revenue
# It's right skewed --> sqaure-root transformation
ggplot(transaction, aes(x = gross_revenue)) +
  geom_histogram()

transaction$gross_revenue <- sqrt(transaction$gross_revenue)



#.3 net_revenue
# It's right skewed --> sqaure-root transformation
ggplot(transaction, aes(x = net_revenue)) +
  geom_histogram()

transaction$net_revenue <- sqrt(transaction$net_revenue)


ggplot(transaction, aes(x = order_state)) +
  geom_bar()



# sold price
transaction$sold_price <- sqrt(transaction$sold_price)

# omit NAs
transaction <- na.omit(transaction)



# Feature Selection
require(Boruta)
set.seed(0623)

boruta_new <- Boruta(order_state ~., data = transaction, doTrace = 2)

plot(boruta_new, las = 2, cex.axis = 0.6)
# all varialbes seems important
Boruta::getSelectedAttributes(boruta_new)


x <- cbind(transaction$at_games_price, transaction$coupons, transaction$gross_revenue,
           transaction$net_revenue, transaction$vip_discounts, transaction$charge_amount, transaction$sold_price)

require(psych)


summary(x)
# If data are highly correlated, its perfect to use PCA
# use correction to find PCA, manitude of variables are all different
cor(x)

# PCA
pcal <- princomp(x, scores = TRUE, cor = TRUE)
summary(pcal)
# Cumulative proportiona is important!
# Usually use the ones whose standard devation is above one

#confirm the ones whos Eigenvalues are above one
plot(pcal)




# 2.0 Machine LEarning using H20 ====


h2o.init(nthreads = -1, max_mem_size = "32g")
transaction <- as.h2o(transaction)
# Create training, validation, and testing splits
splits <- h2o.splitFrame(transaction, ratios = c(0.7,0.15), seed = 0623)
names(splits) <- c("train","valid","test")

hyper_params <- list(
  activation = c("Rectifier","Tanh","Maxout","RectifierWithDropout","TanhWithDropout","MaxoutWithDropout"),
  hidden = list(c(20,20),c(50,50),c(30,30,30),c(25,25,25,25),c(200,200),c(512,512),c(128,128,128),c(512,512,512),c(128,128,128,128)),
  input_dropout_ratio = c(0,0.01,0.05,0.1,0.2,0.4,0.5),
  l1 = seq(0,1e-4,1e-6), # add regularization
  l2 = seq(0,1e-4,1e-6),
  rate = c(0.01,0.02,0.04,0.05),
  rate_annealing = c(2e-6,2e-7),
  momentum_start = c(0.1,0.2,0.5),
  momentum_stable = c(0.1,0.4,0.5,0.99)
)
# Set criteria for the model to be evaluated
search_criteria = list(seed = 0623, strategy = "RandomDiscrete", max_runtime_secs = 360, max_models = 100)

# Set Response / Predictor variables
y1 <- "order_state"
x1 <- setdiff(names(transaction),y1);x1

# Random Hyper-parameter Search
random_grid <- h2o.grid(
  algorithm = "deeplearning",
  grid_id = "order_state",
  training_frame = splits$train,
  validation_frame = splits$valid,
  x = x1,
  y = y1,
  nfolds = 10, # 10-fold cross-validation on training-set
  epochs = 1000, # enough epoch when early-stoppting is enabled
  stopping_metric = "AUTO", # Auto-detect stopping metric (i.e logloss, misclassification, rmse, etc)
  stopping_tolerance = 1e-2, # If logloss does not improve by 1% 
  stopping_rounds = 5, # For 5 consecutive events
  max_w2 = 10, # Improve stability 
  hyper_params = hyper_params,
  search_criteria = search_criteria
)
# Get the best-grid
grid1 <- h2o.getGrid("order_state",sort_by = "logloss",decreasing = FALSE)
# Get the model with the best grid-search result
best_model1 <- h2o.getModel(grid1@model_ids[[1]])
# Look for the performance in our test_set
perf1 <- h2o.performance(model = best_model1, newdata = splits$test)
perf1
# Predict results
pred1 <- h2o.predict(best_model1, newdata = splits$test)
pred_order_state <- as.data.frame(pred1)



 ============================================================
  
  
  #4.2 AutoML
aml <- h2o.automl(
  seed = 7,
  x = x1, 
  y = y1, 
  training_frame = splits$train,
  validation_frame = splits$valid,
  stopping_metric = "logloss",
  stopping_tolerance = 1e-2,
  stopping_rounds = 5,
  max_runtime_secs = 720,
  nfolds = 10) 


leader_6 <- aml@leader
leader_6
# EVALUATE PERFORMANCE
perf16 <- h2o.performance(leader_6, newdata = splits$test)
perf16
# Predict 
pred16 <- h2o.predict(leader_6, newdata = splits$test)
aaa <- as.data.frame(pred16)
# show how much each base learner is contributin go the ensemble


h2o.varimp(leader_6)
# plot the base learner contribution
h2o.varimp_plot(leader_6)


# 3.0 Explainable Machine Learning using IML ====
# Implement IML

# 4.0 IML Explanation ====
# 4.1 IML set-up

features_tbl <- train_set %>% select(-Churn) 

response_vec <- train_set %>% pull(Churn) %>% as.numeric() - 1

predict_h2o <- function(model, newdata) {
  results_tbl <- h2o.predict(model, newdata = as.h2o(newdata)) %>% as.tibble()
  results_tbl %>% pull(Yes)
}

predidct_h2o(h2o_rf, newdata = test_data)


# 4.2 PRedicto object

preidctor_rf <- Predictor$new(
  model = h2o_rf,
  data = features_tbl,
  y = response_vec,
  predict.fun = predict_h2o,
  class = "classification"
)
preidctor_rf

# Global Model Explanation

funnel_churn_ggplot

# 5.0 PDP 

# single feature - "contact Type_

pdp_contract <- FeatureEffect$new(
  predictor = preidctor_rf,
  feature = "Contract",
  method = "pdp",
  grid.size = 20
)

pdp_contract %>% 
  plot() + expand_limits(y = 0) # now see their is a different where my RF results out 

# 2-way interactions "contact type & monthyl charges"
# Tip = Increase grid size !

tic()
pdp_monthly_charges_by_contract <- FeatureEffect$new(
  predictor = preidctor_rf,
  feature = c("Contact", "MonthlyChargnes"),
  method = "pdp",
  grid.size = 10
)
toc()

pdp_monthly_charges_by_contract %>% 
  plot(rug = TRUE) + 
  expand_limits(y = 0) +
  theme_tq() +
  scale_color_tq()


# 6.0 ICE 

ice_contact_monthlys <- FeatureEffect$new(
  predictor = preidctor_rf,
  feature = c("Contact", "MonthlyChargnes"),
  method = "ice",
  grid.size = 10,
  center.at = 0 # THis centers every at 0. So it gives more clear visual
)
# IT shows the distributino of customers!

ice_contact_monthlys %>% 
  plot() +
  geom_smooth(color = palette_light(){"green"}, size = 2) +
  expand_limits( y= 0)


# SHAP
shapley_rf <- Shapley$new(
  predictor = predictor_rf,
  x.interest = test_set %>% slice(2) %>% select(-Churn), # for customer # 2
  sample.size = 200
)

shapey_rf %>%  plot() +theme_tq()

# Pro Tips!

breakdown_h2o_leader <- break_down(
  x= explainer_h2o_leader,
  test_set %>% slice(2) %>% select(-Churn),
  interactions = FALSE
)

plot(breakdown_h2o_leader, max_features = 4)
