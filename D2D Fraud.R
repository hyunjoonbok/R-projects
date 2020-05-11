# Connecting to Database
require("DBI")
require("odbc")
require("RMySQL")
sort(unique(odbcListDrivers()[[1]]))

drv = dbDriver("MySQL")

con <- dbConnect(RMySQL::MySQL(), dbname = "d2d_analyze", username = "analytic", 
                 password = "atgames1234", host = "localhost")


con <- dbConnect(RMySQL::MySQL(), dbname = "d2d_analyze", username = "analytic", 
                 password = "atgames1234", host = "localhost")

dbListTables(con)

setwd("C:/Users/bokhy/Documents/R-code-storage")
require(tidyverse)
require(lubridate)
require(caret)
require(h2o)
require(corrplot)

# 1. EDA & Data cleaning====

transaction <- read.csv("transaction_product_item.csv")
glimpse(transaction)


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


transaction$title <- as.factor(transaction$title)

# omit NAs
transaction <- na.omit(transaction)


# Solve class imbalance

# 1. oversampling for better sensitivity (probabilty of correctly guessing out "refunded")
require(ROSE)

both <- ROSE::ovun.sample(order_state ~ ., data = transaction, method = "both",
                          p = 0.5, seed = 0623, N = 17990)$data
table(both$order_state)

rose_eval <- ROSE.eval(order_state ~., data = transaction, learner = glm,
                      method.assess = "BOOT", control.learner = list(family=binomial) ,
                      trace = TRUE, seed = 0623, B=10, acc.measure = "precision")
table(rose_new$order_state)

# Feature Selection
require(Boruta)
set.seed(0623)

boruta_new <- Boruta(order_state ~., data = transaction, doTrace = 2)

plot(boruta_new, las = 2, cex.axis = 0.6)
# all varialbes seems important
Boruta::getSelectedAttributes(boruta_new)


# =====

x <- cbind(transaction$at_games_price, transaction$coupons, transaction$gross_revenue,
           transaction$net_revenue, transaction$vip_discounts, transaction$charge_amount, transaction$sold_price)

require(psych)

pairs.panels(x, gap = 0, bg = c("red", "yellow")[transaction$order_state], pch = 21)


summary(x)
# If data are highly correlated, its perfect to use PCA
# use correction to find PCA, manitude of variables are all different
cor(x)

# PCA
pcal <- princomp(x, scores = TRUE, cor = TRUE)
attributes(pcal)
summary(pcal)
# Usually use the ones whose proportion of variance is above 0.95
# Usually use the ones whose standard devation is above one



# correlations between variables are now 0 !!
pairs.panels(pcal$scores, gap = 0, bg = c("red", "yellow")[transaction$order_state], pch = 21)


PCA_numbers <- data.frame(pcal$scores , transaction$order_state)


# paste PCA numbers into origianl ones
transaction$at_games_price <- pcal$scores[,1]
transaction$coupons <- pcal$scores[,2]
transaction$gross_revenue <- pcal$scores[,3]
transaction$net_revenue <- pcal$scores[,4]
transaction$vip_discounts <- pcal$scores[,5]
transaction$charge_amount <- pcal$scores[,6]
transaction$sold_price <- pcal$scores[,7]




# 2.0 Machine LEarning ====


h2o.init(nthreads = -1, max_mem_size = "32g")
transaction_h2o <- as.h2o(transaction)
# Create training, validation, and testing splits
splits <- h2o.splitFrame(transaction_h2o, ratios = c(0.7,0.15), seed = 0623)
names(splits) <- c("train","valid","test")



# GBM tuning





## find the range of max_depth for the top 5 models
topDepths = sortedGrid@summary_table$max_depth[1:5]
minDepth = min(as.numeric(topDepths))
maxDepth = max(as.numeric(topDepths))



hyper_params = list(
  ## restrict the search to the range of max_depth established above
  max_depth = seq(minDepth, maxDepth, 1),
  ## search a large space of row sampling rates per tree
  sample_rate = seq(0.2,1,0.01),
  ## search a large space of column sampling rates per split
  col_sample_rate = seq(0.2,1,0.01),
  ## search a large space of column sampling rates per tree
  col_sample_rate_per_tree = seq(0.2,1,0.01),
  ## search a large space of how column sampling per split should change as a function of the depth of the split
  col_sample_rate_change_per_level = seq(0.9,1.1,0.01),
  ## search a large space of the number of min rows in a terminal node
  min_rows = 2^seq(0,log2(nrow(train)) - 1,1),
  ## search a large space of the number of bins for split-finding for continuous and integer columns
  nbins = 2^seq(4,10,1),
  ## search a large space of the number of bins for split-finding for categorical columns
  nbins_cats = 2^seq(4,12,1),
  ## search a few minimum required relative error improvement thresholds for a split to happen
  min_split_improvement = c(0,1e-8,1e-6,1e-4),
  ## try all histogram types (QuantilesGlobal and RoundRobin are good for numeric columns with outliers)
  histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin")
)


search_criteria = list(
  ## Random grid search
  strategy = "RandomDiscrete",
  ## limit the runtime to 60 minutes
  max_runtime_secs = 3600,
  ## build no more than 100 models
  max_models = 100,
  ## random number generator seed to make sampling of parameter combinations reproducible
  seed = 1234,
  ## early stopping once the leaderboard of the top 5 models is converged to 0.1% relative difference
  stopping_rounds = 5,
  stopping_metric = "AUC",
  stopping_tolerance = 1e-3
)
grid <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,
  ## hyper-parameter search configuration (see above)
  search_criteria = search_criteria,
  ## which algorithm to run
  algorithm = "gbm",
  ## identifier for the grid, to later retrieve it
  grid_id = "final_grid",
  ## standard model parameters
  x = x1,
  y = y1,
  training_frame = splits$train,
  validation_frame = splits$valid,
  ## more trees is better if the learning rate is small enough
  ## use "more than enough" trees - we have early stopping
  ntrees = 1000,
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.05,
  ## learning rate annealing: learning_rate shrinks by 1% after every tree
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.99,
  ## early stopping based on timeout (no model should take more than 1 hour - modify as needed)
  max_runtime_secs = 3600,
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "AUC",
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10,
  ## base random number generator seed for each model (automatically gets incremented internally for each model)
  seed = 1234
)
## Sort the grid models by AUC
sortedGrid <- h2o.getGrid("final_grid", sort_by = "auc", decreasing = TRUE)
sortedGrid

# Let’s see how well the best model of the grid search (as judged by validation set AUC) does on the held out test set:
gbm <- h2o.getModel(sortedGrid@model_ids[[1]])
gbm
h2o.performance(gbm, newdata = splits$test)
print(h2o.auc(h2o.performance(gbm, newdata = splits$test)))




# =====


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
    seed = 0623,
    x = x1, 
    y = y1, 
    training_frame = splits$train,
    validation_frame = splits$valid,
    stopping_metric = "AUTO",
    stopping_tolerance = 1e-2,
    stopping_rounds = 5,
    max_runtime_secs = 720,
    nfolds = 10) 


leader_6 <- aml@leader
leader_6
# EVALUATE PERFORMANCE
perf16 <- h2o.performance(leader_6, newdata = splits$test)
perf16

h2o.sensitivity(perf16)
# Predict 
pred16 <- h2o.predict(leader_6, newdata = splits$test)
refund_prediction <- as.data.frame(pred16)
# show how much each base learner is contributin go the ensemble



h2o.varimp(leader_6)
# plot the base learner contribution
h2o.varimp_plot(leader_6)



# save the model
model_path <- h2o.saveModel(object=leader_6, path=getwd(), force=TRUE)

print(model_path)

# load the model
saved_model <- h2o.loadModel(model_path)
