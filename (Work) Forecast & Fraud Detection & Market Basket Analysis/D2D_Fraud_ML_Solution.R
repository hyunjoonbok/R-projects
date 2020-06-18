require("DBI")
require("odbc")
require("RMySQL")
require(tidyverse)
require(lubridate)
require(caret)
require(h2o)
require(corrplot)
require(correlationfunnel)
library(GGally)
require(Boruta)
require(skimr)
require(mice)
require(tidyquant)
require(rsample)
require(recipes)
require(iml)
require(DALEX)
require(correlationfunnel)
require(DataExplorer)
require(tictoc)
require(xgboost)
require(mlbench)
require(lime)
require(yardstick)
require(corrr)
require(knitr)
require(Matrix)
#library(tidymodels)  
library(dplyr, warn.conflicts = FALSE)

#transaction <- read_csv("aa.csv",col_names = TRUE)

setwd("C:/Users/bokhy/Desktop/ATG")

# 1. Data cleaning ====
transaction <- read_csv("d2d_transactions.csv",col_names = TRUE)

table(transaction$order_state)
transaction$order_state[transaction$order_state == "CHARGED_BACK"] <- "REFUNDED"
transaction$order_state[transaction$order_state == "DUPLICATED_REFUND"] <- "REFUNDED"
transaction$order_state[transaction$order_state == "DUPLICATED_REFUNDED"] <- "REFUNDED"
transaction$order_state[transaction$order_state == "SUBMITTED"] <- "CHARGED"

table(transaction$currency_full_name)
transaction$currency_full_name[transaction$currency_full_name == "Euro Member Countries"] <- "EUR"
transaction$currency_full_name[transaction$currency_full_name == "United Kingdom Pound"] <- "GBP"
transaction$currency_full_name[transaction$currency_full_name == "United States Dollar"] <- "USD"

table(transaction$company_name)
transaction$company_name[transaction$company_name == "1C Company"] <- "1C Entertainment"
transaction$company_name[transaction$company_name == "Axis Game Factory, Inc."] <- "Axis Game Factory LLC"
transaction$company_name[transaction$company_name == "Electronic Arts UK"] <- "Electronic Arts"
transaction$company_name[transaction$company_name == "Koei Tecmo"] <- "Koei"
transaction$company_name[transaction$company_name == "Private Division"] <- "TakeTwo Private Division"
transaction$company_name[transaction$company_name == "THQ Nordic"] <- "THQ Nordic GmbH"
transaction$company_name[transaction$company_name == "Ubisoft Entertainment"] <- "Ubisoft"
transaction$company_name[transaction$company_name == "Team 17"] <- "Team17"



# Filter-out incomplete transactions
transaction <- transaction %>% 
  # Too old transactions
  filter(!type == 'LivegamerPayment') %>% 
  # incomplete transactions
  filter(order_state == 'CHARGED' | order_state == 'REFUNDED') %>% 
  filter(!company_name == 'AtGames Test Publisher 1') %>% 
  filter(!company_name == 'AtGames Test Publisher 2') %>% 
  filter(!company_name == 'AtGames Test Publisher 3') %>% 
  filter(!company_name == 'Ravenscourt - DUPLICATE') %>% 
  select(title, company_name, currency_full_name, country_full_name,
         msrp, publisher_price, coupons, discounts, rev_share_1,
         order_state, type, gross_revenue, net_revenue, 
         vip_discount, vip_discounts, payment_date, account_id
         ) 

transaction$account_id <- as.factor(transaction$account_id)
# Create uniform title name for all regions (Remove UK,EU specification)
transaction$title <- gsub("\\â„¢","",transaction$title)
transaction$title <- gsub("\\??","",transaction$title)
transaction$title <- gsub("\\{EU}","",transaction$title)
transaction$title <- gsub("\\{UK}","",transaction$title)
transaction$title <- gsub("\\®","",transaction$title)
transaction$title <- gsub("[[:blank:]]","",transaction$title)

# check game list for any duplicates
game_list <- transaction %>% 
  select(title) %>% 
  unique()

# Create datetime columns
transaction$payment_date <- as.POSIXct(strptime(transaction$payment_date, "%Y-%m-%d %H:%M:%S"))
transaction$payment_Year <- as.factor(lubridate::year(transaction$payment_date))
transaction$payment_Quarter <- as.factor(lubridate::quarter(transaction$payment_date))
transaction$payment_Month <- as.factor(lubridate::month(transaction$payment_date))
transaction$payment_Date <- as.factor(lubridate::day(transaction$payment_date))
transaction$payment_Hour <- as.numeric(lubridate::hour(transaction$payment_date))
transaction$payment_Minute <- as.factor(lubridate::minute(transaction$payment_date))
transaction$payment_Second <- as.factor(lubridate::second(transaction$payment_date))
transaction$payment_Weeknum <- as.factor(lubridate::week(transaction$payment_date))
transaction$payment_Weekdays <- as.factor(weekdays(transaction$payment_date))
transaction$timeoftheday<- with(transaction, ifelse(payment_Hour >= 5 & payment_Hour<=11, "morning",
                                      ifelse(payment_Hour>11 & payment_Hour<=16, "afternoon",
                                             ifelse(payment_Hour>16 & payment_Hour<=21, "evening" ,"night"))))
transaction$payment_Hour <- as.factor(transaction$payment_Hour)

# Factorize a few other variables
transaction$title <- as.factor(transaction$title)
transaction$company_name <- as.factor(transaction$company_name)
transaction$currency_full_name <- as.factor(transaction$currency_full_name)
transaction$country_full_name <- as.factor(transaction$country_full_name)
transaction$order_state <- as.factor(transaction$order_state)
transaction$type <- as.factor(transaction$type)
transaction$rev_share_1 <- as.factor(transaction$rev_share_1)
transaction$gross_revenue <- as.numeric(transaction$gross_revenue)
transaction$net_revenue <- as.numeric(transaction$net_revenue)
transaction$vip_discount <- as.factor(transaction$vip_discount)
transaction$timeoftheday <- as.factor(transaction$timeoftheday)

# Adding newly created variables
transaction$sold_price <- transaction$gross_revenue + transaction$vip_discounts
transaction <- transaction %>% 
#  select(-payment_date) %>% 
  filter(gross_revenue >= 0) %>% 
  filter(sold_price >= 0) %>% 
  filter(net_revenue >= 0)

# Check NA values
transaction %>% na_if("NULL") %>% map_df(~sum(is.na(.)))

# Examing numeric columns
transaction %>% select_if(is.numeric) %>% skim()

glimpse(transaction)

# Save dataset for later use
# saveRDS(transaction, "C:/Users/bokhy/Desktop/d2d_transactions.rds")



# 2. Data Pre-processing ====
library(e1071)
```
#If the skewness of the predictor variable is 0, the data is perfectly symmetrical,
#If the skewness of the predictor variable is less than -1 or greater than +1, the data is highly skewed,
#If the skewness of the predictor variable is between -1 and -0.5 or between +1 and +0.5 then the data is moderately skewed,
#If the skewness of the predictor variable is -0.5 and +0.5, the data is approximately symmetric.
```
## (1) msrp
skewness(transaction$msrp)
## right-skewed --> square-root 
ggplot(transaction, aes(x = msrp)) +
  geom_histogram() 
transaction$msrp <- sqrt(transaction$msrp)

## (2) gross_revenue
skewness(transaction$gross_revenue)
# It's right skewed --> sqaure-root 
ggplot(transaction, aes(x = gross_revenue)) +
  geom_histogram()
transaction$gross_revenue <- sqrt(transaction$gross_revenue)

## (3) net_revenue
skewness(transaction$net_revenue)
# It's right skewed --> sqaure-root 
ggplot(transaction, aes(x = net_revenue)) +
  geom_histogram()
transaction$net_revenue <- sqrt(transaction$net_revenue)

## (4) publisher_price
skewness(transaction$publisher_price)
# It's right skewed --> sqaure-root 
ggplot(transaction, aes(x = publisher_price)) +
  geom_histogram()
transaction$publisher_price <- sqrt(transaction$publisher_price)

## (5) sold_price
skewness(transaction$sold_price)
# It's right skewed --> sqaure-root 
ggplot(transaction, aes(x = sold_price)) +
  geom_histogram()
transaction$sold_price <- sqrt(transaction$sold_price)

## (6) sold_price
skewness(transaction$vip_discounts)
# It's right skewed --> sqaure-root 
ggplot(transaction, aes(x = vip_discounts)) +
  geom_histogram()
transaction$vip_discounts <- sqrt(transaction$vip_discounts)


# 3. Feature Selection ====

## (3.1) omit NAs and run Boruta ====
transaction <- na.omit(transaction)
set.seed(0623)
#boruta_new <- Boruta(order_state ~., data = transaction, doTrace = 2)
plot(boruta_new, las = 2, cex.axis = 0.6)

Boruta::getSelectedAttributes(boruta_new)
print(boruta_new)
# Remove unimportant variables
transaction <- transaction %>% select(-discounts)

## (3.2) Check for correlation ====
require(psych)

x <- cbind(transaction$msrp, transaction$publisher_price, transaction$coupons,
           transaction$gross_revenue, transaction$net_revenue, 
           transaction$vip_discounts, transaction$sold_price)
#pairs.panels(x, gap = 0, bg = c("red", "yellow")[transaction$order_state], pch = 21)
# If data are highly correlated, use PCA to reduce dimension
summary(x)

## (3.3) PCA ====
cor(x)
# "Cumulative proportion" is important 
# Usually we use the variables whose standard-devation is > 1
pcal <- princomp(x, scores = TRUE, cor = TRUE)
summary(pcal)

#confirm the ones whos Eigenvalues are above one
plot(pcal)

# correlations between variables are now 0 !!
PCA_numbers <- data.frame(pcal$scores , transaction$order_state)
PCA_numbers

# Replace PCA values into origianl variables
# We choose up to Comp 4
transaction$msrp <- pcal$scores[,1]
transaction$publisher_price <- pcal$scores[,2]
transaction$coupons <- pcal$scores[,3]
transaction$gross_revenue <- pcal$scores[,4]
transaction$net_revenue <- pcal$scores[,5]
transaction$vip_discounts <- pcal$scores[,6]
transaction$sold_price <- pcal$scores[,7]


# (3.4) Class Imbalance ====
# https://github.com/WinVector/vtreat
# https://github.com/WinVector/vtreat/blob/master/Examples/fit_transform/fit_prepare_api.md
# https://livebook.manning.com/book/practical-data-science-with-r-second-edition/chapter-8/125

prop.table(table(transaction$order_state))
# Let’s SMOTE
# (oversamples rare event by using bootstrapping and k-nearest neighbor to synthetically create additional observations of that event)
require(DMwR)
transaction$order_state <- as.factor(transaction$order_state)
balanced.data.1 <- DMwR::SMOTE(order_state ~ ., as.data.frame(transaction), perc.over = 300, perc.under=200)
#transaction$target <- as.numeric(transaction$target)
# We keep 60% OK, 40% Fraud rate
prop.table(table(balanced.data.1$order_state))


balanced.data.1 <- na.omit(balanced.data.1)




# 4. Machine Learning using H20 ====
# (4.1 H2O) ====
set.seed(0623)
library(caret)
train.index.1 <- createDataPartition(transaction$order_state, p = .6, list = FALSE)
train <- transaction[ train.index.1,]
test  <- transaction[-train.index.1,]
## Start H2O and split Train/Valid/Test
h2o.init(nthreads = -1, max_mem_size = "12g")
train <- as.h2o(train) # 60%
df.h2o <- as.h2o(test)
splits <- h2o.splitFrame(df.h2o, ratios = 0.5, seed = 0623)
valid  <- h2o.assign(splits[[1]], "valid.hex") # 20%
test   <- h2o.assign(splits[[2]], "test.hex") #20%

# Set predictor/response
y1 <- "order_state"
x1 <- setdiff(names(transaction),y1);x1

# (4.1.1 AutoML) ====
# Set Response / Predictor variables

aml <- h2o.automl(
  seed = 623,
  x = x1, 
  y = y1, 
  training_frame = train,
  validation_frame = valid,
  stopping_metric = "rmse",
  stopping_tolerance = 1e-2,
  stopping_rounds = 5,
  max_runtime_secs = 720,
  nfolds = 5) 

summary(aml)
aml@leaderboard

leader <- aml@leader
leader
# EVALUATE PERFORMANCE on Testset
h2o.performance(leader, newdata = test)

h2o.confusionMatrix(leader, valid = T)

# Predict 
pred_data <- h2o.predict(leader, newdata = test)

h2o.varimp(leader)
h2o.varimp_plot(leader)

# (4.1.2 Deeplearning) ====
max_epochs <- 30 
m_new <- h2o.deeplearning(
  activation = 'RectifierWithDropout',
  model_id="deeplearning_d2d_fraud", 
  # checkpoint="dl_model_tuned_new2", ## If  want to continue with the one you already did, compare it with this
  training_frame=train, 
  validation_frame=valid, 
  x=x1, 
  y=y1, 
  overwrite_with_best_model=F,    ## Return the final model after 10 epochs, even if not the best
  hidden=c(128,128,128),          ## more hidden layers -> more complex interactions (try [512], [64,64,64], [32,32,32,32,32])
  epochs=max_epochs,                    
  score_validation_samples=10000, ## downsample validation set for faster scoring
  score_validation_sampling = "Stratified",
  score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
  stopping_metric="AUTO",
  stopping_tolerance=1e-3,        ## stop when validation logloss does not improve by >=1% for 2 scoring events
  stopping_rounds=5,
  adaptive_rate=F,                ## manually tuning learning rate (if this is F, below metrics become important)
  rate=0.01, 
  rate_annealing=2e-6,            
  momentum_start=0.3,             ## manually tuned momentum
  momentum_stable=0.5, 
  momentum_ramp=1e7, 
  l1=1e-4,                        ## add some L1/L2 regularization
  l2=1e-4,
  max_w2=10,                       ## helps stability for Rectifier
  nfolds=5,                       ## N-fold cross-validation is especially useful with early stopping
  fold_assignment="AUTO"        ## can be "AUTO", "Modulo", "Random" or "Stratified"
) 

summary(m_new)
plot(m_new)

# Get Best model
best_model_new <- h2o.getModel(m_new@model_id)  
best_model_new

# Let's look at the model with the lowest validation misclassification rate
h2o.confusionMatrix(best_model_new,valid=T)
h2o.confusionMatrix(best_model_new)
# Parameters in Best Model
best_params <- best_model_new@allparameters
best_params

# Let's compare the training error
h2o.performance(m_new, newdata=train)    ## full training data
h2o.performance(m_new, newdata=valid)    ## full validation data
h2o.performance(m_new, newdata=test)     ## full test data

# prediction on the test set and compare the confusion matrices explicitly

p <- h2o.predict(m_new, newdata = test)
p <- as.data.frame(p);p


# (4.1.3 GBM) ====
# (4.1.3.1 Naive GBM) ====
gbm <- h2o.gbm(x = x1, 
               y = y1, 
               training_frame = train,
               validation_frame = valid,
               ntrees = 1000,                ## decrease the trees, mostly to allow for run time
               learn_rate = 0.01,           ## increase the learning rate (from 0.1)
               max_depth = 15,             ## increase the depth (from 5)            ## 
               sample_rate = 0.8,          ## use a random 70% of the rows to fit each tree
               col_sample_rate = 0.7,       ## use 70% of the columns to fit each tree
               stopping_rounds = 4,        ## 
               stopping_tolerance = 0.01,  ##
               stopping_metric = "rmse", 
               score_each_iteration = T, 
               max_runtime_secs =3600 ##
               )
gbm

## Get the AUC on the validation set
h2o.auc(h2o.performance(gbm, newdata = valid))

h2o.confusionMatrix(gbm,valid=T)

# (4.1.3.2 Grid-Serach) ====

## Depth 10 is usually plenty of depth for most datasets, but you never know
#hyper_params = list( max_depth = seq(1,29,2) )
hyper_params = list( max_depth = c(4,6,8,12,16) ) ##faster for larger datasets
grid <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,
  ## full Cartesian hyper-parameter search
  search_criteria = list(strategy = "Cartesian"),
  ## which algorithm to run
  algorithm="gbm",
  ## identifier for the grid, to later retrieve it
  grid_id="depth_grid",
  ## standard model parameters
  x = x1,
  y = y1,
  training_frame = train,
  validation_frame = valid,
  ## more trees is better if the learning rate is small enough
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 500,
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.05,
  ## learning rate annealing: learning_rate shrinks by 1% after every tree
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.99,
  ## sample 80% of rows per tree
  sample_rate = 0.8,
  ## sample 80% of columns per split
  col_sample_rate = 0.8,
  ## fix a random number generator seed for reproducibility
  seed = 623,
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5,
  stopping_tolerance = 1e-3,
  stopping_metric = "logloss",
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10
)
## by default, display the grid search results sorted by increasing logloss (since this is a classification task)
grid
## sort the grid models by decreasing AUC
sortedGrid <- h2o.getGrid("depth_grid", sort_by="logloss", decreasing = FALSE)
sortedGrid
## find the range of max_depth for the top 5 models
topDepths = sortedGrid@summary_table$max_depth[1:5]
minDepth = min(as.numeric(topDepths))
maxDepth = max(as.numeric(topDepths))

hyper_params = list(
  ## restrict the search to the range of max_depth established above
  max_depth = seq(minDepth,maxDepth,1),
  ## search a large space of row sampling rates per tree
  sample_rate = seq(0.2,1,0.01),
  ## search a large space of column sampling rates per split
  col_sample_rate = seq(0.2,1,0.01),
  ## search a large space of column sampling rates per tree
  col_sample_rate_per_tree = seq(0.2,1,0.01),
  ## search a large space of how column sampling per split should change as a function of the depth of the split
  col_sample_rate_change_per_level = seq(0.9,1.1,0.01),
  ## search a large space of the number of min rows in a terminal node
  min_rows = 2^seq(0,log2(nrow(train))-1,1),
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
  stopping_metric = "logloss",
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
  training_frame = train,
  validation_frame = valid,
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
  stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "logloss",
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10,
  ## base random number generator seed for each model (automatically gets incremented internally for each model)
  seed = 1234
)
grid
## Sort the grid models by AUC
sortedGrid <- h2o.getGrid("final_grid", sort_by = "logloss", decreasing = FALSE)
sortedGrid

gbm <- h2o.getModel(sortedGrid@model_ids[[1]])
h2o.performance(gbm, newdata = test)

h2o.confusionMatrix(gbm,valid=T)

# Now we can confirm that these parameters are generally sound, 
# by building a GBM model on the whole dataset (instead of the 60%) 
# and using internal 5-fold cross-validation (re-using all other parameters including the seed):

model <- do.call(h2o.gbm,
                 ## update parameters in place
                 {
                   p <- gbm@parameters
                   p$model_id = NULL          ## do not overwrite the original grid model
                   p$training_frame = h2o.rbind(train, valid)      ## use the full dataset
                   p$validation_frame = NULL  ## no validation frame
                   p$nfolds = 5               ## cross-validation
                   p
                 }
)
# Ouch! So it looks like we overfit quite a bit on the validation set
model@model$cross_validation_metrics_summary
# So to get a better estimate of model performance, the Random hyper-parameter search could have used nfolds = 5 (or 10, or similar) 
# in combination with 80% of the data for training (i.e., not holding out a validation set, but only the final test set)
# Instaed, let’s just scan through the top 5 models and cross-validated their parameters with nfolds=5 on the entire dataset

for (i in 1:5) {
  gbm <- h2o.getModel(sortedGrid@model_ids[[i]])
  cvgbm <- do.call(h2o.gbm,
                   ## update parameters in place
                   {
                     p <- gbm@parameters
                     p$model_id = NULL          ## do not overwrite the original grid model
                     p$training_frame = h2o.rbind(train, valid)      ## use the full dataset
                     p$validation_frame = NULL  ## no validation frame
                     p$nfolds = 5               ## cross-validation
                     p
                   }
  )
  print(gbm@model_id)
  print(cvgbm@model$cross_validation_metrics_summary) 
}

# Get best model and prediction
gbm <- h2o.getModel(sortedGrid@model_ids[[1]])

# This give the best cofusionmatrix result in test-set!
h2o.performance(gbm, newdata = test)

preds <- h2o.predict(gbm, test)

# Note that the target uses the threshold with the highest F1 score (here: 0.361618)
head(preds)
gbm@model$validation_metrics@metrics$max_criteria_and_metric_scores

# Save model and prediction
h2o.exportFile(preds, "d2d_fraud_bestPreds", force=TRUE)
h2o.saveModel(gbm, "d2d_fraud_bestPreds/d2d_fraud_bestModel.csv", force=TRUE)
# The model can also be exported as a plain old Java object (POJO) for H2O-independent
h2o.download_pojo(gbm)

# ===
# Let's try on not-processed new test set
# New_test <- "xxx.csv"
index <- createDataPartition(transaction$order_state, p = .7, list = FALSE)
New_test  <- transaction[-index,]
New_test <- as.h2o(New_test)
h2o.performance(gbm, newdata = New_test)
preds <- h2o.predict(gbm, test)
head(preds)
gbm@model$validation_metrics@metrics$max_criteria_and_metric_scores
# ===



# (4.2 XGBoost) ====

# Train/Test split
train.index <- createDataPartition(balanced.data.1$order_state, p = .8, list = FALSE)
training <- balanced.data.1[ train.index,]
testing  <- balanced.data.1[-train.index,]

# One-hot encode all Categorical varialbes
# and create data in Matrix form to be supplied into XGBOOST rightaway

library(vtreat)
library(wrapr)
library(WVPlots)

(parallel_cluster <- parallel::makeCluster(parallel::detectCores()))

outcome <- 'order_state'
vars <- setdiff(colnames(transaction), outcome)

transform_spec <- vtreat::BinomialOutcomeTreatment(
  var_list = vars,              # columns to transform
  outcome_name = 'order_state', # outcome variable
  outcome_target = 'CHARGED'    # outcome of interest
)

treatment_plan_2 <- fit_prepare(transform_spec, training)  

d_prepared = treatment_plan_2$cross_frame
d_prepared %.>% head(.) 
transform = treatment_plan_2$treatments
get_feature_names(transform)

dtest_prepared <- vtreat::prepare(transform,
                                  testing,
                                  #pruneSig = c(1/26),  # Note: usually want pruneSig = 1 / colcount
                                  parallelCluster = parallel_cluster)
#dtest_prepared <- dtest_prepared %>% select(1:195, vip_discount_lev_x_8, everything())

treatment_plan_1 <- vtreat::mkCrossFrameCExperiment(
    dframe = training,                                    # data to learn transform from
    varlist = vars,  # columns to transform
    outcomename = 'order_state',                            # outcome variable
    outcometarget = 'CHARGED', # outcome of interest
    rareCount = 3,
    rareSig = 0.3,
    parallelCluster = parallel_cluster
  )

d_prepared = treatment_plan_1$crossFrame
transform = treatment_plan_1$treatments

dtest_prepared <- vtreat::prepare(transform,
                                    testing,
                                    pruneSig = c(1/26),  # Note: usually want pruneSig = 1 / colcount
                                    parallelCluster = parallel_cluster)



# (1) Caret method

# Convert the training and testing sets into DMatrixes
X_train = xgb.DMatrix(as.matrix(d_prepared %>% select(-order_state)))
y_train = d_prepared$order_state
X_test = xgb.DMatrix(as.matrix(dtest_prepared %>% select(-order_state)))
y_test = dtest_prepared$order_state

# Specify cross-validation method and number of folds. Also enable parallel computation
xgb_trcontrol = trainControl(
  method = "cv",
  number = 5,  
  allowParallel = TRUE,
  verboseIter = FALSE,
  returnData = FALSE
)

# Do Grid space to search for the "best" hyperparameters
xgbGrid <- expand.grid(nrounds = c(100,200),  
                       max_depth = c(10, 15, 20, 25),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       ## The values below are default values in the sklearn-api. 
                       eta = 0.01,
                       gamma = 0,
                       min_child_weight = 1,
                       subsample = 1
)

# Train my Model
set.seed(0623) 
xgb_model = train(
  X_train, y_train,  
  trControl = xgb_trcontrol,
  tuneGrid = xgbGrid,
  method = "xgbTree", 
  verbose = TRUE
)

# Best hyperparameter value
xgb_model$bestTune

# Model Evaluation
predicted = predict(xgb_model, X_test)
residuals = y_test - predicted
RMSE = sqrt(mean(residuals^2))
cat('The root mean square error of the test data is', round(RMSE,3),'\n')

# Plot Actual vs Predicted
options(repr.plot.width = 8, repr.plot.height = 4)
my_data = as.data.frame(cbind(predicted = predicted,
                              observed = y_test))

# Plot predictions vs test data
ggplot(my_data,aes(predicted, observed)) + geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method = lm) + ggtitle('Linear Regression ') + ggtitle("Extreme Gradient Boosting: Prediction vs Test Data") +
  xlab("Predecited Power Output ") + ylab("Observed Power Output") + 
  theme(plot.title = element_text(color = "darkgreen",size = 16,hjust = 0.5),
        axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12,hjust = 0.5),
        axis.title.x = element_text(size = 14), axis.title.y = element_text(size = 14))



# (2) Native XGboost
train_matrix <- xgb.DMatrix(data = as.matrix(d_prepared %>% select(-order_state)), label = as.numeric(d_prepared[,'order_state']) -1 );train_matrix
test_matrix <- xgb.DMatrix(data = as.matrix(dtest_prepared %>% select(-order_state)), label = as.numeric(dtest_prepared[,'order_state']) -1 );test_matrix


best_param <- list()
best_seednumber <- 123
best_rmse <- Inf
best_rmse_index <- 0

# you run cross validation 100 times, each time with random parameters. 
# Then you get best parameter set, that is in the iteration with minimum min_rmse.
set.seed(0623)
for (iter in 1:100) {
  param <- list(objective = "binary:logistic",
                eval_metric = "rmse",
                max_depth = sample(6:10, 1),
                eta = runif(1, .01, .3), # Learning rate, default: 0.3
                subsample = runif(1, .6, .9),
                colsample_bytree = runif(1, .5, .8), 
                min_child_weight = sample(1:40, 1),
                max_delta_step = sample(1:10, 1)
  )
  cv.nround <-  1000
  cv.nfold <-  5 # 5-fold cross-validation
  seed.number  <-  sample.int(10000, 1) # set seed for the cv
  set.seed(seed.number)
  mdcv <- xgb.cv(data = train_matrix, params = param, nthread = 3, 
                 nfold = cv.nfold, nrounds = cv.nround,
                 verbose = F, early_stopping_rounds = 20, maximize = FALSE,
                 print_every_n = 10)
  
  min_rmse_index  <-  mdcv$best_iteration
  min_rmse <-  mdcv$evaluation_log[min_rmse_index]$test_rmse_mean
  
  if (min_rmse < best_rmse) {
    best_rmse <- min_rmse
    best_rmse_index <- min_rmse_index
    best_seednumber <- seed.number
    best_param <- param
  }
}

# The best index (min_rmse_index) is the best "nround" in the model
nround = best_rmse_index
set.seed(best_seednumber)
md <- xgb.train(data=train_matrix, params=best_param, nrounds=nround, nthread=3, verbose = TRUE)
md
#xg_mod <- xgboost(data = test_matrix, params = best_param, nround = nround, verbose = F)

# Check error in testing data
yhat_xg <- predict(md, test_matrix)
(MSE_xgb <- mean((yhat_xg - Y_test)^2))



# or Bayesian optimization to facilitate the process of hyperparameter search
require(rBayesianOptimization)

cv_folds <- KFold(d_prepared$order_state, nfolds = 5, stratified = FALSE, seed = 0623)
xgb_cv_bayes <- function(nround, max.depth, min_child_weight, subsample,
                         eta, gamma, colsample_bytree, max_delta_step) {
  param <- list(booster = "gbtree", 
                max_depth = max.depth,
                min_child_weight = min_child_weight,
                eta = eta,
                gamma = gamma,
                subsample = subsample, 
                colsample_bytree = colsample_bytree,
                max_delta_step = max_delta_step,
                lambda = 1, alpha = 0,
                objective = "binary:logistic",
                eval_metric = "rmse")
  cv <- xgb.cv(params = param, data = train_matrix, folds  = cv_folds, nthread = 4,
               nrounds = 50, early_stopping_rounds = 5, maximize = FALSE, 
               prediction = TRUE, showsd = TRUE,
               verbose = TRUE, print_every_n = 10)
  
  list(Score = cv$evaluation_log$test_rmse_mean[cv$best_iteration],
       Pred = cv$best_iteration)
  # we don't need cross-validation prediction and we need the number of rounds.
  # a workaround is to pass the number of rounds(best_iteration) to the Pred, which is a default parameter in the rbayesianoptimization library.
}

OPT_Res <- BayesianOptimization(xgb_cv_bayes,
                                bounds = list(max.depth = c(2L, 10L),
                                              min_child_weight = c(2L, 10L),
                                              subsample = c(0.5, 0.9),
                                              eta = c(0.005,0.3),
                                              gamma = c(0.0, 0.2),
                                              colsample_bytree = c(0.5,1),
                                              max_delta_step = c(1L,10L)),
                                init_grid_dt = NULL, init_points = 10, n_iter = 10,
                                acq = "ucb", kappa = 2.576, eps = 0.0,
                                verbose = TRUE)

best_param <- list(
  booster = "gbtree",
  eval.metric = "rmse",
  objective = "binary:logistic",
  max_depth = OPT_Res$Best_Par["max.depth"],
  eta = OPT_Res$Best_Par["eta"],
  gamma = OPT_Res$Best_Par["gamma"],
  subsample = OPT_Res$Best_Par["subsample"],
  colsample_bytree = OPT_Res$Best_Par["colsample_bytree"],
  min_child_weight = OPT_Res$Best_Par["min_child_weight"],
  max_delta_step = OPT_Res$Best_Par["max_delta_step"]
  )
# number of rounds should be tuned using CV
#https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/
# However, nrounds can not be directly derivied from the bayesianoptimization function
# Here, OPT_Res$Pred, which was supposed to be used for cross-validation, is used to record the number of rounds
nrounds=OPT_Res$Pred[[which.max(OPT_Res$History$Value)]]
xgb_model <- xgb.train(params = best_param, data = train_matrix, nrounds = nrounds, nthread=3, verbose = TRUE)
xgb_model

# 5. Feature Importance ====
imp <- xgb.importance(colnames(train_matrix),model = xgb_model); imp
xgb.plot.importance(imp)
xgb.plot.tree(feature_names = colnames(train_matrix), model = xgb_model, trees = 0)


# 6. Prediction & Confusion Matrix using Test data now ====
pred <- predict(xgb_model, newdata = test_matrix)
head(pred)

test_labels <- as.numeric(dtest_prepared$order_state) # the test labels
xgb_pred_class <- as.numeric(pred > 0.50) # to get your predicted labels 
# keep in mind that 0.50 is a threshold that can be modified.

confusionMatrix(as.factor(xgb_pred_class), as.factor(test_labels-1))


# 7. Explaining Machine Learning (Global: PDP, ICE & Local: LIME, Shapley)  ====
# (7.1 IML Explanation) ====
train.index <- createDataPartition(transaction$order_state, p = .7, list = FALSE)
train_set <- transaction[ train.index,]
test_set  <- transaction[-train.index,]

features_tbl <- train_set %>% select(-order_state) 

response_vec <- train_set %>% pull(order_state) %>% as.numeric() - 1

predict_h2o <- function(model, newdata) {
  results_tbl <- h2o.predict(gbm, newdata = as.h2o(newdata)) %>% as_tibble() 
  results_tbl %>% pull(1)
}


# (7.2 Create predictor object) ====
preidctor_rf <- Predictor$new(
  model = m_new,
  data = features_tbl,
  y = response_vec,
  predict.fun = predict_h2o,
  class = "classification"
)
preidctor_rf

# (7.3 PDP) ====

# Examine single feature - "payment_Weekdays"

pdp_contract <- FeatureEffect$new(
  predictor = preidctor_rf,
  feature = c("payment_Year","payment_Month"),
  method = "pdp",
  grid.size = 20
)

pdp_contract %>% 
  plot() + expand_limits(y = 0) # now see their is a different where my RF results out 

# (7.4 ICE) ====
ice_contact_monthlys <- FeatureEffect$new(
  predictor = preidctor_rf,
  feature = "payment_Year",
  method = "ice",
  grid.size = 10,
  center.at = 0 # This centers every at 0. So it gives more clear visual
)
