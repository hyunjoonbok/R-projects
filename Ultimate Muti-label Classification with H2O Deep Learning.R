## Multi-Label Classification with H2O DeepLearning ##

# Load package
setwd("C:/Users/bokhy/Desktop/R")

library(h2o)
library(tidyverse)
library(tidymodels)
library(rsample)

# Start h2o cluster
h2o.init(nthreads=-1, max_mem_size="16G")
h2o.removeAll()

## Data from "https://www.kaggle.com/c/forest-cover-type-prediction/data?select=test.csv.zip"
df <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/covtype/covtype.data.gz",
               col_names = FALSE)

# Change few variables to factors
names <- c(11:55)
df[,names] <- lapply(df[,names] , factor)

# Rename columns
df <- df %>% 
  rename(
    Elevation = X1,
    Aspect = X2,
    Slope = X3,
    Horizontal_Distance_To_Hydrology = X4,
    Vertical_Distance_To_Hydrology = X5,
    Horizontal_Distance_To_Roadways = X6,
    Hillshade_9am = X7,
    Hillshade_Noon = X8,
    Hillshade_3pm = X9,
    Horizontal_Distance_To_Fire_Points = X10,
    Wilderness_Area1 = X11,
    Wilderness_Area2 = X12,
    Wilderness_Area3 = X13,
    Wilderness_Area4 = X14,
    Soil_Type1 = X15,
    Soil_Type2 = X16,
    Soil_Type3 = X17,
    Soil_Type4 = X18,
    Soil_Type5 = X19,
    Soil_Type6 = X20,
    Soil_Type7 = X21,
    Soil_Type8 = X22,
    Soil_Type9 = X23,
    Soil_Type10 = X24,
    Soil_Type11 = X25,
    Soil_Type12 = X26,
    Soil_Type13 = X27,
    Soil_Type14 = X28,
    Soil_Type15 = X29,
    Soil_Type16 = X30,
    Soil_Type17 = X31,
    Soil_Type18 = X32,
    Soil_Type19 = X33,
    Soil_Type20 = X34,
    Soil_Type21 = X35,
    Soil_Type22 = X36,
    Soil_Type23 = X37,
    Soil_Type24 = X38,
    Soil_Type25 = X39,
    Soil_Type26 = X40,
    Soil_Type27 = X41,
    Soil_Type28 = X42,
    Soil_Type29 = X43,
    Soil_Type30 = X44,
    Soil_Type31 = X45,
    Soil_Type32 = X46,
    Soil_Type33 = X47,
    Soil_Type34 = X48,
    Soil_Type35 = X49,
    Soil_Type36 = X50,
    Soil_Type37 = X51,
    Soil_Type38 = X52,
    Soil_Type39 = X53,
    Soil_Type40 = X54,
    Cover_Type = X55
  ) 

# Set Prdictors / Response columns
response <- "Cover_Type"
predictors <- setdiff(names(df), response)
predictors

# Train/Valid/Test split
set.seed(0623)
vb_split <- initial_split(df, strata = "Cover_Type")
train <- training(vb_split)
test <- testing(vb_split)

train <- as.h2o(train) # 60%
df.h2o <- as.h2o(test)

splits <- h2o.splitFrame(df.h2o, ratios = 0.5, seed = 0623)
valid  <- h2o.assign(splits[[1]], "valid.hex") # 20%
test   <- h2o.assign(splits[[2]], "test.hex") #20%

# check for any missing values
h2o.describe(df.h2o)

## ============================== ##

# 1. Modeling with Manual Tuning 

## ============================== ##

# With some tuning, it is possible to obtain less than 10% test set error rate 
# We can continue training the manually tuned model above, for any more epochs we want
# Make sure to put "checkpoint" to the previously trained model_id

max_epochs <- 20 
m_new <- h2o.deeplearning(
  activation = 'RectifierWithDropout',
  model_id="dl_model_tuned_new4", 
# checkpoint="dl_model_tuned_new2", ## If  want to continue with the one you already did, compare it with this
  training_frame=train, 
  validation_frame=valid, 
  x=predictors, 
  y=response, 
  overwrite_with_best_model=F,    ## Return the final model after 10 epochs, even if not the best
  hidden=c(512,512),          ## more hidden layers -> more complex interactions (try [512], [64,64,64], [32,32,32,32,32])
  epochs=max_epochs,                    
  score_validation_samples=10000, ## downsample validation set for faster scoring
  score_validation_sampling = "Stratified",
  score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
  stopping_metric="rmse",
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
  fold_assignment="AUTO",        ## can be "AUTO", "Modulo", "Random" or "Stratified"
) 

summary(m_new)
plot(m_new)

# Get Best model
best_model_new <- h2o.getModel(m_new@model_id)  
best_model_new

# Let's look at the model with the lowest validation misclassification rate
h2o.confusionMatrix(best_model_new,valid=T)
# Parameters in Best Model
best_params <- best_model_new@allparameters
best_params

# Let's compare the training error
h2o.performance(m_new, train=T)          ## sampled training data (from model building)
h2o.performance(m_new, valid=T)          ## sampled validation data (from model building)
h2o.performance(m_new, newdata=train)    ## full training data
h2o.performance(m_new, newdata=valid)    ## full validation data
h2o.performance(m_new, newdata=test)     ## full test data

# prediction on the test set and compare the confusion matrices explicitly
pred <- h2o.predict(m_new, test)
pred

# error rate
test$Accuracy <- pred$predict == test$Cover_Type
(1-mean(test$Accuracy))*100


## ============================== ##

# 2. Modeling with Hyper-parameter Tuning thru Random Grid Search

## ============================== ##

# hyper-parameter search for more than 4 parameters can be done more efficiently with random parameter search than with grid search
# So we chose to go with random-grid serach 
# We simply build up to max_models models with parameters 
# drawn randomly from user-specified distributions (here, uniform).

hyper_params <- list(
  activation = c("Rectifier","Tanh","Maxout","RectifierWithDropout","TanhWithDropout","MaxoutWithDropout"),
  hidden = list(c(20,20),c(50,50),c(32,32,32),c(64,64),c(25,25,25,25),c(200,200),c(512,512),c(128,128,128),c(512,512,512),c(128,128,128,128)),
  input_dropout_ratio = c(0,0.01,0.05,0.1,0.2,0.4,0.5),
  l1 = seq(0,1e-4,1e-6), # add regularization
  l2 = seq(0,1e-4,1e-6),
  rate = c(0.01,0.02,0.04,0.05),
  rate_annealing = c(2e-6,2e-7,2e-8),
  momentum_start = c(0.1,0.2,0.5),
  momentum_stable = c(0.1,0.4,0.5,0.99),
  balance_classes = TRUE # For classification class-imbalnace problem
)

# (Default) Strategy = “Cartesian” covers the entire space of hyperparameter combinations
# Strategy = “RandomDiscrete” do a random search of all the combinations of your hyperparameters. RandomDiscrete should be usually combined with at least one early stopping criterion, max_models and/or max_runtime_secs
search_criteria = list(strategy = "RandomDiscrete",
                       max_runtime_secs = 3600, max_models = 10, seed = 0623)

grid <- h2o.grid(
  algorithm="deeplearning",
  grid_id="dl_random_grid", 
  training_frame=splits$train,
  validation_frame=splits$valid, 
  x=predictors, 
  y=response,
  epochs=15,
  stopping_metric="AUTO",
  stopping_tolerance=1e-2,        ## stop when misclassification does not improve within 1% for 2 scoring events
  stopping_rounds=2,
  score_validation_samples=10000, ## downsample validation set for faster scoring
  score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
  adaptive_rate=F,                ## manually tuned learning rate
  max_w2=10,                      ## can help improve stability for Rectifier
  nfolds=5,                       ## N-fold cross-validation is especially useful with early stopping
  fold_assignment="AUTO",        ## can be "AUTO", "Modulo", "Random" or "Stratified"
  hyper_params=hyper_params, 
  search_criteria = search_criteria,
  max_after_balance_size = 3
)
grid
summary(grid, show_stack_traces = TRUE)

grid_result <- h2o.getGrid("dl_random_grid",sort_by="logloss",decreasing=FALSE)
# To see what other "sort_by" criteria are allowed
#grid_result <- h2o.getGrid("dl_random_grid",sort_by="wrong_thing",decreasing=FALSE)
grid_result

## Find the best model and its full set of parameters
grid_result@summary_table[1,]
best_model <- h2o.getModel(grid_result@model_ids[[1]])
best_model

# Let's look at the model with the lowest validation misclassification rate
h2o.confusionMatrix(best_model,valid=T)
best_params <- best_model@allparameters
best_params


h2o.performance(best_model, newdata=splits$train)    ## full training data
h2o.performance(best_model, newdata=splits$valid)    ## full validation data
h2o.performance(best_model, newdata=splits$test)     ## full test data
# prediction on the test set and compare the confusion matrices explicitly
pred <- h2o.predict(best_model, splits$test)
pred

splits$test$Accuracy <- pred$predict == splits$test$Cover_Type
# error rate
(1-mean(splits$test$Accuracy)) *100


## ============================================================ ## 

# Select the best model you want to save, and save the model to disk 
path <- h2o.saveModel(m_new, path="C:/Users/bokhy/Documents/R-projects/", force=TRUE)

# Load model later 
print(path)
m_loaded <- h2o.loadModel(path)
summary(m_loaded)