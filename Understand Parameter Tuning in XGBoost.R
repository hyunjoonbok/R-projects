# Understand Parameter Tuning in XGBoost

# 1. Load data & packages
setwd("C:/Users/bokhy/Desktop/R/xgboost_tutorial/user2018tutorial-master/data")
require(xgboost)
require(caret)
load("otto.rda")
str(train_otto)
head(train_otto)
train_otto <- as.data.frame(train_otto)
train_otto$target_otto <- train_otto[,1] - 1

set.seed(0623)
inTrain <- createDataPartition(y = train_otto$target_otto, p = 0.8, list = FALSE)
training <- train_otto[inTrain,]
testing <- train_otto[-inTrain,]



# 2. Cross-Validation
# Setting up parameters
nc <- length(unique(testing$target_otto));nc # number of classes in label
param <- list("objective" = "multi:softprob", # multi-classification probelm with output as probability 
                            "eval_metric" = "mlogloss", # multiple log-loss
                            "num_class" = nc)


bst.cv <- xgb.cv(params = param, data = as.matrix(training[,-1]), label = as.matrix(training$target_otto), nfold = 3, nrounds = 5) # even this takes a minute. In reality, nfold >5 and nround >200 should be done.
bst.cv


# 3. SO what are the tunable parameters?
# First, look at objective, eval_metric, eta, nrounds !!

# Overfitting --> decrease max_depth, increase subsample, colsample_bytree, increase gamma,
# Underfitting --> increase max_depth, decrease subsample and colsample_bytree, decrease gamma or lambda, add "parallel trees"

# Do faster calculation --> (only in some cases)
# add these in  params <- list(grow_policy = "depthwise", tree_method = "hist") 


train_matrix = xgb.DMatrix(data = as.matrix(training), label = training$target_otto)
test_matrix = xgb.DMatrix(data = as.matrix(testing), label = testing$target_otto)


bst_model <- xgb.train(params = param,
                       data = train_matrix,
                       nrounds = 5,
                       eta = 0.4, # [default is 0.3 and range from 0 to 1} learning rate 
                       max.depth = 3, # overfitting
                       gamma = 0, #  [between 0 and infinity ] if larger -> more conservative algorithm
                       subsample = 1,# [between 0 and 1 ] if lower -> prevent overfitting
                       colsample_bytree = 1, # stronger randomness
                       missing = NA,
                       seed = 0623)


# 4. # Feature Importance
imp <- xgb.importance(colnames(train_matrix),model = bst_model); imp
xgb.plot.importance(imp)
xgb.plot.tree(feature_names = colnames(train_matrix), model = bst_model, trees = 0)




# 5. Prediction & Confusion Matrix using Test data now
pred <- predict(bst_model, newdata = test_matrix)
head(pred)
require(dplyr)
a <- matrix(pred, nrow = nc, ncol = length(pred)/nc) %>% 
  t() %>% 
  data.frame() %>% 
  mutate(label = pred, max_prob = max.col(.,"last")-1)
table(Prediction = a$max_prob, Actual = a$label)

