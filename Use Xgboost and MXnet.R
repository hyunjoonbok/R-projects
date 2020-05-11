## Use Xgboost!! 

# 1. Load Packages
setwd("C:/Users/bokhy/Desktop/R/xgboost_tutorial/user2018tutorial-master/data")
require(xgboost)
require(caret)
load("hr.rda")
dim(train_hr)
head(train_hr)

set.seed(0623)
ind <- sample(nrow(train_hr))
train_ind <- ind[1:10000]
test_ind <- ind[10001:14999]

x <- train_hr[train_ind, -1]
y <- train_hr[train_ind, 1]

x.test <- train_hr[test_ind, -1]
y.test <- train_hr[test_ind, 1]

# 2. Cross-Validation
# Setting up parameters
param <- list("objective" = "binary:logistic",
              "eval_metric" = "auc",
              "eta" = 0.5, 
              "max.depth" = 3)
## we should take a look at "test-logloss", enlarge nrounds until "test-logloss" is increasing (usually more than 200)
bst.cv <- xgb.cv(params = param, data = x, label = y, nfold = 10, nrounds = 200)
bst.cv
plot(bst.cv$evaluation_log$test_auc_mean, type = "l")

# 3. XGBOOST Model to train
##!! Use above CV to decide the "Best" Parameter by looking at the auc or error !!##
## Then, use the same parameters to build xgboost() ##
## Here, we don't need nfold
model <- xgboost(params = param, data = x, label = y, nrounds = 200)

# 4. Predict using Test data
pred <- predict(model, x.test)
head(pred)
## Auc Curve to see AUC on test data
## Transform y test value as factor
require(AUC)
auc(roc(pred, as.factor(y.test)))
# compare this result to above CV test-auc!

# 5. [Interpret Feature Importance]
importance <- xgb.importance(model = model); importance
# Gain is important!!
# Gain: how much we should improve our model in terms of our objective by deep analyzing each feature
# Frequency: how often we see that feature impacts the decicsion in the trees
xgb.plot.importance(importance)
# visualize the tree
# "Trees = 0" uses the first tree
xgb.plot.tree(feature_names = colnames(x), model = model, trees = 0)
# Problem is that we can't decide based on single tree!
# SO below is the best representation (merges all threes with possible features)!!
# Number inside shows how many trees each feature has
xgb.plot.multi.trees(model)

xgb.plot.deepness(model)
# if data points stop at 5 --> maybe we are using too deep tree
