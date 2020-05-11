## 1. 
require(xgboost)
set.seed(0623)
data("agaricus.train")
data("agaricus.test")
train <- agaricus.train
test <- agaricus.test
# Setting up parameters
param <- list("objective" = "binary:logistic",
              "eval_metric" = "logloss",
              "eta" = 0.4, "max.depth" = 2)

# Cross Validation
## we should take a look at "test-logloss", enlarge nrounds until "test-logloss" is increasing (usually more than 200)
bst.cv <- xgb.cv(params = param, data = as.matrix(train$data), label = train$label, nfold = 10, nrounds = 20)

# little plot of above CV result
plot(log(bst.cv$evaluation_log$test_logloss_mean), type = "l")

# Xgboost
# This shows train-error, which we don't need to care a lot
bst <- xgboost(data = as.matrix(train$data), label = train$label, max.depth = 2, eta = 0.5, nrounds = 5, objective = "binary:logistic")
# predict 
pred = predict(bst, test$data)

# little plot of prediction
trees = xgb.model.dt.tree(dimnames(train$data)[[2]], model = bst);trees

# Feature Selection
names <- dimnames(train$data)[[2]]
imprt_matrix <- xgb.importance(names,model=bst)
xgb.plot.importance(imprt_matrix[1:10]) # we should take variables with higher value
require(DiagrammeR)
xgb.plot.tree(feature_names = names, model = bst, n_first_tree = 2)





##2.
require(xgboost)
require(magrittr)
require(dplyr)
require(Matrix)

# Data
data <- read.csv("binary.csv")
str(data)
data$rank <- as.factor(data$rank) # as it is categorical variable

# Split data
set.seed(0623)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8,0.2))
train <- data[ind==1,]
test <- data[ind ==2,]

# Create matrix - One-Hot Encoding for Factor variables 
# create  Dummy varialbes for factor variable and make it numerics
trainm <- sparse.model.matrix(admit ~. -1, data = train)
# There will be no change for numeric variables
head(trainm)

train_label <- train[,"admit"] # this contains "admit"'s value
train_matrix <- xgb.DMatrix(data = as.matrix(trainm), label = train_label)
# We have converted training set into neccesary format used

testm <- sparse.model.matrix(admit ~. -1,data = test)
test_label <- test[,"admit"]
test_matrix <- xgb.DMatrix(data = as.matrix(testm), label = test_label)

## setting up Parameters
nc <- length(unique(train_label));nc # number of classes in label
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = nc)
watchlist <- list(train = train_matrix,
                  test = test_matrix)

## Build XGBOOST model
bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = 100,
                       watchlist = watchlist)
bst_model
## Training & Test Error Plot
e <- data.frame(bst_model$evaluation_log)
plot(e$iter, e$train_mlogloss, col = "blue") #traning error
lines(e$iter, e$test_mlogloss, col = "red") # add test error

min(e$test_mlogloss) # we can see overfitting...
e[e$test_mlogloss == 0.600161,]

# So let's optimize it
bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = 100,
                       watchlist = watchlist,
                       # default is 0.3 and range from 0 to 1
                       eta = 0.1) #if low, its more robust to overfitting)


bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = 100,
                       watchlist = watchlist,
                       # default is 0.3 and range from 0 to 1
                       eta = 0.05)
min(e$test_mlogloss)


# Feature Importance
imp <- xgb.importance(colnames(train_matrix),model = bst_model)
xgb.plot.importance(imp)
print(imp)
# Gain is important value
# Gain: Improvement in accuracy by a feature to the branches it is on



## Prediction & Confusion Matrix using Test data now
p <- predict(bst_model, newdata = test_matrix)
head(p)
# In this dataset, high value: person should not be accepted, Low value: person should be accepted
pred <- matrix(p, nrow = nc, ncol = length(p)/nc) %>% 
  t() %>% 
  data.frame() %>% 
  mutate(label = test_label, max_prob = max.col(.,"last")-1)
head(pred)
# X1 has prob that studnet is not be admitted
# X2 has prob that student will be admitted
# if label is 1 = they will be admiited, 0 = not be admitted
# The first cases shows student will not be admitted, but actually admitted

table(Prediction = pred$max_prob, Actual = pred$label)


## More xgboost Params!

bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = 100,
                       watchlist = watchlist,
                       # default is 0.3 and range from 0 to 1
                       eta = 0.01, # learning rate 
                       max.depth = 3,
                       gamma = 0, #  [between 0 and infinity ] if larger -> more conservative algorithm
                       subsample = 1,# [between 0 and 1 ] if lower -> prevent overfitting
                       colsample_bytree = 1,
                       missing = NA,
                       seed = 0623) 
#plot..
e <- data.frame(bst_model$evaluation_log)
plot(e$iter, e$train_mlogloss, col = "blue") 
lines(e$iter, e$test_mlogloss, col = "red") 
# Ok now the gap between two line is narrower! Good!!


bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = 499,
                       watchlist = watchlist,
                       # default is 0.3 and range from 0 to 1
                       eta = 0.01, # learning rate 
                       max.depth = 3,
                       gamma = 0, #  [between 0 and infinity ] if larger -> more conservative algorithm
                       subsample = 1,# [between 0 and 1 ] if lower -> prevent overfitting
                       colsample_bytree = 1,
                       missing = NA,
                       seed = 0623) 

#plot..
e <- data.frame(bst_model$evaluation_log)
plot(e$iter, e$train_mlogloss, col = "blue") 
lines(e$iter, e$test_mlogloss, col = "red") 

min(e$test_mlogloss) 
e[e$test_mlogloss == 0.575499,]

# so change with over number and run all the way 
p <- predict(bst_model, newdata = test_matrix)
pred <- matrix(p, nrow = nc, ncol = length(p)/nc) %>% 
  t() %>% 
  data.frame() %>% 
  mutate(label = test_label, max_prob = max.col(.,"last")-1)
table(Prediction = pred$max_prob, Actual = pred$label)

## Conclustion:
## Change param in XGBOOST model to have the best