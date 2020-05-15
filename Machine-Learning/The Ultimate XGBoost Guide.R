#### The Ultimate XGBoost Guide ####

### Build a binary classification model ###


# Load Packages
require(readxl)
require(tidyverse)
require(xgboost)
require(caret)
setwd("C:/Users/bokhy/Desktop/R")
power_plant = as.data.frame(read_excel("Folds5x2_pp.xlsx"))
# PE (Power Output) in the dataset 
# is the value we are trying to predict given the measurements above.

set.seed(0623)
# Create index for testing and training data
inTrain <- createDataPartition(y = power_plant$PE, p = 0.8, list = FALSE)
# subset power_plant data to training
training <- power_plant[inTrain,]
# subset the rest to test
testing <- power_plant[-inTrain,]

# Convert the training and testing sets into DMatrixes

X_train = xgb.DMatrix(as.matrix(training %>% select(-PE)))
y_train = training$PE
X_test = xgb.DMatrix(as.matrix(testing %>% select(-PE)))
y_test = testing$PE

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
                       eta = 0.1,
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
  verbose = FALSE
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
imprt_matrix <- xgb.importance(names,model = bst)
xgb.plot.importance(imprt_matrix[1:10]) # we should take variables with higher value
require(DiagrammeR)
xgb.plot.tree(feature_names = names, model = bst, trees = 2)





##2.
require(xgboost)
require(magrittr)
require(dplyr)
require(Matrix)

# Data
data <- read.csv("binary.csv")
str(data)
data$rank <- as.factor(data$rank) # change it as it is categorical variable

# Split data
set.seed(0623)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8,0.2))
train <- data[ind == 1,]
test <- data[ind == 2,]

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

# now use 0.05 for eta

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
                       max.depth = 3, # overfitting
                       gamma = 0, #  [between 0 and infinity ] if larger -> more conservative algorithm
                       subsample = 1,# [between 0 and 1 ] if lower -> prevent overfitting
                       colsample_bytree = 1, # stronger randomness
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
                       colsample_bytree = 1, # change randomness
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