# XGBoost to solove Samsung Data Competition (Incomplete!)

## =========================================================================== ##

require(Boruta)
require(mlbench)
require(readxl)
require(lubridate)
require(xgboost)
require(keras)
require(tensorflow)
require(lime)
require(rsample)
require(recipes)
require(yardstick)
require(corrr)
require(caret)
require(knitr)
require(Matrix)
require(caret)      
require(tidyverse)  
require(rsample) 
# Feature Selection

set.seed(0623)
train_set <- as.data.frame(df.h2o)
boruta1 <- Boruta(The.type.of.violation_sub.category ~., data = train_set, doTrace = 2)

plot(boruta1, las = 2, cex.axis = 0.6)

head(train_set)


# Tentative Fix
bor = TentativeRoughFix(boruta1)
print(bor)

attStats(boruta1)

getNonRejectedFormula(boruta1)




# Split data
set.seed(0623)
inTrain <- createDataPartition(y = train_set$Day.of.the.week, p = 0.7, list = FALSE)
training <- train_set[inTrain,]
testing <- train_set[-inTrain,]


rec_obj <- recipe(Day.and.night ~ ., 
                  data = training) %>% 
  
  step_center(all_numeric(), -all_outcomes()) %>% 
  step_scale(all_numeric(), -all_outcomes()) %>% 
  step_dummy(all_nominal()) %>%
  #step_log(X_UTMK, Y_UTMK) %>%
  
  step_discretize(Number.of.casualties) %>%
  step_discretize(Number.of.seriously.injured.person) %>%
  step_discretize(Number.of.slightly.injured.person) %>%
  step_discretize(Injury.claimant.number) %>% 
  
  step_YeoJohnson(all_predictors()) %>% 
  step_bs(Longitude, Latitude) %>% 
  step_knnimpute(all_numeric(), K = 3) %>% 
  
  
  prep(training = training, retain = TRUE)


# Predictors
x_train_tbl <- bake(rec_obj, newdata = training, everything())
x_test_tbl  <- bake(rec_obj, newdata = testing, everything())


#-------------Basic Training using XGBoost in caret Library-----------------
# Set up control parameters for caret::train
# Here we use 10-fold cross-validation, repeating twice, and using random search for tuning hyper-parameters.
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 2, search = "random", allowParallel = TRUE,
                           verboseIter = FALSE)

# train a xgbTree model using caret::train
model_1 <- train(factor(Day.and.night)~., data = training, method = "xgbTree", trControl = fitControl)
model_2 <- train(factor(Day.and.night)~., data = training, method = "xgbLinear", trControl = fitControl)


# or 
model_3 <-train(Day.and.night_night ~ .,
                data = x_train_tbl,
                method = "xgbTree",
                trControl = trainControl(method = "repeatedcv", number = 5, repeats = 10, allowParallel = TRUE, verboseIter = FALSE))

# See model results
print(model_1)
print(model_2)

print(model_3)


## ================================================================================================== ##

training$Day.of.the.week <- ifelse(training$Day.of.the.week == "Monday", 1,( 
  ifelse(training$Day.of.the.week == "Tuesday",2,(
    ifelse(training$Day.of.the.week == "Wednesday",3,(
      ifelse(training$Day.of.the.week == "Thursday",4,(
        ifelse(training$Day.of.the.week == "Friday",5,(
          ifelse(training$Day.of.the.week == "Saturday",6,7)))))))))))


testing$Day.of.the.week <- ifelse(testing$Day.of.the.week == "Monday", 1,( 
  ifelse(testing$Day.of.the.week == "Tuesday",2,(
    ifelse(testing$Day.of.the.week == "Wednesday",3,(
      ifelse(testing$Day.of.the.week == "Thursday",4,(
        ifelse(testing$Day.of.the.week == "Friday",5,(
          ifelse(testing$Day.of.the.week == "Saturday",6,7)))))))))))



training$Day.and.night <- ifelse(pull(training, Day.and.night) == "day", 1, 0)
testing$Day.and.night <- ifelse(pull(testing, Day.and.night) == "day", 1, 0)

# training preprocess
train_label <- training[,3]
train_label <- as.numeric(training[,3]) -1 
training <- training[,-3] # remove label from original
#training <- training[-c(train_label == 9),]
trainm <- sparse.model.matrix(Day.of.the.week ~.-1, data = training)
train_matrix <- xgb.DMatrix(data = as.matrix(trainm), label = train_label)

# testing preprocess
test_label <- as.numeric(testing[,3]) -1 
testing <- testing[,-3]
testm <- sparse.model.matrix( ~.-1 ,data = testing)
test_matrix <- xgb.DMatrix(data = as.matrix(testm), label = test_label)


watchlist <- list(train = train_matrix,
                  test = test_matrix)

cv_model <- xgb.cv(data = train_matrix,
                   nthread = 3,  # number of threads allocated to the execution of XGBoost
                   nfold = 10,  # the original data is divided into 5 eqqual random samples
                   nrounds = 10000, # number of iterations
                   max_depth = 8, # maximum depth of a tree
                   eta = 0.1, # controls the learning rate. 0 < eta < 1
                   subsample = 0.70, #subsample ratio of the training instance. 
                   colsample_bytree = 0.70, #subsample ratio of columns when constructing each tree
                   booster = "gbtree", # gbtree or gblinear
                   eval_metric = "error", #binary classification error rate
                   maximize = FALSE, #maximize=TRUE means the larger the evaluation score the better
                   early_stopping_rounds = 70, # training with a validation set will 
                   # stop if the performance keeps getting worse 
                   # consecutively for k rounds.
                   objective = "binary:logistic", # logistic regression
                   print_every_n = 10, # output is printed every 10 iterations
                   verbose = TRUE) # print the output 



temp_model <- xgb.train(data = train_matrix,
                        nthread = 3,
                        nrounds = 50,
                        max_depth = 8,
                        eta = 0.1,
                        subsample = 0.70,
                        colsample_bytree = 0.70,
                        booster = "gbtree",
                        eval_metric = "error",
                        maximize = FALSE,
                        objective = "binary:logistic",
                        print_every_n = 10,
                        verbose = TRUE,
                        watchlist = watchlist)

imp <- xgb.importance(colnames(train_matrix),model = temp_model); imp
xgb.plot.importance(imp, top_n = 10)

pred <- predict(temp_model, test_matrix)
print(head(pred))

prediction <- as.numeric(pred > 0.5)
print(head(prediction))
confusionMatrix(as.factor(prediction), as.factor(test_label))








# Multiclassification
nc <- length(unique(test_label));nc

param <- list("objective" = "multi:softmax", 
              "eval_metric" = "mlogloss",
              "eta" = 0.1, 
              "max.depth" = 4, 
              "subsample" = 0.5,
              "colsample_bytree" = 1,
              "nthread" = 3,
              "num_class" = nc
)

bst_model <- xgb.train(params = param,
                       data = train_matrix,
                       nrounds = 250,
                       watchlist = watchlist)

bst_model

imp <- xgb.importance(colnames(train_matrix),model = bst_model); imp
xgb.plot.importance(imp, top_n = 10)
xgb.plot.tree(feature_names = colnames(train_matrix), model = bst_model, trees = 0)


e <- data.frame(bst_model$evaluation_log)
plot(e$iter, e$train_mlogloss, col = "blue") #traning error
lines(e$iter, e$test_mlogloss, col = "red") # add test error

#
chi_test <- chisq.test(training$Perpetrator.of.a.traffic.accident_main.category, train_label)
chi_test <- chisq.test(training$Number.of.death, train_label)

print(chi_test)


# Prediction
pred <- predict(bst_model, test_matrix)

# Accuracy of prediction (good if close to 1)
sum(diag(table(test_label ,pred)))/nrow(testing)

labels <- getinfo(test_matrix, 'label')
cat('error of preds=', mean(as.numeric(pred>0.5)!=labels),'\n') #test-error

# binary transformation
prediction <- as.numeric(pred > 0.5)
print(head(prediction))
confusionMatrix(as.factor(prediction), as.factor(test_label))






# or
model <- xgboost(params = param, data = as.matrix(x_train_tbl), label = train_label, nrounds = 200)
model

imp <- xgb.importance(colnames(train_matrix),model = model); imp
xgb.plot.importance(imp, top_n = 10)

e <- data.frame(model$evaluation_log)
plot(e$iter, e$train_rmse, col = "blue") #traning error
lines(e$iter, e$test_rmse, col = "red")



# Prediction
pred1 <- predict(bst_model, newdata = as.matrix(test_label))
head(pred1)

pred2 <- predict(model, newdata = as.matrix(test_label))
head(pred2)

# when AUC
require(AUC)
auc(roc(pred1, as.factor(test_label)))
auc(roc(pred2, as.factor(test_label)))



# or
ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE, summaryFunction = twoClassSummary)
grid <- expand.grid(.interaction.depth = seq(1,7,by=2), 
                    .n.trees = seq(100,1000,by=50),
                    .shrinkage = c(0.01,0.1),
                    .n.minobsinnode = c(10))
Fit_tune <- train(Day.and.night ~.,data = train_matrix, method= "rf", 
                  verbose = FALSE, trControl = ctrl, metric = "ROC", 
                  tuneGrid = grid)

head(x_train_tbl)

