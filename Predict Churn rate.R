# Caret Package 
require(caret)
require(C50)
require(pROC)
require(e1071)

data(churn)
str(churnTrain)

allData <- rbind(churnTrain,churnTest)
# column names except churn
predictors <- names(churnTrain)[names(churnTrain) != "churn"]

# Data split
set.seed(0623)
inTrainingSet <- createDataPartition(allData$churn, p = 0.75, list = FALSE)
churnTrain <- allData[inTrainingSet,]
churnTest <- allData[-inTrainingSet,]

# Data Pre-processing
# calcuates followings that can be used to apply to any dataset (very useful)
# "Centering","scaling","spatial sign transformation", "PCA", "Box-Cox" transformation", etc
numerics <- c("account_length", "total_day_calls", "total_night_calls")
# To determin means and standard deviations,
procValues <- preProcess(churnTrain[,numerics], method = c("center","scale","YeoJohnson"))
# Then predict to do the adjustments
trainScaled <- predict(procValues,churnTrain[,numerics])
testScaled <- predict(procValues,churnTest[,numerics])


## Using "gbm" package
require(gbm)
forGBM <- churnTrain
forGBM$churn <- ifelse(forGBM$churn == "yes",1,0)

# Build a gbm model
Fit <- gbm(formula = churn ~., distribution = "bernoulli",data = forGBM, n.trees = 2000, interaction.depth = 7, shrinkage = 0.01,verbose = FALSE)

# ????????? ???????????????


## Tuning the model
Fit_tune <- train(churn ~.,data = churnTrain, method= "gbm", verbose = FALSE)

# But by default, train uses bootstrap for tuning
# we can use 5 repeats for 10-folf cv and then tune the model
ctrl <- trainControl(method = "repeatedcv", repeats = 5)
Fit_tune <- train(churn ~.,data = churnTrain, method= "gbm", 
                  verbose = FALSE, trControl = ctrl)

# so we want to tune the model based on area under ROC curve!
# and pick the model with largest AUC

# so we need to tell "train" function above to produce class probabilites and rank models by area under ROC curve!
# twoClassSummaray calculates sensitivity, specificity and Area under ROC curve
ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE, summaryFunction = twoClassSummary)
Fit_tune <- train(churn ~.,data = churnTrain, method= "gbm", 
                  verbose = FALSE, trControl = ctrl, metric = "ROC")

# Also, "train" uses a minimal search grid: only 3 value for each tuning parameter
# Let's look at tree depths from 1 to 7, boosting iterations from 100 to 1,000 and two different learning rates
# We can create a data-frame with these parameters
# Make sure the column names in grid should be . + parameter names  
ctrl <- trainControl(method = "repeatedcv", repeats = 5,
                     classProbs = TRUE, summaryFunction = twoClassSummary)
grid <- expand.grid(.interaction.depth = seq(1,7,by=2), 
                    .n.trees = seq(100,1000,by=50),
                    .shrinkage = c(0.01,0.1),
                    .n.minobsinnode = c(10))
Fit_tune <- train(churn ~.,data = churnTrain, method= "gbm", 
                  verbose = FALSE, trControl = ctrl, metric = "ROC", 
                  tuneGrid = grid)

# Fit_tune ?????? ????????? ????????? ????????? ???????????? parameter ??? ??????
Fit_tune


# See that in graphical representation
require(ggplot2)
ggplot(Fit_tune) + theme(legend.position = 'top')


# If we preidct, it automatically use the optimal paratmers
gbmPred <- predict(Fit_tune, churnTest);gbmPred
str(gbmPred)

gbmProbs <- predict(Fit_tune,churnTest,type = "prob")
str(gbmProbs)
head(gbmProbs)

# to see the actual precision and recall
confusionMatrix(gbmPred, churnTest$churn)

#To see actual rocCurve!
rocCurve <- roc(response = churnTest$churn,
                predictor = gbmProbs[ ,'yes'],
                levels = rev(levels(churnTest$churn))) ;rocCurve

plot(rocCurve)
plot(rocCurve,
     print.thres = c(.5,.2),
     print.thres.pch = 16,
     print.thres.cex = 1.2)
# x is specificitiy, y is sensitivity
# Gives 2 cutoffs at 0.2 and 0.5 that is close to perfect model (1,1)


################# Additional ###################
# pre-processing using SVM
svmTune <- train(churn~. , data = churnTrain,
                 ## Tell it to fit a SVM model and tune
                 method = "svmRadial",
                 ## This pre-processing will be applied to 
                 ## these data and new samples too
                 preProc = c("center","scale"),
                 ## Tune over different values of cost,
                 tuneLength = 10,
                 trControl = ctrl,
                 metric = "ROC")


fdaTune <- train(churn ~ . ,data = churnTrain,
                 # Now lets' try a flexible discriminant model
                 # using MARS basis functions
                 method = "fda",
                 tuneLength = 10,
                 trControl = ctrl,
                 metric = "ROC")