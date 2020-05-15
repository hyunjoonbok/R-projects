##==== Ridge Regression ====##
# Ridge Regression is a commonly used technique to address the problem of "multi-collinearity"

# Prepare data
require(ridge)
require(car)
data(longley)  
inputData <- data.frame (longley) # plug in your data here
colnames(inputData)[1] <- "response"  # rename response var

# Calculate Correlations
XVars <- inputData[, -1] # X variables
round(cor(XVars), 2) # Correlation Test

# 1. Training/Test Data
set.seed(0623) 
c <- sample(1:nrow(inputData), 0.7*nrow(inputData)) 
trainingData <- inputData[c, ] 
testData <- inputData[-c, ] 

# 2. Predict Using Linear Regression vs Using Ridge Regression
# Linear Regression
lmMod <- lm(response ~ ., trainingData) 
summary (lmMod) 
vif(lmMod) 
# There is significant multi-collinearity between GNP & Year and Population & Employed

predicted <- predict (lmMod, testData)  # predict on test data
compare <- cbind(actual=testData$response, predicted)  # combine actual and predicted
mean (apply(compare, 1, min)/apply(compare, 1, max)) # calculate accuracy
#>  98.76%

# Ridge Regression
linRidgeMod <- linearRidge(response ~ ., data = trainingData)
summary(linRidgeMod)
predicted <- predict(linRidgeMod, testData)  # predict on test data
compare <- cbind(actual=testData$response, predicted)  # combine
mean (apply(compare, 1, min)/apply(compare, 1, max)) # calculate accuracy
#>  99.10%

