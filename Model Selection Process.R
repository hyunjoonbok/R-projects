##==== Model Selection ====##

## Data Prepration
inputData <- read.csv("http://rstatistics.net/wp-content/uploads/2015/09/ozone2.csv", stringsAsFactors=F)
response_df <- inputData['ozone_reading']  # Y variable
predictors_df <- inputData[, !names(inputData) %in% "ozone_reading" ]  # X variables

# Stepwise Regression
lmMod <- lm(ozone_reading ~ . , data = inputData)
selectedMod <- step(lmMod) # find the equation that has the highest adj-Rsq
require(car)
all_vifs <- vif(selectedMod);all_vifs # Checking multicollinerity 
# Simply remove variables with VIF > 4 !!
## And new lm's varaibles' p-values should be statistically significant.

# Best subsets
# Best subsets is a technique that relies on stepwise regression to search, find and visualise regression models
require(leaps)
regsubsetsObj <- regsubsets(x=predictors_df ,y=response_df, nbest = 2, really.big = T)
plot(regsubsetsObj, scale = "adjr2")  # regsubsets plot based on R-sq
# For instance, draw an imaginary horizontal line along the X-axis from any point along the Y-axis. That line would correspond to a linear model, where, the black boxes that line touches form the X variables
# The caveat however is that it is not guaranteed that these models will be statistically significant.

# Leaps
# Leaps is similar to best subsets but is known to use a better algorithm to shortlist the models

leapSet <- leaps(x=predictors_df, y=inputData$ozone_reading, nbest = 1, method = "adjr2")
summary(leapSet)


## Anova!!
## If you have two or more models that are subsets of a larger model, 
## you can use anova() to check if the additional variable(s) contribute to the predictive ability of the model.