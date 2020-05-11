##==== Logistic Regression ====##
# This predcits binary classification
# Lets try and predict if an individual will earn more than $50K using logistic regression based on demographic variables available in the adult data

inputData <- read.csv("http://rstatistics.net/wp-content/uploads/2015/09/adult.csv")
head(inputData)

## Check Class bias
table(inputData$ABOVE50K)
# there is a class bias, a condition observed when the proportion of events is much smaller than proportion of non-events
# So, sample to have equal amount from both

# Create Training/Test sets
input_ones <- inputData[which(inputData$ABOVE50K==1),]
input_zeros <- inputData[which(inputData$ABOVE50K==0),]
set.seed(0623)
input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones))
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_ones))  # 0's for training. Pick as many 0's as 1's
training_ones <- input_ones[input_ones_training_rows, ]  
training_zeros <- input_zeros[input_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's 

test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's 

##++++ Optional ++++ ##

# Compute Information Values
# The smbinning::smbinning function converts a continuous variable into a categorical variable using recursive partitioning
library(smbinning)
# segregate continuous and factor variables
factor_vars <- c ("WORKCLASS", "EDUCATION", "MARITALSTATUS", "OCCUPATION", "RELATIONSHIP", "RACE", "SEX", "NATIVECOUNTRY")
continuous_vars <- c("AGE", "FNLWGT","EDUCATIONNUM", "HOURSPERWEEK", "CAPITALGAIN", "CAPITALLOSS")

iv_df <- data.frame(VARS=c(factor_vars, continuous_vars), IV=numeric(14))  # init for IV results

# compute IV for categoricals
for(factor_var in factor_vars){
  smb <- smbinning.factor(trainingData, y="ABOVE50K", x=factor_var)  # WOE table
  if(class(smb) != "character"){ # heck if some error occured
    iv_df[iv_df$VARS == factor_var, "IV"] <- smb$iv
  }
}

# compute IV for continuous vars
for(continuous_var in continuous_vars){
  smb <- smbinning(trainingData, y="ABOVE50K", x=continuous_var)  # WOE table
  if(class(smb) != "character"){  # any error while calculating scores.
    iv_df[iv_df$VARS == continuous_var, "IV"] <- smb$iv
  }
}

iv_df <- iv_df[order(-iv_df$IV), ]  # sort
iv_df

##++++ Optional ++++ ##

# Build Logistic Regression Models and Predict
logitMod <- glm(ABOVE50K ~ RELATIONSHIP + AGE + CAPITALGAIN + OCCUPATION + EDUCATIONNUM, data=trainingData, family=binomial)

predicted <- predict(logitMod, testData)  # predicted scores in log-odds
new_predicted <- plogis(predicted)
list(new_predicted)
# The glm() procedure with family="binomial" will build the logistic regression model on the given formula. 
# When we use the predict function on this model, it will predict the log(odds) of the Y variable. 
# This is not what we ultimately want because, the predicted values may not lie within the 0 and 1 range as expected. 
# So, to convert it into prediction probability scores that is bound between 0 and 1, we use the plogis().

## Model Diagnostics After
library(InformationValue)
optCutOff <- optimalCutoff(testData$ABOVE50K, predicted)[1] 
summary(logitMod)

vif(logitMod) # should be less than 4

# Misclassification Error
misClassError(testData$ABOVE50K, predicted, threshold = optCutOff)
# Misclassification error is the percentage mismatch of predcited vs actuals, irrespective of 1's or 0's. The lower the misclassification error, the better is your model.
