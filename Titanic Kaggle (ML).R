##==== Machine Learning (Titatic Kaggle Competition) ##====
setwd("C:/Users/Benson/Desktop")
require(randomForest)

titanic.train <- read.csv("train.csv", stringsAsFactors = FALSE)
titanic.test <- read.csv("test.csv", stringsAsFactors = FALSE)

##==== Combine the file together ##====
titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE

#they have to line up
#so create additional "Survived" column in test-set
titanic.test$Survived <- NA
titanic.full <- rbind(titanic.train,titanic.test)


##==== Clean the missing value ##====
##Origianlly it should be calculated with regression with missing value!!
# Replacing "Embarked" missing value with S (which has the largest) (not the best...)
titanic.full[titanic.full$Embarked == '', "Embarked"] <- 'S'
table(titanic.full$Embarked)

#==== Replacing "Age" missing values with linear regression
#titanic.full[is.na(titanic.full$Age), "Age"] <- median(titanic.full$Age, na.rm = TRUE)
table(titanic.full$Age)
boxplot(titanic.full$Age)
boxplot.stats(titanic.full$Age)
upper.whisker.age <- boxplot.stats(titanic.full$Age)$stats[5]
outlier.filter.age.upper <- titanic.full$Age < upper.whisker.age
outlier.filter.age.upper

titanic.full[outlier.filter.age.upper,]

titanic.new <- titanic.full[outlier.filter.age.upper,]

#
age.model <- lm(Age ~ Pclass + Fare + Sex + SibSp + Parch + Embarked, data = titanic.new)

age.row <- titanic.full[is.na(titanic.full$Age), c("Pclass","Fare","Sex","SibSp", "Parch","Embarked")]
age.predictions <- predict(age.model,newdata = age.row)

titanic.full[is.na(titanic.full$Age), "Age"] <- age.predictions

#==== Replacing "Fare" missing value with linear regression
# titanic.full[is.na(titanic.full$Fare), "Fare"] <- median(titanic.full$Fare, na.rm = TRUE)
boxplot(titanic.full$Fare)
boxplot.stats(titanic.full$Fare)
upper.whisker <- boxplot.stats(titanic.full$Fare)$stats[5]
outlier.filter <- titanic.full$Fare < upper.whisker
outlier.filter
# if there's outlier below, you need to do that too!

titanic.full[outlier.filter,] #this gives all the row except outliers
# Now build a model
str(titanic.full)
fare.model <- lm(Fare~Pclass + Sex + Age + SibSp + Parch + Embarked, data = titanic.full[outlier.filter,])

# make data set to have ones 
fare.row <- titanic.full[is.na(titanic.full$Fare),c("Pclass","Sex","Age","SibSp","Parch", "Embarked")]
fare.predictions <- predict(fare.model, newdata = fare.row)

#Replace missing values with the ones we just found
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.predictions

##

##==== Categorical casting ##====
str(titanic.full)
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)

##==== Re-dividing into train/test ##====
titanic.train <- titanic.full[titanic.full$IsTrainSet==TRUE,]
titanic.test <- titanic.full[!titanic.full$IsTrainSet==TRUE,]


##==== Categorical casting one-more ##====
titanic.train$Survived <- as.factor(titanic.train$Survived)


##==== Build a predictive model! ##====
## Here we skip 70/30 split and Cross Validation!!

titanic.model <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch  + Fare + Embarked, data = titanic.train,ntree=500, mtry = 3, nodesize = 0.01 * nrow(titanic.train))
Survived <- predict(titanic.model , newdata = titanic.test)
Survived
PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived

write.csv(output.df,file = "kaggle_submission.csv", row.names = FALSE)

