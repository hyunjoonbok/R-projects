##==== Ordinal logistic regression ====##
# Ordinal logistic regression can be used to model a ordered factor response.
# Use "polr() from the MASS package"
# We can use ordered logistic regression to predict the cars evaluation based on cars evaluation dataset using different attributes

carsdata <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data", stringsAsFactors=F)
colnames(carsdata) <- c("buying", "maint", "doors", "persons", "lug_boot", "safety", "class")

# 1. Reorder the levels of factors
## In order logistic regression, the order of the levels in the factor variables matters. 
## So, lets define them explicitly. This is an critical step

carsdata$buying <- factor(carsdata$buying, levels=c("low", "med", "high", "vhigh"), ordered=TRUE)
carsdata$maint <- factor(carsdata$maint, levels=c("low", "med", "high", "vhigh"), ordered=TRUE)
carsdata$doors <- factor(carsdata$doors, levels=c("2", "3", "4", "5more"), ordered=TRUE)
carsdata$persons <- factor(carsdata$persons, levels=c("2", "4", "more"), ordered=TRUE)
carsdata$lug_boot <- factor(carsdata$lug_boot, levels=c("small", "med", "big"), ordered=TRUE)
carsdata$safety <- factor(carsdata$safety, levels=c("low", "med", "high"), ordered=TRUE)
carsdata$class <- factor(carsdata$class, levels=c("unacc", "acc", "good", "vgood"), ordered=TRUE)

# 2. Training/Test Data
set.seed(0623)
b <- sample(1:nrow(carsdata), 0.7 * nrow(carsdata))
training <- carsdata[b, ]
test <- carsdata[-b, ]

# 3. Build the ordered logistic regression model
require(MASS)
polrModel <- polr(class ~ safety + lug_boot + doors + buying + maint, data=training)
# gives error when "persons" is included...
summary(polrModel)

# 4. Predict on test data
predicted_cars <- predict(polrModel, test)  # predict the classes directly
head(predicted_cars)
predictedScores <- predict(polrModel, test, type="p")
head(predictedScores)

# 5. Diagonostics
# Confusion matrix 
table(test$class, predicted_cars)
# misclassification error
mean(as.character(test$class) != as.character(predicted_cars))
# Low! Good!