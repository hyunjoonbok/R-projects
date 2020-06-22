##==== Multinomial Regression ====##
# Multinomial regression is similar to logistic regression 
# but is better when the response variable is a categorical variable with more than 2 levels.
# Use either "mlogit() from mlogit package" or "multinom() from nnet package"
cmc <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/cmc/cmc.data", stringsAsFactors=FALSE)
colnames(cmc) <- c("wife_age", "wife_edu", "hus_edu", "num_child", "wife_rel", "wife_work", "hus_occu", "sil", "media_exp", "cmc")

# 1. Convert numeric to factors
cmc$wife_edu <- factor(cmc$wife_edu)
cmc$hus_edu <- factor(cmc$hus_edu)
cmc$wife_rel <- factor(cmc$wife_rel)
cmc$wife_work <- factor(cmc$wife_work)
cmc$hus_occu <- factor(cmc$hus_occu)
cmc$sil <- factor(cmc$sil)
cmc$media_exp <- factor(cmc$media_exp)
cmc$cmc <- factor(cmc$cmc)

# 2. Training/Test Data set
set.seed(0623)
a <- sample(1:nrow(cmc), 0.7*nrow(cmc))
traning <- cmc[a,]
test <- cmc[-a,]

# 3. Make Multinomial regression model
require(nnet)
multi <- multinom(cmc ~ ., data = traning)
summary(multi)

# 4. Make prediction using test data and above model
predicted_cmc <- predict(multi, test)
list(predicted_cmc)

# 5. Diagonostics
table(predicted_cmc, test$cmc)
mean(as.character(predicted_cmc) != as.character(test$cmc))
# A misclassification error of 49.3% is probably too high. 
# May be it can be improved by improving the model terms or may be the variables are not as good in explaining the contraceptive method used. 
# Either ways, I would encourage the investigator to try other ML approaches as well for this problem.