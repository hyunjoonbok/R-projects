# Naïve Bayes Classifier #

## The Naïve Bayes classifier is a simple probabilistic classifier which is based on Bayes theorem but with strong assumptions regarding independence. 
## Historically, this technique became popular with applications in email filtering, spam detection, and document categorization

# 1. load package
require(rsample)  # data splitting 
require(dplyr)    # data transformation
require(ggplot2)  # data visualization
require(caret)    # implementing with caret
require(h2o)
require(corrplot)

# 2. we will use the attrition data that has been included in the rsample package. 
#   The goal is to predict employee attrition.

# conver some numeric variables to factors
attrition <- attrition %>%
  mutate(
    JobLevel = factor(JobLevel),
    StockOptionLevel = factor(StockOptionLevel),
    TrainingTimesLastYear = factor(TrainingTimesLastYear)
  )

# Create training (70%) and test (30%) sets for the attrition data.
# Use set.seed for reproducibility
set.seed(0623)
split <- initial_split(attrition, prop = .7, strata = "Attrition")
train <- training(split)
test  <- testing(split)

# distribution of Attrition rates across train & test set
table(train$Attrition) %>% prop.table()
table(test$Attrition) %>% prop.table()


# 3. The simplified classifier
## With naïve Bayes, we assume that the predictor variables are conditionally independent of one another given the response value.

train %>%
  filter(Attrition == "Yes") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot()

## Advantages and Shortcomings
# The naïve Bayes classifier is simple (both intuitively and computationally), fast, performs well with small amounts of training data, and scales well to large data sets. 
# The greatest weakness of the naïve Bayes classifier is that it relies on an often-faulty assumption of equally important and independent features which results in biased posterior probabilities. 
## Although this assumption is rarely met, in practice, this algorithm works surprisingly well. 
## This is primarily because what is usually needed is not a propensity (exact posterior probability) for each record that is accurate in absolute terms but just a reasonably accurate rank ordering of propensities


# 4. Implementation

# 1) First, we apply a naïve Bayes model with 10-fold cross validation, which gets 83% accuracy
# Considering about 83% of our observations in our training set do not attrit, our overall accuracy is no better than if we just predicted "No" attrition for every observation.
# --> SO not good


# create response and feature data
features <- setdiff(names(train), "Attrition")
x <- train[, features]
y <- train$Attrition
# set up 10-fold cross validation procedure
train_control <- trainControl(
  method = "cv", 
  number = 10
)
# train model
nb.m1 <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control
)
# results
confusionMatrix(nb.m1)



# 2) We can tune the few hyperparameters that a naïve Bayes model has.

# "Usekernel" parameter allows us to use a kernel density estimate for continuous variables versus a guassian density estimate,
# "Adjust" allows us to adjust the bandwidth of the kernel density (larger numbers mean more flexible density estimate),
# "fL" allows us to incorporate the Laplace smoother.
# Normalize with Box Cox, standardize with center-scaling, and reducing with PCA 


# set up tuning grid
search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0:5,
  adjust = seq(0, 5, by = 1)
)
# train model
nb.m2 <- train(
  x = x,
  y = y,
  method = "nb",
  trControl = train_control,
  tuneGrid = search_grid,
  preProc = c("BoxCox", "center", "scale", "pca")
)
# top 5 models
nb.m2$results %>% 
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))
# plot it
plot(nb.m2)
# results 
confusionMatrix(nb.m2)


# 3) Try Prediction with above model
pred <- predict(nb.m2, newdata = test)
head(pred)
confusionMatrix(pred, test$Attrition)
## Its obvious that our model is not capturing a large percentage of our actual attritions (illustrated by our low specificity).


# 4) Start up h2o:
h2o.init()

# create feature names
y <- "Attrition"
x <- setdiff(names(train), y)

# h2o cannot ingest ordered factors
train.h2o <- train %>%
  mutate_if(is.factor, factor, ordered = FALSE) %>%
  as.h2o()

# train model
nb.h2o <- h2o.naiveBayes(
  x = x,
  y = y,
  training_frame = train.h2o,
  nfolds = 10,
  laplace = 0
)

# assess results
h2o.confusionMatrix(nb.h2o)




# 5) Let's do some feature preprocessing as we did with caret and tune the Laplace smoother using h2o.grid

# do a little preprocessing
preprocess <- preProcess(train, method = c("BoxCox", "center", "scale", "pca"))
train_pp   <- predict(preprocess, train)
test_pp    <- predict(preprocess, test)

# convert to h2o objects
train_pp.h2o <- train_pp %>%
  mutate_if(is.factor, factor, ordered = FALSE) %>%
  as.h2o()

test_pp.h2o <- test_pp %>%
  mutate_if(is.factor, factor, ordered = FALSE) %>%
  as.h2o()

# get new feature names --> PCA preprocessing reduced and changed some features
x <- setdiff(names(train_pp), "Attrition")

# create tuning grid
hyper_params <- list(
  laplace = seq(0, 5, by = 0.5)
)

# build grid search 
grid <- h2o.grid(
  algorithm = "naivebayes",
  grid_id = "nb_grid",
  x = x, 
  y = y, 
  training_frame = train_pp.h2o, 
  nfolds = 10,
  hyper_params = hyper_params
)

# Sort the grid models by mse
sorted_grid <- h2o.getGrid("nb_grid", sort_by = "accuracy", decreasing = TRUE);sorted_grid

# grab top model id
best_h2o_model <- sorted_grid@model_ids[[1]]
best_model <- h2o.getModel(best_h2o_model)

# confusion matrix of best model
h2o.confusionMatrix(best_model)

# ROC curve
auc <- h2o.auc(best_model, xval = TRUE)
fpr <- h2o.performance(best_model, xval = TRUE) %>% h2o.fpr() %>% .[['fpr']]
tpr <- h2o.performance(best_model, xval = TRUE) %>% h2o.tpr() %>% .[['tpr']]
data.frame(fpr = fpr, tpr = tpr) %>%
  ggplot(aes(fpr, tpr) ) +
  geom_line() + 
  ggtitle( sprintf('AUC: %f', auc) )


# 6) Once we've identified the optimal model, we can assess on our test set.
h2o.performance(best_model, newdata = test_pp.h2o)


h2o.shutdown(prompt = FALSE)
