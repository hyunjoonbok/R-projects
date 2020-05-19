## == Model Interpretability with DALEX == ##
# load required packages
require(rsample)
require(dplyr)
require(h2o)

# initialize h2o session
h2o.init()

# Load classification data
df <- rsample::attrition %>% 
  mutate_if(is.ordered, factor, ordered = FALSE) %>%
  mutate(Attrition = recode(Attrition, "Yes" = "1", "No" = "0") %>% factor(levels = c("1", "0")))

# convert to h2o object
df.h2o <- as.h2o(df)

# create train, validation, and test splits
set.seed(0623)
splits <- h2o.splitFrame(df.h2o, ratios = c(.7, .15), destination_frames = c("train","valid","test"))
names(splits) <- c("train","valid","test")

# variable names for resonse & features
y <- "Attrition"
x <- setdiff(names(df), y)

# elastic net model 
glm <- h2o.glm(
  x = x, 
  y = y, 
  training_frame = splits$train,
  validation_frame = splits$valid,
  family = "binomial",
  seed = 0623
)

# random forest model
rf <- h2o.randomForest(
  x = x, 
  y = y,
  training_frame = splits$train,
  validation_frame = splits$valid,
  ntrees = 1000,
  stopping_metric = "AUC",    
  stopping_rounds = 10,         
  stopping_tolerance = 0.005,
  seed = 0623
)

# gradient boosting machine model
gbm <-  h2o.gbm(
  x = x, 
  y = y,
  training_frame = splits$train,
  validation_frame = splits$valid,
  ntrees = 1000,
  stopping_metric = "AUC",    
  stopping_rounds = 10,         
  stopping_tolerance = 0.005,
  seed = 123
)

# model performance
h2o.auc(glm, valid = TRUE)
h2o.auc(rf, valid = TRUE)
h2o.auc(gbm, valid = TRUE)



# Although DALEX does have native support for some ML model objects (i.e. lm, randomForest), 
# it does not have native many of the preferred ML packages produced more recently 
# (i.e. h2o, xgboost, ranger). To make DALEX compatible with these objects, we need these 3 things:

# x_valid: Our feature set needs to be in its original form not as an h2o object.
# y_valid: Our response variable needs to be a numeric vector. For regression problems this is simple, as it will already be in this format. For binary classification this requires you to convert the responses to 0/1.
# pred: a custom predict function that returns a vector of numeric values. For binary classification problems, this means extracting the probability of the response.

require(DALEX)
# convert feature data to non-h2o objects
x_valid <- as.data.frame(splits$valid)[, x]

# make response variable numeric binary vector
y_valid <- as.vector(as.numeric(as.character(splits$valid$Attrition)))
head(y_valid)

# create custom predict function
pred <- function(model, newdata)  {
  results <- as.data.frame(h2o.predict(model, as.h2o(newdata)))
  return(results[[3L]])
}

pred(rf, x_valid) %>% head()


# Once you have these three components, you can now create your explainer objects for each ML model.
# Considering I used a validation set to compute the AUC, we want to use that same validation set for ML interpretability.

# elastic net explainer
explainer_glm <- explain(
  model = glm,
  data = x_valid,
  y = y_valid,
  predict_function = pred,
  label = "h2o glm"
)

# random forest explainer
explainer_rf <- explain(
  model = rf,
  data = x_valid,
  y = y_valid,
  predict_function = pred,
  label = "h2o rf"
)

# GBM explainer
explainer_gbm <- explain(
  model = gbm,
  data = x_valid,
  y = y_valid,
  predict_function = pred,
  label = "h2o gbm"
)


summary(explainer_glm)

# Residual Diagnostics


# Assessing residuals of predicted versus actuals can allow you to identify where models deviate in their predictive accuracy
# compute predictions & residuals
resids_glm <- model_performance(explainer_glm);resids_glm
resids_rf  <- model_performance(explainer_rf);resids_rf
resids_gbm <- model_performance(explainer_gbm);resids_gbm


## In this example, the residuals are comparing the probability of attrition to the binary attrition value (1-yes, 0-no). 
## Looking at the quantiles you can see that the median residuals are lowest for the GBM model
## The plot also shows that GBM has the lowest residual
## !! Thus, although the GBM model had the lowest AUC score, it actually performs best when considering the median absoluate residuals ##

# create comparison plot of residuals for each model
p1 <- plot(resids_glm, resids_rf, resids_gbm)
p2 <- plot(resids_glm, resids_rf, resids_gbm, geom = "boxplot")

gridExtra::grid.arrange(p1, p2, nrow = 1)

# Looking at the quantiles you can see that the median residuals are lowest for the GBM model
# And looking at the boxplots you can see that the GBM model also had the lowest median absolute residual value. 
# Thus, although the GBM model had the lowest AUC score, it actually performs best when considering the median absoluate residuals

#  However, you can also see a higher number of residuals in the tail of the GBM residual distribution (left plot) 
# suggesting that there may be a higher number of large residuals compared to the GLM model.


# Variable importance

# compute permutation-based variable importance
vip_glm <- variable_importance(explainer_glm, n_sample = -1, loss_function = loss_root_mean_square) 
vip_rf  <- variable_importance(explainer_rf, n_sample = -1, loss_function = loss_root_mean_square)
vip_gbm <- variable_importance(explainer_gbm, n_sample = -1, loss_function = loss_root_mean_square)

plot(vip_glm, vip_rf, vip_gbm, max_vars = 10)

