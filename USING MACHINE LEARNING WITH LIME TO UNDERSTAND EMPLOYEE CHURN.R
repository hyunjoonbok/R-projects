## USING MACHINE LEARNING WITH LIME TO UNDERSTAND EMPLOYEE CHURN

# There's one catch: Complex models are unexplainable. that is until LIME came along! LIME, 
# which stands for Local Interpretable Model-agnostic Explanations, 
# has opened the doors to black-box (complex, high-performance, but unexplainable) models in business applications

## LIME: A SECRET WEAPON FOR ROI-DRIVEN DATA SCIENCE!!

# 1. Libraries
require(lime)       # ML local interpretation
require(vip)        # ML global interpretation
require(pdp)        # ML global interpretation
require(ggplot2)    # visualization pkg leveraged by above packages
require(caret)      # ML model building
require(h2o)        # ML model building
require(tidyverse)  # Use tibble, dplyr
require(rsample)    # Get HR Data via rsample::attrition
require(gridExtra)  # Plot multiple lime plots on one graph
require(ranger)

# initialize h2o
h2o.init()

# 2. Create data sets and h2o objects
df <- rsample::attrition %>% 
  mutate_if(is.ordered, factor, ordered = FALSE) %>%
  mutate(Attrition = factor(Attrition, levels = c("Yes", "No")))

index <- 1:5
train_obs <- df[-index, ]
local_obs <- df[index, ]

y <- "Attrition"
x <- setdiff(names(train_obs), y)
train_obs.h2o <- as.h2o(train_obs)
local_obs.h2o <- as.h2o(local_obs)


# 3. Create  models

# Create Random Forest model with ranger via caret
fit.caret <- train(
  Attrition ~ ., 
  data       = train_obs, 
  method     = 'ranger',
  trControl  = trainControl(method = "cv", number = 5, classProbs = TRUE),
  tuneLength = 1,
  importance = 'impurity'
)

# create h2o models
h2o_rf  <- h2o.randomForest(x, y, training_frame = train_obs.h2o)
h2o_glm <- h2o.glm(x, y, training_frame = train_obs.h2o, family = "binomial")
h2o_gbm <- h2o.gbm(x, y, training_frame = train_obs.h2o)


# ranger model --> model type not built in to LIME
fit.ranger <- ranger::ranger(
  Attrition ~ ., 
  data        = train_obs, 
  importance  = 'impurity',
  probability = TRUE
)


# 4. GLOBAL INTERPRETATION

# The most common ways of obtaining global interpretation is through:
  ## variable importance measures
  ## partial dependence plots
vip(fit.ranger) + ggtitle("ranger: RF")
# After the most globally relevant variables have been identified, 
# the next step is to understand how the response variable changes based on these variables

# built-in PDP support in H2O
h2o.partialPlot(h2o_rf, data = train_obs.h2o, cols = "MonthlyIncome")
## probability of an employee attriting decreases, 
## on average, as their monthly income approaches $5,000 and then remains relatively flat.

# individual conditional expectation (ICE) curves
fit.ranger %>%
  pdp::partial(pred.var = "MonthlyIncome", grid.resolution = 25, ice = TRUE) %>%
  autoplot(rug = TRUE, train = train_obs, alpha = 0.1, center = TRUE)

## by centering we identify the decrease as monthly income approaches $5,000 
## followed by an increase in probability of attriting once an employee's monthly income approaches $20,000.



# 5. LOCAL INTERPRETATION

## This can be applied to any supervised regression or classification model!!

# LIME::LIME
## The lime() function creates an "explainer" object, 
## which is just a list that contains the machine learning model and the feature distributions for the training data

explainer_caret <- lime(train_obs, fit.caret, n_bins = 5)
class(explainer_caret)
summary(explainer_caret)

# LIME::EXPLAIN
explanation_caret <- lime::explain(
  x               = local_obs, 
  explainer       = explainer_caret, 
  n_permutations  = 5000,
  dist_fun        = "gower",
  kernel_width    = .75,
  n_features      = 5, 
  feature_select  = "highest_weights",
  labels          = "Yes"
)

glimpse(explanation_caret)

# VISUALIZING RESULTS
plot_features(explanation_caret)
## Explanation Fit(r-squared): how well that model explains the local region


plot_explanations(explanation_caret)

# TUNING LIME

explanation_caret <- lime::explain(
  x               = local_obs, 
  explainer       = explainer_caret, 
  n_permutations  = 5000,
  dist_fun        = "manhattan",
  kernel_width    = 3,
  n_features      = 5, 
  feature_select  = "lasso_path",
  labels          = "Yes"
)

plot_features(explanation_caret)



# 6. SUPPORTED VS NON-SUPPORT MODELS

## Currently, lime supports supervised models produced in caret, mlr, xgboost, h2o, keras, and MASS::lda.

explainer_h2o_rf  <- lime(train_obs, h2o_rf, n_bins = 5)
explainer_h2o_glm <- lime(train_obs, h2o_glm, n_bins = 5)
explainer_h2o_gbm <- lime(train_obs, h2o_gbm, n_bins = 5)

explanation_rf  <- lime::explain(local_obs, 
                                 explainer_h2o_rf, 
                                 n_features      = 5, 
                                 labels          = "Yes", 
                                 kernel_width    = .1, 
                                 feature_select  = "highest_weights")
explanation_glm <- lime::explain(local_obs, 
                                 explainer_h2o_glm, 
                                 n_features      = 5, 
                                 labels          = "Yes", 
                                 kernel_width    = .1, 
                                 feature_select  = "highest_weights")
explanation_gbm <- lime::explain(local_obs, 
                                 explainer_h2o_gbm, 
                                 n_features      = 5, 
                                 labels          = "Yes", 
                                 kernel_width    = .1, 
                                 feature_select  = "highest_weights")

p1 <- plot_features(explanation_rf,  ncol = 1) + ggtitle("rf")
p2 <- plot_features(explanation_glm, ncol = 1) + ggtitle("glm")
p3 <- plot_features(explanation_gbm, ncol = 1) + ggtitle("gbm")

gridExtra::grid.arrange(p1, p2, p3, nrow = 1)

# Variable importance

# compute permutation-based variable importance
## Adjusting n_sample = -1 just means to use all observations.
vip_glm <- variable_importance(explainer_glm, n_sample = -1, loss_function = loss_root_mean_square) 
vip_rf  <- variable_importance(explainer_rf, n_sample = -1, loss_function = loss_root_mean_square)
vip_gbm <- variable_importance(explainer_gbm, n_sample = -1, loss_function = loss_root_mean_square)

plot(vip_glm, vip_rf, vip_gbm, max_vars = 10)

# Predictor-response relationship
# compute PDP for a given variable --> uses the pdp package
# For example, we saw that the Age variable was one of the most influential variables across all three models.
pdp_glm  <- variable_response(explainer_glm, variable =  "Age", type = "pdp")
pdp_rf   <- variable_response(explainer_rf,  variable =  "Age", type = "pdp")
pdp_gbm  <- variable_response(explainer_gbm, variable =  "Age", type = "pdp")

plot(pdp_glm, pdp_rf, pdp_gbm)

# The below partial dependence plot illustrates that the GBM and random forest models are using the Age signal in a similar non-linear manner; 
# however, the GLM model is not able to capture this same non-linear relationship. So although the GLM model may perform better (re: AUC score), it may be using features in biased or misleading ways.

cat_glm  <- variable_response(explainer_glm, variable = "EnvironmentSatisfaction", type = "factor")
cat_rf  <- variable_response(explainer_rf, variable = "EnvironmentSatisfaction", type = "factor")
cat_gbm  <- variable_response(explainer_gbm, variable = "EnvironmentSatisfaction", type = "factor")
plot(cat_glm, cat_rf, cat_gbm)

# create a single observation
new_cust <- splits$valid[1, ] %>% as.data.frame()

# compute breakdown distances
new_cust_glm <- prediction_breakdown(explainer_glm, observation = new_cust)
new_cust_rf  <- prediction_breakdown(explainer_rf, observation = new_cust)
new_cust_gbm <- prediction_breakdown(explainer_gbm, observation = new_cust)

# class of prediction_breakdown output
class(new_cust_gbm)
## [1] "prediction_breakdown_explainer" "data.frame"

# check out the top 10 influential variables for this observation
new_cust_gbm[1:10, 1:5]

plot(new_cust_gbm)








h2o.shutdown()
