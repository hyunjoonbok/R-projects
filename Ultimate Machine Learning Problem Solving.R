# Explaining Machine Learning

# sucstiptions
#1. use random forest to model behavior
# sample data could be (gender, citizen, dependate, tenure, etc)

# 4 different method to explain what's going on 
# Global PDP ICE
# Local LIME Shapley (each feature contribute to churn)

# Partial Dependence Plot (PDP): We take few different observations (100 ), we calucate predictino holding except just one feature
# averageing all the restuls (e.x. chrun rate for Female/Male)

# ICE (Individual COnditional Expectaition) Plot : It does not avarerage! It centers them around some value. It shows the trend line very well
# (i.e how does age affect my dataset)

# LIME : Train an interpretable model on weighted Decision Tree 
# (i.e. see which feature is contributing a lot for a specific customer)

# Shapley Value Plot : Each feacture is player in game. Let's say Prediction is Payout
# Coalition: we group featrues together that are similar 
# Gain: The actual prediction minus the average for all features
# Shapley Value - Average contriution to the prediction in differnt coalitions

# LIME and Shapley has Pro and Cons
# LIME: Not accruate but fast (dashboard). Assumes Linear Behavior
# Shapley: Accurate -> gaurangtess a fair distrubtion of all features. It utilize game-theory, it takes time though

# SHAP (Shapley Additive ExPlanation) - Use to explain Shapley (slow)

# Explainable Machine Learning
# IML package! DALEX Package!


## ========================================= ##
## Pro Tips ##
# 1. Use Correctional Funnel Package: Help investigate each feature. Focus on top features
# 2. Use PDP and ICE to make story about specific feature (i.e. Monthyl charge, tenure)
# that would be related to Churn
# 3. Talk about one specific customer!
## ========================================= ##




require(tidyverse)
require(tidyquant)
# Train-test split
require(rsample)
# Pre-processing
require(reciples)
# machine learning
require(h2o)
# Explaing
require(iml)
require(DALEX)
# EDA
require(correlationfunnel)
require(DataExplorer)

require(tictoc)

require(gt)
# 1. Read Data

# 2. EDA ====

data %>% plot_missing()

data %>% 
  filter(!is.na()) %>% # missing numbers 
    binarize() %>% 
    correlate()%>% #Total Cahrages__1231.123 number)  
    plot_correlation_funnel()
    
# Fix missing value


# 3.0 Machine Learning
data %>% glimpse()

# 3.1 Pre-provessing
rsample_splits <- inital_split(data, prop=0.8)
a <- reciple(CHurn ~. , data= data) %>% 
  step_mutate(totalChanrge) %>% 
  setp_rm(custoemrID) %>% #remove
  step_string2factor(all_nominal()) %>% 
  prep()

train_set <- bake(a, training(rsample_splits))
test_set <-bake(a, testing(rsample_splits))  # same

#3.2 Random Forest
h2o.init()

h2o_rf <- h2o.randomForest(
  x = x,
  y =y,
  
)
h2o.predict()
h2o.auc()

# 4.0 IML Explanation
# 4.1 IML set-up

features_tbl <- train_set %>% select(-Churn) 

response_vec <- train_set %>% pull(Churn) %>% as.numeric() - 1

predict_h2o <- function(model, newdata) {
  results_tbl <- h2o.predict(model, newdata = as.h2o(newdata)) %>% as.tibble()
  results_tbl %>% pull(Yes)
}

predidct_h2o(h2o_rf, newdata = test_data)


# 4.2 PRedicto object

preidctor_rf <- Predictor$new(
  model = h2o_rf,
  data = features_tbl,
  y = response_vec,
  predict.fun = predict_h2o,
  class = "classification"
)
preidctor_rf

# Global Model Explanation

funnel_churn_ggplot

# 5.0 PDP 

# single feature - "contact Type_

pdp_contract <- FeatureEffect$new(
  predictor = preidctor_rf,
  feature = "Contract",
  method = "pdp",
  grid.size = 20
)

pdp_contract %>% 
  plot() + expand_limits(y = 0) # now see their is a different where my RF results out 

# 2-way interactions "contact type & monthyl charges"
# Tip = Increase grid size !

tic()
pdp_monthly_charges_by_contract <- FeatureEffect$new(
  predictor = preidctor_rf,
  feature = c("Contact", "MonthlyChargnes"),
  method = "pdp",
  grid.size = 10
)
toc()

pdp_monthly_charges_by_contract %>% 
  plot(rug = TRUE) + 
  expand_limits(y = 0) +
  theme_tq() +
  scale_color_tq()


# 6.0 ICE 

ice_contact_monthlys <- FeatureEffect$new(
  predictor = preidctor_rf,
  feature = c("Contact", "MonthlyChargnes"),
  method = "ice",
  grid.size = 10,
  center.at = 0 # THis centers every at 0. So it gives more clear visual
)
# IT shows the distributino of customers!

ice_contact_monthlys %>% 
  plot() +
  geom_smooth(color = palette_light(){"green"}, size = 2) +
  expand_limits( y= 0)


# SHAP
shapley_rf <- Shapley$new(
  predictor = predictor_rf,
  x.interest = test_set %>% slice(2) %>% select(-Churn), # for customer # 2
  sample.size = 200
)

shapey_rf %>%  plot() +theme_tq()

# Pro Tips!

breakdown_h2o_leader <- break_down(
  x= explainer_h2o_leader,
  test_set %>% slice(2) %>% select(-Churn),
  interactions = FALSE
)

plot(breakdown_h2o_leader, max_features = 4)

