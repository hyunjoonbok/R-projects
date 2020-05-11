#### ML Model Interpretability with DALEX #### 


## DALEX: Descriptive mAchine Learning EXplanations ##

# Ex) Predictive modeling for apartment prices

# 1) Load packages
require(DALEX)
require(ceterisParibus)
require(randomForest)
require(breakDown)
require(live)
require(auditor)

data("apartments")
data("apartmentsTest")
head(apartments)

# 2) two models
set.seed(0623)
apartments_lm_model <- lm(m2.price ~ construction.year + surface + floor + no.rooms + district, data = apartments)
apartments_lm_model
apartments_rf_model <- randomForest(m2.price ~ construction.year + surface + floor + no.rooms + district, data = apartments)
apartments_rf_model

# [other explainers]
require(e1071)
apartments_svm_model <- svm(m2.price ~ construction.year + surface + floor + no.rooms + district, data = apartments)

# 3) how good are these model?
# Look at RMSE
predicted_mi2_lm <- predict(apartments_lm_model, apartmentsTest)
sqrt(mean((predicted_mi2_lm - apartmentsTest$m2.price)^2))

predicted_mi2_rf <- predict(apartments_rf_model, apartmentsTest)
sqrt(mean((predicted_mi2_rf - apartmentsTest$m2.price)^2))

# 4) Wrap Model
explainer_lm <- explain(model = apartments_lm_model, 
                        data = apartmentsTest[,2:6],
                        y = apartmentsTest$m2.price,
                        label = "Linear model")

explainer_rf <- explain(model = apartments_rf_model, 
                        data = apartmentsTest[,2:6],
                        y = apartmentsTest$m2.price,
                        label = "randomforest model")

explainer_svm <- explain(apartments_svm_model,
                         data = apartmentsTest[,2:6],
                         y = apartmentsTest$m2.price,
                         label = "SVM model")

# 5) Create and Plot explainer 
# [1] continuous variable
sv_rf <- single_variable(explainer_rf, 
                         variable = "construction.year",
                         type = "pdp")
plot(sv_rf)

sv_lm <- single_variable(explainer_lm,
                         variable = "construction.year",
                         type = "pdp")
plot(sv_rf, sv_lm)

sv_svm <- single_variable(explainer_svm,
                          variable = "construction.year",
                          type = "pdp")

plot(sv_rf, sv_lm, sv_svm) # I would use SVM!


# [2] categorical variable
svd_lm <- single_variable(explainer_lm,
                          variable = "district",
                          type = "factor")
plot(svd_lm)

svd_rf <- single_variable(explainer_rf,
                          variable = "district",
                          type = "factor")

plot(svd_rf, svd_lm) #randomforest fails to capture smoonthly

svd_svm <- single_variable(explainer_svm,
                          variable = "district",
                          type = "factor")
plot(svd_rf, svd_lm, svd_svm)


# 6) HOw accurate are these model??

resids_lm <- model_performance(explainer_lm);resids_lm
resids_rf  <- model_performance(explainer_rf);resids_rf
resids_svm <- model_performance(explainer_svm);resids_svm

plot(resids_lm, resids_rf, resids_svm, geom = "boxplot") # SVM has the lowest residual!
plot(resids_lm, resids_rf, resids_svm)


require(auditor) # model verification, validation, error analysis
audit_lm <- audit(explainer_lm)
plotResidual(audit_lm, variable = "construction.year")
audit_rf <- audit(explainer_rf)
plotResidual(audit_rf, variable = "construction.year")
audit_svm <- audit(explainer_svm)
plotResidual(audit_svm, variable = "construction.year")

plotResidual(audit_lm,audit_rf,audit_svm, variable = "construction.year")


# 7) Which variables are important?
vi_lm <- variable_importance(explainer_lm, loss_root_mean_square);vi_lm
plot(vi_lm) # if we drop "district", the loss will increase from 286 to 1044
# baseline: when you suffle all variables again

vi_rf <- variable_importance(explainer_rf,loss_root_mean_square);vi_rf
vi_svm <- variable_importance(explainer_svm,loss_root_mean_square);vi_svm
plot(vi_lm, vi_rf, vi_svm) ## From this we know we shuold use SVM
                              

## ============================================================================================== ##
# Explain a single prediction!

# Example Case
# x variables: surface: 130, no.rooms: 5, floor: 3, construction.year: 1978
# Response variable: m2.price: 4200 EUR

# SO how to explain this?

# Ceteris Paribus Plot #
# How the prediction would change, if only the single variables are affected?

# 1.

explainer_lm <- explain(model = apartments_lm_model, 
                        data = apartmentsTest[,2:6],
                        y = apartmentsTest$m2.price,
                        label = "Linear model")

explainer_rf <- explain(model = apartments_rf_model, 
                        data = apartmentsTest[,2:6],
                        y = apartmentsTest$m2.price,
                        label = "randomforest model")

explainer_svm <- explain(apartments_svm_model,
                         data = apartmentsTest[,2:6],
                         y = apartmentsTest$m2.price,
                         label = "SVM model")

# 2. predicted value
new_apartment <- apartmentsTest[1,] # here we chose sample data

require(ceterisParibus)
wi_lm <- ceteris_paribus(explainer = explainer_lm,
                         observation = new_apartment)
plot(wi_lm,
     split = "variables",
     color = "variables",
     quantiles = FALSE)

wi_rf <- ceteris_paribus(explainer = explainer_rf,
                         observation = new_apartment)
wi_svm <- ceteris_paribus(explainer = explainer_svm,
                         observation = new_apartment)

# compare models in a single plot
plot(wi_lm, wi_rf, wi_svm,
     split = "variables",
     color = "variables", # color = "model" is another option
     quantiles = FALSE)

plot(wi_lm, wi_rf, wi_svm,
     split = "variables",
     color = "models", # color = "model" is another option
     quantiles = FALSE)


cr_rf <- local_fit(explainer = explainer_rf,
                   observation = new_apartment,
                   select_points = 0.002)
cr_svm <- local_fit(explainer = explainer_svm,
                   observation = new_apartment,
                   select_points = 0.002)
plot(cr_rf) # not good..
plot(cr_svm) # Much nicer, less residuals, but the result could be different for predictions



# 3. How much each variables affect this prediction?

br_rf <- prediction_breakdown(explainer_rf,
                              observation = new_apartment)
plot(br_rf)

br_svm <- prediction_breakdown(explainer_svm,
                               observation = new_apartment)
plot(br_rf,br_svm)
# for rf: 712 Euro higher than intercept
# for svm: 1087 Euro higher than intercept
# "district" seems like the most important variables in both