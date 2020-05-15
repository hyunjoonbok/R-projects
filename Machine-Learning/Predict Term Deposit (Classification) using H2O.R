# Predict which customers more likely to enroll in Bank's Term Deposit (Product in bank)
# And explain why

# random Grid Search combined with Stacked Ensembles is a powerful combination
require(tidyverse)
require(readxl)
require(h2o)

# 1.0 Read excel sheets
getwd()
path <- "C:/Users/bokhy/Desktop/bank_term_deposit_marketing_analysis.xlsx"
sheets <- excel_sheets(path)

# 2.0 Omvestogate Data
sheets %>% 
  map(~ read_excel(path = path, sheet = .)) %>%  set_names(sheets)

# 3.0 Perform Vlookup equivalent
data_joined_tbl <- sheets[4:7] %>% 
  map(~ read_excel(path = path, sheet = .)) %>% reduce(left_join, by = "ID")
data_joined_tbl 

data_joined_tbl %>%
  slice(1:10) %>%
  knitr::kable()


## Machine Learning - Labeling

# 4.0 start H2O
h2o.init()
# convert string coluns to factor 
data_joined_tbl <- data_joined_tbl %>% 
  mutate_if(is.character, as.factor)
data_joined_tbl
# Convert training set to H2OFrame
train <- as.h2o(data_joined_tbl)
h2o.describe(train)

# response column
y <- "TERM_DEPOSIT"
x <- setdiff(names(train), c(y, "ID"))

#4.2 AutoML
aml <- h2o.automl(
  y = y,
  x = x,
  training_frame = train,
  max_models = 10,
  seed = 0623
)

lb <- aml@leaderboard
print(lb)
print(lb , n = nrow(lb))


# 4.4 Ensemble Exploration
# get model ids for all models
model_ids <- as.data.frame(aml@leaderboard$model_id)[,1]
model_ids
# 
se <- h2o.getModel(grep("StackedEnsemble_AllModels", model_ids, value = TRUE)[1])
se
metalearner <- h2o.getModel(se@model$metalearner$name)
metalearner

# show how much each base learner is contributin go the ensemble
h2o.varimp(metalearner)
# plot the base learner contribution
h2o.varimp_plot(metalearner)



# 4.5 variable importance
xgb <- h2o.getModel(grep('XGBoost', model_ids, value = TRUE)[1])

h2o.varimp(xgb)
h2o.varimp_plot(xgb)

xgb@parameters


h2o.shutdown()
