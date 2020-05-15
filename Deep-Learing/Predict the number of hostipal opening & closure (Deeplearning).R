# Predict the number of hostipal opening/closure


# Read files
pack <- c("googledrive","dplyr","tidyr","purrr","data.table","dtplyr")
invisible(lapply(pack, library, character.only = TRUE))

dir.create("./data")

drive_auth()
ids <- drive_ls(as_id("1-GTQhq5HyxE5ePfjV0OwDIFiouHe2l5l"))
for(i in 1: nrow(ids)){
  drive_download(as_id(ids$id[i]), paste0("./data/",ids$name[i]))
}

unz <- list.files("./data", pattern = "zip")
for(i in 1:length(unz)){
  unzip(zipfile = paste0("./data/",unz[i]), exdir = "./data")
}
file.remove(paste0("./data/",unz))

train <- fread("./data/train.csv")
test <- fread("./data/test.csv")
submission <- fread("./data/submission.csv")

## =============================================================== ##

# Load Pacakges

require(purrr)
require(Boruta)
require(lubridate)
require(xgboost)
require(recipes)
require(caret)
require(ggplot2)    
require(tidyverse)
require(skimr)
require(mice)
require(missForest)

## =============================================================== ##

training$days_since_2016 <- as.integer(as.POSIXct(strptime(20160101, "%Y%m%d",tz = "UTC")) - as.POSIXct(strptime(training$openDate, "%Y%m%d",tz = "UTC"))) 
training$days_since_2017 <- as.integer(as.POSIXct(strptime(20170101, "%Y%m%d",tz = "UTC")) - as.POSIXct(strptime(training$openDate, "%Y%m%d",tz = "UTC"))) 
training$year_since_2016 <- year(as.POSIXct(strptime(20160101, "%Y%m%d",tz = "UTC"))) - year(as.POSIXct(strptime(training$openDate, "%Y%m%d",tz = "UTC"))) 
training$year_since_2017 <- year(as.POSIXct(strptime(20170101, "%Y%m%d",tz = "UTC"))) - year(as.POSIXct(strptime(training$openDate, "%Y%m%d",tz = "UTC")))

training$year <- year(as.POSIXct(strptime(training$openDate, "%Y%m%d",tz = "UTC")))
training$month <- month(as.POSIXct(strptime(training$openDate, "%Y%m%d",tz = "UTC")))
training$day <- day(as.POSIXct(strptime(training$openDate, "%Y%m%d",tz = "UTC")))
training$week <- week(as.POSIXct(strptime(training$openDate, "%Y%m%d",tz = "UTC")))
training$quarter <- quarter(as.POSIXct(strptime(training$openDate, "%Y%m%d",tz = "UTC")))



# Add 
training$revenue_add <- training$revenue1 + training$revenue2
training$salescost_add <- training$salescost1 + training$salescost2
training$sga_add <- training$sga1 + training$sga2
training$salary_add <- training$salary1 + training$salary2
training$noi_add <- training$noi1 + training$noi2
training$noe_add <- training$noe1 + training$noe2
training$interest_add <- training$interest1 + training$interest2
training$ctax_add <- training$ctax1 + training$ctax2
training$profit_add <- training$profit1 + training$profit2
training$liquidAsset_add <- training$liquidAsset1 + training$liquidAsset2
#training$quickAsset_add <- training$quickAsset1 + training$quickAsset2
training$receivableS_add <- training$receivableS1 + training$receivableS2
#training$inventoryAsset_add <- training$inventoryAsset1 + training$inventoryAsset2
training$nonCAsset_add <- training$nonCAsset1 + training$nonCAsset2
training$tanAsset_add <- training$tanAsset1 + training$tanAsset2
training$OnonCAsset_add <- training$OnonCAsset1 + training$OnonCAsset2
training$debt_add <- training$debt1 + training$debt2
training$liquidLiabilities_add <- training$liquidLiabilities1 + training$liquidLiabilities2
training$shortLoan_add <- training$shortLoan1 + training$shortLoan2
training$NCLiabilities_add <- training$NCLiabilities1 + training$NCLiabilities2
training$longLoan_add <- training$longLoan1 + training$longLoan2
training$netAsset_add <- training$netAsset1 + training$netAsset2
training$surplus_add <- training$surplus1 + training$surplus2



training <- training[,-c(1,5,24,48)]


# Scale large numerical variables
training[,c(6:51,64:85)] <- scale(training[,c(6:51,64:85)])


training$OC <- as.factor(training$OC)
training$employee1 <- as.integer(training$employee1)
training$employee2 <- as.integer(training$employee2)
training$year <- as.factor(training$year)
training$month <- as.factor(training$month)
training$day <- as.factor(training$day)
training$week <- as.factor(training$week)
training$quarter <- as.factor(training$quarter)

## =============================================================== ##

sort(sapply(training, function(x) { sum(is.na(x)) }), decreasing=TRUE)


imp.train_raw <- mice(training, m = 5, maxit = 1, method = 'cart', seed = 0623)
imp.train_raw

train_complete <- complete(imp.train_raw)

sort(sapply(train_complete, function(x) { sum(is.na(x)) }), decreasing=TRUE)
table(is.na(train_complete))

glimpse(train_complete)
## =============================================================== ##



# Pre-processing #
#### ============================================================  ####


# Feature Selection

set.seed(0623)

boruta_new <- Boruta(OC ~., data = training, doTrace = 2)

plot(boruta_new, las = 2, cex.axis = 0.6)


# Tentative Fix
bor = TentativeRoughFix(boruta_new)
print(bor)

getSelectedAttributes(bor)

getNonRejectedFormula(boruta_new)




# 1. Change NA (일시불) value in "installment" into 1
training$installments <- ifelse((is.na(training$installments == TRUE)),1,training$installments)
training$store_id <- as.factor(training$store_id)
# Check NA values
training %>% na_if("NA") %>% map_df(~sum(is.na(.)))
head(training)
# Examing numeric columns
training %>% select_if(is.numeric) %>% skim()

training %>% na_if("NA")


#### ============================================================  ####


require(h2o)

h2o.init(nthreads = -1, max_mem_size = "16g")

df.h2o_train <- as.h2o(training)
df.h2o_test <- as.h2o(testing)

splits <- h2o.splitFrame(df.h2o_train, ratios = 0.7, seed = 0623)
names(splits) <- c("train","valid")



hyper_params <- list(
  activation = c("Rectifier","Tanh","Maxout","RectifierWithDropout","TanhWithDropout","MaxoutWithDropout"),
  hidden = list(c(20,20),c(50,50),c(30,30,30),c(25,25,25,25),c(200,200),c(512,512),c(128,128,128),c(512,512,512),c(128,128,128,128)),
  input_dropout_ratio = c(0,0.01,0.05,0.1,0.2,0.4,0.5),
  l1 = seq(0,1e-4,1e-6), # add regularization
  l2 = seq(0,1e-4,1e-6),
  rate = c(0.01,0.02,0.04,0.05),
  rate_annealing = c(2e-6,2e-7),
  momentum_start = c(0.1,0.2,0.5),
  momentum_stable = c(0.1,0.4,0.5,0.99)
)
search_criteria = list(seed = 0623, strategy = "RandomDiscrete", max_runtime_secs = 360, max_models = 100)



y <- "OC"
x <- setdiff(names(training), y);x


automl_model <- h2o.automl(
  seed = 0623,
  x = x, 
  y = y, 
  training_frame = splits$train,
  validation_frame = splits$valid,
  stopping_metric = "AUTO",
  stopping_rounds = 5,
  max_runtime_secs = 3600,
  max_models = 10,
  nfolds = 5) 

# Get the best model
leader <- automl_model@leader
leader
# EVALUATE PERFORMANCE
perf <- h2o.performance(leader, newdata = df.h2o_test)
perf
# Predict 
pred10 <- h2o.predict(leader, newdata = df.h2o_test)
as.data.frame(pred10)





a <- h2o.grid(
  algorithm = "deeplearning",
  grid_id = "amount",
  training_frame = splits$train,
  validation_frame = splits$valid,
  x = x,
  y = y,
  nfolds = 5,
  epochs = 10000,
  stopping_metric = "AUTO",
  stopping_tolerance = 1e-2,
  stopping_rounds = 5,
  max_w2 = 10,
  score_training_samples = 30000,
  score_validation_samples = 30000,
  score_duty_cycle = 0.025,
  hyper_params = hyper_params,
  search_criteria = search_criteria
)
grid <- h2o.getGrid("amount",sort_by = "logloss",decreasing = FALSE)

best_model <- h2o.getModel(grid@model_ids[[1]])

perf <- h2o.performance(model = best_model, newdata = df.h2o_test)

pred <- h2o.predict(best_model, newdata = df.h2o_test)
