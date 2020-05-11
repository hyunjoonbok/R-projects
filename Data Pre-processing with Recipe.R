# Data Pre-processing with Recipe
require(AmesHousing)
ames <- make_ames()

# 1) simple Linear Model
# create a design matrix for 2 predictor variable
# log the outcome variable

modl <- lm(log(Sale_Price) ~ Alley + Lot_Area, data = ames, subset = Year_Sold > 2000)

# 2) train and apply to data
require(recipes)
require(tidyverse)
require(rsample)

set.seed(0623)
data_split <- initial_split(ames, prop = 0.75)

ames_train <- training(data_split)
ames_test <- testing(data_split)

# Create an inital reciple with the same operations 
rec <- recipe(Sale_Price ~ Alley + Lot_Area, data = ames_train %>% head()) %>% 
  step_log(Sale_Price) %>% 
  step_dummy(Alley) # one-hot encode categorical variable

# "retain = TRUE" keeps the processed training set, which is created during the estimation phase
rec_trained <- prep(rec, training = ames_train, retain = TRUE); rec_trained

# Get the processed training set
# design_mat <- juice(rec_trained); design_mat

# Apply to any other data sets
rec_test <- bake(rec_trained, newdata = ames_test); rec_test



#(((# IF Needed to add more pre-precessing?
standardized <- rec_trained %>% 
  step_center(all_numeric()) %>% 
  step_scale(all_numeric())
## only estimate new parts:
standardized <- prep(standardized, verbose = FALSE)
##)))##



# 3) More complex recipe
ames_rec <- recipe(Sale_Price ~ Bldg_Type + Neighborhood + Year_Built + 
                     Gr_Liv_Area + Full_Bath + Year_Sold + Lot_Area +
                     Central_Air + Longitude + Latitude, 
                   data = ames_train) %>% 
  step_log(Sale_Price, base = 10) %>% 
  
  # Mitigate extreme skewness in some predictors
  step_YeoJohnson(Lot_Area, Gr_Liv_Area) %>% 
  
  # some Neighborhood are rare, so pool them out into "other"
  step_other(Neighborhood, threshold = 0.05) %>% 
  
  step_dummy(all_nominal()) %>% 
  
  #relationship between Central Air and Year Built
  step_interact(~ starts_with("Central_Air"):Year_Built) %>% 
  
  # geocode values have highly non-linear relationships with price
  step_bs(Longitude, Latitude, options = list(df = 5)) %>% 
  
  prep(traning = ames_train, retain = TRUE)

# 4) Use with Caret
# All of these operations should be done inside resampling to get proper error estimates
require(caret)
xgb_trcontrol = trainControl(
  method = "cv",
  number = 5,  
  allowParallel = TRUE,
  verboseIter = FALSE,
  returnData = FALSE
)
# model
lm_mod <- train(ames_rec, data = ames, method = "lm", trControl = xgb_trcontrol)
lm_mod$recipe
lm_mod$results
lm_mod$resample

## ===================================================================================================== ##

# Data Pre-processing with Recipe [more robust model]

require(AmesHousing)
require(broom)
require(kknn)
require(recipes)
require(rsample)
require(tidyverse)
require(yardstick)


# [1] 
set.seed(0623)
ames <- make_ames()
data_split <- initial_split(ames,prop = 0.75 ,strata = "Sale_Price")

ames_train <- training(data_split)
ames_test <- testing(data_split)


ames_rec <- recipe(Sale_Price ~ Bldg_Type + Neighborhood + Year_Built + 
                     Gr_Liv_Area + Full_Bath + Year_Sold + Lot_Area +
                     Central_Air + Longitude + Latitude, 
                   data = ames_train) %>% 
  step_log(Sale_Price, base = 10) %>% 
  
  # Mitigate extreme skewness in some predictors
  step_YeoJohnson(Lot_Area, Gr_Liv_Area) %>% 
  
  # some Neighborhood are rare, so pool them out into "other"
  step_other(Neighborhood, threshold = 0.05) %>% 
  
  step_dummy(all_nominal()) %>% 
  
  step_interact(~ starts_with("Central_Air"):Year_Built) %>% 
  # geocode values have highly non-linear relationships with price
  step_bs(Longitude, Latitude, options = list(df = 5))









