require(tensorflow)
require(tfestimators)
# install_tensorflow(version = "gpu")


##=== Example ===##
# Simple linear regression model with the mtcars dataset to demonstrate the use of estimators

# 1. Input Function
# Estimators can accept data from arbitrary data sources through an 'input function'
# The tfestimators package provides the input_fn() helper function for generating input functions from common R data structures, e.g. R matrices and data frames.

# return an input_fn for a given subset of data
mtcars_input_fn <- function(data, num_epochs = 1) {
  input_fn(data, 
           features = c("disp", "cyl"), 
           response = "mpg",
           batch_size = 32,
           num_epochs = num_epochs)
}


# 2. FEATURE COLUMNS
# Feature columns are mappings of raw input data to the data 
# that we'll actually feed into our training, evaluation, and prediction steps
cols <- feature_columns(
  column_numeric("disp"),
  column_numeric("cyl")
)


# 3. Estimator (#1 and #2 should be done before going forward)
# Next, we create the estimator by calling the linear_regressor() function and passing it a set of feature columns:
model <- linear_regressor(feature_columns = cols)


# 4. Training 
indices <- sample(1:nrow(mtcars), size = 0.80 * nrow(mtcars))
train <- mtcars[indices, ]
test  <- mtcars[-indices, ]

model %>% train(mtcars_input_fn(train, num_epochs = 10))

# 5. Evaluation
model %>% evaluate(mtcars_input_fn(test))


# 6. Prediction
obs <- mtcars[1:3, ]
model %>% predict(mtcars_input_fn(obs))


# 7. Save model in directory and load model
saved_model_dir <- model_dir(model)
loaded_model <- linear_regressor(feature_columns = cols,
                                 model_dir = saved_model_dir);loaded_model




## =========================================================================================##

## Example of DNN Linear Combined canned Estimator

# 1. Input Function
input <- input_fn(mtcars, 
                  features = c("drat", "mpg", "am"),
                  response = "vs",
                  batch_size = 128,
                  epochs = 3)


# 2. Feature columns 
linear_feature_columns <- feature_columns(column_numeric("mpg"))
dnn_feature_columns <- feature_columns(column_numeric("drat"))



# 3.generate classifier
classifier <-
  dnn_linear_combined_classifier(
    linear_feature_columns = linear_feature_columns,
    dnn_feature_columns = dnn_feature_columns,
    dnn_hidden_units = c(3, 3),
    dnn_optimizer = "Adagrad"
  )


# 4. Train and Prediction
classifier %>%
  train(input_fn = input, steps = 2)

predictions <- predict(
  classifier,
  input_fn = input,
  predict_keys = "probabilities")

# or logistic
#predictions <- predict(
#  classifier,
#  input_fn = input,
#  predict_keys = "logistic")

# 5. Evaluate
classifier %>% evaluate(input_fn(testdata))


# 6. Tensorboard (optional)
estimator(
  model_fn = classifier,
  model_dir = "/tmp/test"
) %>% train(input_fn = input, steps = 100L)

tensorboard(log_dir = "/tmp/test")

## =========================================================================================##
# We can access the TensorFlow Dataset API via the tfdatasets package, 
# which enables us to create scalable input pipelines that can be used with tfestimators.

# 1.Data preparation
require(dplyr)
set.seed(123)
train_idx <- sample(nrow(iris), nrow(iris) * 2/3)

iris_train <- iris[train_idx,]
iris_validation <- iris[-train_idx,]
iris_sample <- iris_train %>%
  head(10)

write.csv(iris_train, "iris_train.csv", row.names = FALSE)
write.csv(iris_validation, "iris_validation.csv", row.names = FALSE)
write.csv(iris_sample, "iris_sample.csv", row.names = FALSE)

# 2. Estimator Construction
require(tfestimators)
response <- "Species"
features <- setdiff(names(iris), response)
feature_columns <- feature_columns(
  column_numeric(features)
)

classifier <- dnn_classifier(
  feature_columns = feature_columns,
  hidden_units = c(16, 32, 16),
  n_classes = 3,
  label_vocabulary = c("setosa", "virginica", "versicolor")
)

# 3. Input Function
iris_input_fn <- function(data) {
  input_fn(data, features = features, response = response)
}
require(tfdatasets)
## Study this pacakage!!
iris_spec <- csv_record_spec("iris_sample.csv")
iris_train <- text_line_dataset(
  "iris_train.csv", record_spec = iris_spec) %>%
  dataset_batch(10) %>% 
  dataset_repeat(10)
iris_validation <- text_line_dataset(
  "iris_validation.csv", record_spec = iris_spec) %>%
  dataset_batch(10) %>%
  dataset_repeat(1)

# 4. Training and Evaluation
history <- train(classifier, input_fn = iris_input_fn(iris_train))
plot(history)
predictions <- predict(classifier, input_fn = iris_input_fn(iris_validation));predictions
evaluation <- evaluate(classifier, input_fn = iris_input_fn(iris_validation));evaluation

