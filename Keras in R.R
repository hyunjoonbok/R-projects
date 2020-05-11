## Different Examples can be found on
## https://tensorflow.rstudio.com/keras/articles/sequential_model.html

require(keras)
## MNIST example :recognizing handwritten digits from the MNIST dataset
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

# convert the 3D arrays into matrices by reshaping width and height into a single dimension
x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))

# convert the grayscale values from integers ranging between 0 to 255
# into floating point values ranging between 0 and 1
x_train <- x_train / 255
x_test <- x_test / 255

# we one-hot encode the Y-vectors into binary class matrices using the Keras "to_categorical()" function
y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)

# create a sequential model (a linear stack of layers)
# The input_shape argument to the first layer specifies the shape of the input data (a length 784 numeric vector representing a grayscale image).
# The final layer outputs a length 10 numeric vector (probabilities for each digit) using a softmax activation function.
model <- keras_model_sequential()

# The model needs to know what input shape it should expect.
# For this reason, the first layer in a sequential model
# (and only the first, because following layers can do automatic shape inference) needs to receive information about its input shape.

# Also, If you pass both batch_size=32 and input_shape=c(6, 8) to a layer,
# it will then expect every batch of inputs to have the batch shape (32, 6, 8).
model %>%
  layer_dense(units = 256,
              activation = 'relu',
              input_shape = c(784)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')
summary(model)

# Compile the model with appropriate loss function, optimizer,and metrics

# For multi-class classification problem
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

#== For Mean Square Error ==#
# model %>% compile(
# optimizer = optimizer_rmsprop(lr = 0.002),
# loss = 'mse'
# )

#== Binary Classification ==#
# model %>% compile(
# optimizer = optimizer_rmsprop(),
# loss = loss_binary_crossentropy,
# metrics = metric_binary_accuracy
# )



# TRAINING AND EVALUATION


# launch TensorBoard (data won't show up until after the first epoch)
tensorboard("logs/run_a")

# Use the fit() function to train the model for 30 epochs using batches of 128 images
history <- model %>% fit(
  x_train,
  y_train,
  epochs = 30,
  batch_size = 128,
  validation_split = 0.2,
  callbacks = callback_tensorboard("logs/run_a")
)
plot(history)





# Evaluate the model's performance on the test data
score <- model %>% evaluate(x_test, y_test)

# Output metrics
cat('Test loss:', score[[1]], '\n')
cat('Test accuracy:', score[[2]], '\n')

# Predictions on new data!!
model %>% predict_classes(x_test)


# Save and load model
save_model_hdf5(model, 'my_model.h5')
model <- load_model_hdf5('my_model.h5')

## SAVING/LOADING ONLY A MODEL'S ARCHITECTURE (not its weights or its training configuration)
json_string <- model_to_json(model)
yaml_string <- model_to_yaml(model)

model <- model_from_json(json_string)
model <- model_from_yaml(yaml_string)