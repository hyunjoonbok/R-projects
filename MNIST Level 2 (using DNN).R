## MNIST Level 2 ## 

### 1. Read dataset
require(tensorflow)
input_dataset <- tf$examples$tutorials$mnist$input_data
mnist <- input_dataset$read_data_sets("MNIST-data", one_hot = TRUE)

# START INTERACTIVESESSION
sess <- tf$InteractiveSession()

### 2. Softmax Regression
# In this section we will build a softmax regression model with a single linear layer
# In the next section, we will extend this to the case of softmax regression with a multilayer convolutional network.

# PLACEHOLDERS
x <- tf$placeholder(tf$float32, shape(NULL, 784L))
y_ <- tf$placeholder(tf$float32, shape(NULL, 10L))

# VARIABLES
W <- tf$Variable(tf$zeros(shape(784L, 10L)))
b <- tf$Variable(tf$zeros(shape(10L)))

# Before Variables can be used within a session, they must be initialized using that session
sess <- tf$Session()
sess$run(tf$global_variables_initializer())


### 3. TRAIN & PREDICTION & LOSS FUNCTION
# our model
y <- tf$nn$softmax(tf$matmul(x,W) + b)
# our loss function
cross_entropy <- tf$reduce_mean(-tf$reduce_sum(y_ * tf$log(y), reduction_indices=1L))

optimizer <- tf$train$GradientDescentOptimizer(0.5)
train_step <- optimizer$minimize(cross_entropy)

for (i in 1:1000) {
  batches <- mnist$train$next_batch(100L)
  batch_xs <- batches[[1]]
  batch_ys <- batches[[2]]
  sess$run(train_step,
           feed_dict = dict(x = batch_xs, y_ = batch_ys))
}


### 4. EVALUATE THE MODEL
correct_prediction <- tf$equal(tf$argmax(y, 1L), tf$argmax(y_, 1L))

accuracy <- tf$reduce_mean(tf$cast(correct_prediction, tf$float32))

accuracy$eval(feed_dict=dict(x = mnist$test$images, y_ = mnist$test$labels), session = sess)


### Increase accuracy using "Multilayer ConvNet"

# WEIGHT INITIALIZATION

# we're going to need to create a lot of weights and biases
# nitialize them with a slightly positive initial bias to avoid "dead neurons" using function
weight_variable <- function(shape) {
  initial <- tf$truncated_normal(shape, stddev=0.1)
  tf$Variable(initial)
}

bias_variable <- function(shape) {
  initial <- tf$constant(0.1, shape=shape)
  tf$Variable(initial)
}

# CONVOLUTION AND POOLING
conv2d <- function(x, W) {
  tf$nn$conv2d(x, W, strides=c(1L, 1L, 1L, 1L), padding='SAME')
}

max_pool_2x2 <- function(x) {
  tf$nn$max_pool(
    x, 
    ksize=c(1L, 2L, 2L, 1L),
    strides=c(1L, 2L, 2L, 1L), 
    padding='SAME')
}

# FIRST CONVOLUTIONAL LAYER
W_conv1 <- weight_variable(shape(5L, 5L, 1L, 32L))
b_conv1 <- bias_variable(shape(32L))

x_image <- tf$reshape(x, shape(-1L, 28L, 28L, 1L))

h_conv1 <- tf$nn$relu(conv2d(x_image, W_conv1) + b_conv1)
h_pool1 <- max_pool_2x2(h_conv1)

# SECOND CONVOLUTIONAL LAYER
W_conv2 <- weight_variable(shape = shape(5L, 5L, 32L, 64L))
b_conv2 <- bias_variable(shape = shape(64L))

h_conv2 <- tf$nn$relu(conv2d(h_pool1, W_conv2) + b_conv2)
h_pool2 <- max_pool_2x2(h_conv2)

# DENSELY CONNECTED LAYER
W_fc1 <- weight_variable(shape(7L * 7L * 64L, 1024L))
b_fc1 <- bias_variable(shape(1024L))

h_pool2_flat <- tf$reshape(h_pool2, shape(-1L, 7L * 7L * 64L))
h_fc1 <- tf$nn$relu(tf$matmul(h_pool2_flat, W_fc1) + b_fc1)

# DROPOUT to reduce overfitting
# We create a placeholder for the probability that a neuron's output is kept during dropout. 
# This allows us to turn dropout on during training, and turn it off during testing
keep_prob <- tf$placeholder(tf$float32)
h_fc1_drop <- tf$nn$dropout(h_fc1, keep_prob)


# READOUT LAYER (final softmax layer)
W_fc2 <- weight_variable(shape(1024L, 10L))
b_fc2 <- bias_variable(shape(10L))

y_conv <- tf$nn$softmax(tf$matmul(h_fc1_drop, W_fc2) + b_fc2)

# TRAIN AND EVALUATE THE MODEL

# Train
cross_entropy <- tf$reduce_mean(-tf$reduce_sum(y_ * tf$log(y_conv), reduction_indices=1L))
train_step <- tf$train$AdamOptimizer(1e-4)$minimize(cross_entropy)

# Evaluate
correct_prediction <- tf$equal(tf$argmax(y_conv, 1L), tf$argmax(y_, 1L))
accuracy <- tf$reduce_mean(tf$cast(correct_prediction, tf$float32))
sess$run(tf$global_variables_initializer())

for (i in 1:1000) {
  batch <- mnist$train$next_batch(50L)
  if (i %% 100 == 0) {
    train_accuracy <- accuracy$eval(feed_dict = dict(
      x = batch[[1]], y_ = batch[[2]], keep_prob = 1.0))
    cat(sprintf("step %d, training accuracy %g\n", i, train_accuracy))
  }
  train_step$run(feed_dict = dict(
    x = batch[[1]], y_ = batch[[2]], keep_prob = 0.5), session = sess)
}

test_accuracy <- accuracy$eval(feed_dict = dict(
  x = mnist$test$images, y_ = mnist$test$labels, keep_prob = 1.0))
cat(sprintf("test accuracy %g", test_accuracy))


