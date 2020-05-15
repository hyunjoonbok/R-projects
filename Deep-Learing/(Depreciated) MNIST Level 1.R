## MNIST Level 1 ## 

### 1. Read dataset
require(tensorflow)
datasets <- tf$contrib$learn$datasets
mnist <- datasets$mnist$read_data_sets("MNIST-data", one_hot = TRUE)

### 2. Implementing the Regression
x <- tf$placeholder(tf$float32, shape(NULL, 784L))

W <- tf$Variable(tf$zeros(shape(784L, 10L)))
b <- tf$Variable(tf$zeros(shape(10L)))

y <- tf$nn$softmax(tf$matmul(x, W) + b)

### 3. Training the Model
y_ <- tf$placeholder(tf$float32, shape(NULL, 10L))

# loss(cross-entropy) function
cross_entropy <- tf$reduce_mean(-tf$reduce_sum(y_ * tf$log(y), reduction_indices=1L))

# minimize loss
optimizer <- tf$train$GradientDescentOptimizer(0.5)
train_step <- optimizer$minimize(cross_entropy)

# create an operation to initialize the variables we created
init <- tf$global_variables_initializer()

sess <- tf$Session()
sess$run(init)

# train 1000 times
for (i in 1:1000) {
  batches <- mnist$train$next_batch(100L)
  batch_xs <- batches[[1]]
  batch_ys <- batches[[2]]
  sess$run(train_step,
           feed_dict = dict(x = batch_xs, y_ = batch_ys))
}


### 3. Evaluating Our Model
correct_prediction <- tf$equal(tf$argmax(y, 1L), tf$argmax(y_, 1L))

accuracy <- tf$reduce_mean(tf$cast(correct_prediction, tf$float32))

sess$run(accuracy, feed_dict=dict(x = mnist$test$images, y_ = mnist$test$labels))


