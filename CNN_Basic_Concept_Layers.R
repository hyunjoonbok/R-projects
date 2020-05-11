library(keras)

model<-keras_model_sequential() %>% 
  layer_conv_2d(filters = 32,kernel_size = c(3,3),activation = "relu",
                input_shape = c(28,28,1), padding =) %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64,kernel_size = c(3,3),activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>% 
  layer_conv_2d(filters = 64,kernel_size = c(3,3),activation = "relu")
model

model<-model %>%
  layer_flatten() %>%  # 3차원 -> 1차원
  layer_dense(units = 64, activation = "relu") %>% 
  layer_dense(units = 10,activation = "softmax")
model
mnist<-dataset_mnist()
c(c(train_images,train_labels),c(test_images,test_labels)) %<-% mnist
train_images<-array_reshape(train_images,c(60000,28,28,1))
train_images<-train_images/255

test_images<-array_reshape(test_images,c(10000,28,28,1))
test_images<-test_images/255

train_labels<-to_categorical(train_labels)
test_labels<-to_categorical(test_labels)

model %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

model %>% fit(
  train_images,train_labels,
  epochs=5,batch_size=64
)

results<-model %>% evaluate(test_images,test_labels)
results


# 이렇게는 모델을 설계하시면 안됩니다!!
model_no_max_pool<-keras_model_sequential() %>% 
  layer_conv_2d(filters = 32,kernel_size = c(3,3),activation = "relu",
                input_shape = c(28,28,1)) %>% 
  layer_conv_2d(filters = 64,kernel_size = c(3,3),activation = "relu") %>% 
  layer_conv_2d(filters = 64,kernel_size = c(3,3),activation = "relu")
model_no_max_pool
