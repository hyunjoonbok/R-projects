# 오늘 수업코드를 진행해주시려면 케글에서 받아온 데이터를 미리 저장해주셔야 합니다.
# https://www.kaggle.com/c/dogs-vs-cats/data 강아지 고양이 사진 25000장 
# 데이러를 받아오셨다면 데이터가 담긴 폴더가 어디에 저장되었는지 잘 확인해주셔야 합니다.
# 이전 강의를 보셨다면 아래 library(keras) 로 이동해 주세요 ~

original_dataset_dir <- "E:/R/kaggle_original_data" #케글에서 받아온 데이터 폴더, 경로 저장

base_dir <- "E:/R/cats_and_dogs_small" #새로운 폴더 경로 및 이름 저장
dir.create(base_dir) #폴더 생성

train_dir <- file.path(base_dir, "train")
dir.create(train_dir) #학습데이터 폴더 생성
validation_dir <- file.path(base_dir, "validation")
dir.create(validation_dir)#검증데이터 폴더 생성
test_dir <- file.path(base_dir, "test")
dir.create(test_dir)#테스트 데이터 폴더 생성


train_cats_dir <- file.path(train_dir, "cats")
dir.create(train_cats_dir) #학습고양이 폴더 생성
train_dogs_dir <- file.path(train_dir, "dogs")
dir.create(train_dogs_dir) #학습 강아지 폴더생성
validation_cats_dir <- file.path(validation_dir, "cats")
dir.create(validation_cats_dir) #검증 고양이 폴더생성
validation_dogs_dir <- file.path(validation_dir, "dogs")
dir.create(validation_dogs_dir) #검증 강아지 폴더 생성
test_cats_dir <- file.path(test_dir, "cats")
dir.create(test_cats_dir) #테스트 고양이폴더 생성
test_dogs_dir <- file.path(test_dir, "dogs")
dir.create(test_dogs_dir)


fnames <- paste0("cat.", 1:1000, ".jpg") #paste0는 공백을 지워주며 입력값들을 붙여주는 역할
file.copy(file.path(original_dataset_dir, fnames), 
          file.path(train_cats_dir)) #파일 카피 
fnames <- paste0("cat.", 1001:1500, ".jpg")
file.copy(file.path(original_dataset_dir, fnames), 
          file.path(validation_cats_dir))
fnames <- paste0("cat.", 1501:2000, ".jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(test_cats_dir))
fnames <- paste0("dog.", 1:1000, ".jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(train_dogs_dir))
fnames <- paste0("dog.", 1001:1500, ".jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(validation_dogs_dir)) 
fnames <- paste0("dog.", 1501:2000, ".jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(test_dogs_dir))

#이미지가 정확히 복사되었는지 확인.
cat("total training cat images:", length(list.files(train_cats_dir)), "\n")
cat("total training dog images:", length(list.files(train_dogs_dir)), "\n")
cat("total validation cat images:", length(list.files(validation_cats_dir)), "\n")
cat("total validation dog images:", length(list.files(validation_dogs_dir)), "\n")
cat("total test cat images:", length(list.files(test_cats_dir)), "\n")
cat("total test dog images:", length(list.files(test_dogs_dir)), "\n")



##############################################################################################
# 오늘의 강의 시작 

library(keras)
conv_base<- application_vgg16(
  weights = "imagenet",
  include_top = FALSE,
  input_shape = c(150,150,3)
  )

conv_base

base_dir <- "E:/R/cats_and_dogs_small"
train_dir <- file.path(base_dir, "train") 
validation_dir <- file.path(base_dir, "validation")
test_dir <- file.path(base_dir, "test")

datagen<-image_data_generator(rescale = 1/255)
batch_size <- 20

extract_features <- function(directory, sample_count) {
  
  features <- array(0, dim = c(sample_count, 4, 4, 512))  
  labels <- array(0, dim = c(sample_count))
  
  generator <- flow_images_from_directory(
    directory = directory,
    generator = datagen,
    target_size = c(150, 150),
    batch_size = batch_size,
    class_mode = "binary"
  )
  
  i <- 0
  while(TRUE) {
    batch <- generator_next(generator)
    inputs_batch <- batch[[1]]
    labels_batch <- batch[[2]]
    features_batch <- conv_base %>% predict(inputs_batch)
    
    index_range <- ((i * batch_size)+1):((i + 1) * batch_size)
    features[index_range,,,] <- features_batch
    labels[index_range] <- labels_batch
    
    i <- i + 1
    if (i * batch_size >= sample_count)
      break
  }
  
  list(
    features = features, 
    labels = labels
  )
}

train <- extract_features(train_dir, 2000)
validation <- extract_features(validation_dir,1000)
test<-extract_features(test_dir,1000)

reshape_features <- function(features){
  array_reshape(features, dim = c(nrow(features), 4*4*512))
}

train$features<-reshape_features(train$features)
validation$features<-reshape_features(validation$features)
test$features <-reshape_features(test$features)

model<- keras_model_sequential() %>% 
  layer_dense(units = 256,activation = "relu",
              input_shape = 4*4*512) %>%
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 1, activation = "sigmoid")
model %>% compile(
  optimizer = optimizer_rmsprop(lr = 2e-5),
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

history<-model %>% fit(
  train$features, train$labels,
  epochs =30,
  batch_size = 20,
  validation_data = list(validation$features,validation$labels)
)



####################################################################

library(keras)

base_dir <- "E:/R/cats_and_dogs_small"
train_dir <- file.path(base_dir, "train") 
validation_dir <- file.path(base_dir, "validation")
test_dir <- file.path(base_dir, "test")


conv_base<- application_vgg16(
  weights = "imagenet",
  include_top = FALSE,
  input_shape = c(150,150,3)
)

model<- keras_model_sequential() %>% 
  conv_base %>% 
  layer_flatten() %>% 
  layer_dense(units = 256, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")

model


cat("지금 conv_base에 학습된 가중치들의 수는 : ",
    length(model$trainable_weights), "\n")
freeze_weights(conv_base)
cat("동결 후 conv_base의 가중치 수는 :",
    length(model$trainable_weights),"\n")


####################################################################


train_datagen = image_data_generator(
  rescale = 1/255,
  rotation_range = 40,
  width_shift_range = 0.2,
  height_shift_range = 0.2,
  shear_range = 0.2,
  zoom_range = 0.2,
  horizontal_flip = TRUE,
  fill_mode = "nearest"
)
test_datagen <- image_data_generator(rescale = 1/255)
train_generator <- flow_images_from_directory(
  train_dir,
  train_datagen,
  target_size = c(150, 150),
  batch_size = 20,
  class_mode = "binary"
)
validation_generator <- flow_images_from_directory(
  validation_dir,
  test_datagen,
  target_size = c(150, 150),
  batch_size = 20,
  class_mode = "binary"
)


model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_rmsprop(lr = 2e-5),
  metrics = c("accuracy")
)
history <- model %>% fit_generator(
  train_generator,
  steps_per_epoch = 100,
  epochs = 30,
  validation_data = validation_generator,
  validation_steps = 50
)

save_model_hdf5(model, "cats_and_dogs_small_3.h5")

plot(history)


########################################################
# 거의 다왔습니다 ! 조금만 힘내주세요!

conv_base    

unfreeze_weights(conv_base, from = "block3_conv1")

model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_rmsprop(lr = 1e-5),
  metrics = c("accuracy")
)
history <- model %>% fit_generator(
  train_generator,
  steps_per_epoch = 100,
  epochs = 100,
  validation_data = validation_generator,
  validation_steps = 50
)

getwd()
save_model_hdf5(model, "cats_and_dogs_small_4.h5")
plot(history)


test_generator <- flow_images_from_directory(
  test_dir,
  test_datagen,
  target_size = c(150, 150),
  batch_size = 20,
  class_mode = "binary"
)

model %>% evaluate_generator(test_generator, steps = 50)


#제 수업을 시청해 주셔서 너무 감사드립니다 :) 