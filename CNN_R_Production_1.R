original_dataset_dir <- "E:/R/kaggle_original_data" #경로 저장

base_dir <- "E:/R/cats_and_dogs_small" #새로운 폴더 경로및 이름 저장
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


fnames <- paste0("cat.", 1:1000, ".jpg") 
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


cat("total training cat images:", length(list.files(train_cats_dir)), "\n")
cat("total training dog images:", length(list.files(train_dogs_dir)), "\n")
cat("total validation cat images:", length(list.files(validation_cats_dir)), "\n")
cat("total validation dog images:", length(list.files(validation_dogs_dir)), "\n")
cat("total test cat images:", length(list.files(test_cats_dir)), "\n")
cat("total test dog images:", length(list.files(test_dogs_dir)), "\n")


library(keras)
model<-keras_model_sequential() %>% 
  layer_conv_2d(filters = 32,kernel_size = c(3,3),activation = "relu",
                input_shape = c(150,150,3)) %>% 
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_conv_2d(filters = 64,kernel_size = c(3,3),activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_conv_2d(filters = 128,kernel_size = c(3,3),activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_conv_2d(filters = 128,kernel_size = c(3,3),activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_flatten() %>% 
  layer_dense(units = 512,activation = "relu") %>% 
  layer_dense(units = 1,activation = "sigmoid")
summary(model)  

model %>% compile(
  loss="binary_crossentropy",
  optimizer = optimizer_rmsprop(lr = 1e-4),
  metrics = c("acc")
)


train_datagen<-image_data_generator(rescale = 1/255)
validation_datagen<-image_data_generator(rescale = 1/255)

train_generator<-flow_images_from_directory(
  train_dir, # 표적 경로
  train_datagen, #훈런데이터 생성
  target_size = c(150,150), #모든 사이즈 150*150
  batch_size = 20, #한번에 20개씩
  class_mode = "binary" #이진분류이기 때문에
)
validation_generator <- flow_images_from_directory(
  validation_dir,
  validation_datagen,
  target_size = c(150,150),
  batch_size = 20,
  class_mode = "binary"
)



# batch<-generator_next(train_generator)
# str(batch)

history<-model %>% fit_generator(
  train_generator,
  steps_per_epoch = 100,
  epochs = 30,
  validation_data = validation_generator,
  validation_steps = 50
)

setwd("E:/R")
model %>% save_model_hdf5("cats_and_dogs_small_1.h5")

plot(history)


## 데이터 보강 시작
datagen<-image_data_generator(
  rescale = 1/255, rotation_range = 40,
  width_shift_range = 0.2,
  height_shift_range = 0.2,
  shear_range = 0.2,
  zoom_range = 0.2,
  horizontal_flip = TRUE,
  fill_mode = "nearest"
)



base_dir <- "E:/R/cats_and_dogs_small"
train_dir <- file.path(base_dir, "train") 
validation_dir <- file.path(base_dir, "validation")
test_dir <- file.path(base_dir, "test")


## 데이터  보강 후
datagen<-image_data_generator(
  rescale = 1/255, 
  rotation_range = 40,
  width_shift_range = 0.2,
  height_shift_range = 0.2,
  shear_range = 0.2,
  zoom_range = 0.2,
  horizontal_flip = TRUE
)

#보강한 사진 보기
fnames <- list.files(train_cats_dir, full.names = TRUE)
img_path <- fnames[[3]]  # <-숫자 부분을 수정하면 다른 사진 보기 가능
img <- image_load(img_path, target_size = c(150, 150))
img_array <- image_to_array(img)
img_array <- array_reshape(img_array, c(1, 150, 150, 3))
augmentation_generator <- flow_images_from_data(
  img_array, 
  generator = datagen, 
  batch_size = 1 
)
op <- par(mfrow = c(2, 2), pty = "s", mar = c(1, 0, 1, 0))
for (i in 1:4) {
  batch <- generator_next(augmentation_generator)
  plot(as.raster(batch[1,,,]))
}
par(op)

#모델 드롭아웃 추가해서 다시 설계

model <- keras_model_sequential() %>% 
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu",
                input_shape = c(150, 150, 3)) %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_flatten() %>% 
  layer_dropout(rate = 0.5) %>%   # 드롭아웃 추가 
  layer_dense(units = 512, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")  


model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_rmsprop(lr = 1e-4),
  metrics = c("acc")
)

#다시 데이터 전처리

datagen <- image_data_generator(
  rescale = 1/255,
  rotation_range = 40,
  width_shift_range = 0.2,
  height_shift_range = 0.2,
  shear_range = 0.2,
  zoom_range = 0.2,
  horizontal_flip = TRUE
)

test_datagen <- image_data_generator(rescale = 1/255)

train_generator <- flow_images_from_directory(
  train_dir,
  datagen,
  target_size = c(150, 150),
  batch_size = 30,
  class_mode = "binary"
)

validation_generator <- flow_images_from_directory(
  validation_dir,
  test_datagen,
  target_size = c(150, 150),
  batch_size = 32,
  class_mode = "binary"
)

# 모델 다시 학습 *시간 소모
history <- model %>% fit_generator(
  train_generator,
  steps_per_epoch = 100,
  epochs = 100,
  validation_data = validation_generator,
  validation_steps = 50
)

getwd()
setwd("E:/R")
model <- load_model_hdf5("cats_and_dogs_small_2.h5") #결과 저장 