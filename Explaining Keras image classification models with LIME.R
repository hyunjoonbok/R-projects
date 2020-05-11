## == Explaining Keras image classification models with LIME == ##

#1. Load packages
require(keras)   # for working with neural nets
require(lime)    # for explaining models
require(magick)  # for preprocessing images
require(ggplot2) # for additional plotting

#2. Load pre-trained model
# The vgg16 model is an image classification model that has been build as part of the ImageNet competition 
# where the goal is to classify pictures into 1000 categories with the highest accuracy
model <- application_vgg16(weights = "imagenet", include_top = TRUE)
model

## AAA  The kitten image classification !! ###

# 2.1 Pass in the training data
# The format for the training data is simply the path to the images, 
# and because the internet runs on kitten pictures we'll use one of these:
img <- image_read('https://www.data-imaginist.com/assets/images/kitten.jpg')
img_path <- file.path(tempdir(), 'kitten.jpg');img_path
image_write(img, img_path)
plot(as.raster(img))

# 2.2 input data and make lime
# As with text models, the explainer will need to know how to prepare the input data for the model. 
# For keras models, this means formatting the image data as tensors
image_prep <- function(x) {
  arrays <- lapply(x, function(path) {
    img <- image_load(path, target_size = c(224,224))
    x <- image_to_array(img)
    x <- array_reshape(x, c(1, dim(x)))
    x <- imagenet_preprocess_input(x)
  })
  do.call(abind::abind, c(arrays, list(along = 1)))
}
explainer <- lime(img_path, model, image_prep)
explainer
# We now have an explainer model for understanding how the vgg16 neural network makes its predictions. 
# Before going further, let's see what the model think:
res <- predict(model, image_prep(img_path))
imagenet_decode_predictions(res)












