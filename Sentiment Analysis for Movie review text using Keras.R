#### Sentiment Analysis for Movie review text using Keras #### 

setwd("E:/R")

imdb_dir <- "C:/Users/bokhy/Documents/R-code-storage/aclImdb/aclImdb"
train_dir <- file.path(imdb_dir, "train")

labels <- c() # movie reviews
texts <- c() # evalutation from customers


for (label_type in c("neg", "pos")) { 
  label <- switch(label_type, neg = 0, pos = 1) # negative review to 0 and positive reviews to 1
  dir_name <- file.path(train_dir, label_type)
  for (fname in list.files(dir_name, pattern = glob2rx("*.txt"), 
                           full.names = TRUE)) {
    texts <- c(texts, readChar(fname, file.info(fname)$size))
    labels <- c(labels, label)
  }
}


# 1. Totenization of sentence data

library(keras)

# We assume that we lack the amount of training data! Only use 200 data
training_samples <- 200      
maxlen <- 100 # only allows maximum 100 words                
validation_samples <- 10000 
max_words <- 10000 # frequently used 10,000 words        

tokenizer <- text_tokenizer(num_words = max_words) %>% 
  fit_text_tokenizer(texts)
sequences <- texts_to_sequences(tokenizer, texts)
head(sequences) # makes sentences to numeric index

word_index = tokenizer$word_index
cat("We found", length(word_index), "total tokens.\n")

data <- pad_sequences(sequences, maxlen = maxlen) # change numeric index to 2nd dimension tensor
labels <- as.array(labels)

indices <- sample(1:nrow(data)) # we need to shuffule the data (because it's now divided into Pos/Neg)
training_indices <- indices[1:training_samples]
validation_indices <- indices[(training_samples + 1): 
                                (training_samples + validation_samples)]

x_train <- data[training_indices,]
y_train <- labels[training_indices]
x_val <- data[validation_indices,]
y_val <- labels[validation_indices]

# Refernece
# https://nlp.stanford.edu/projects/glove/
glove_dir = 'E:/R/glove.6B'

lines <- readLines(file.path(glove_dir, "glove.6B.100d.txt")) # 100-dimension embedding
head(lines)
embeddings_index <- new.env(hash = TRUE, parent = emptyenv())

for (i in 1:length(lines)) {
  line <- lines[[i]]
  values <- strsplit(line, " ")[[1]]
  word <- values[[1]]
  embeddings_index[[word]] <- as.double(values[-1])
}
View(embeddings_index)


embedding_dim <- 100
embedding_matrix <- array(0, c(max_words, embedding_dim))

for (word in names(word_index)) {
  index <- word_index[[word]]
  if (index < max_words) {
    embedding_vector <- embeddings_index[[word]]
    if (!is.null(embedding_vector))
      embedding_matrix[index+1,] <- embedding_vector
  }
}


# 2. Modeling

model <- keras_model_sequential() %>% 
  layer_embedding(input_dim = max_words, output_dim = embedding_dim, 
                  input_length = maxlen) %>% # input_dim = how many words you would consider important
  # dimension of embedding, # input_length = how many words we use per each sentence
  layer_flatten() %>% 
  layer_dense(units = 32, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid") # sigmoid = binary classification

summary(model)


get_layer(model, index = 1) %>% # put the Glove matrix to the very first layer in our model
  set_weights(list(embedding_matrix)) %>% 
  freeze_weights() # in order to not lose the result because it trains again

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)

history <- model %>% fit(
  x_train, y_train,
  epochs = 20,
  batch_size = 32,
  validation_data = list(x_val, y_val)
)

getwd()
save_model_weights_hdf5(model, "pre_trained_glove_model.h5")


# Model that's not pre-trained 
model <- keras_model_sequential() %>% 
  layer_embedding(input_dim = max_words, output_dim = embedding_dim, 
                  input_length = maxlen) %>% 
  layer_flatten() %>% 
  layer_dense(units = 32, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")
model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)
history <- model %>% fit(
  x_train, y_train,
  epochs = 20,
  batch_size = 32,
  validation_data = list(x_val, y_val)
)

# Final Evaluation
test_dir <- file.path(imdb_dir, "test")
labels <- c()
texts <- c()
for (label_type in c("neg", "pos")) {
  label <- switch(label_type, neg = 0, pos = 1)
  dir_name <- file.path(test_dir, label_type)
  for (fname in list.files(dir_name, pattern = glob2rx("*.txt"), 
                           full.names = TRUE)) {
    texts <- c(texts, readChar(fname, file.info(fname)$size))
    labels <- c(labels, label)
  }
}

sequences <- texts_to_sequences(tokenizer, texts)
x_test <- pad_sequences(sequences, maxlen = maxlen)
y_test <- as.array(labels)

model %>% 
  load_model_weights_hdf5("pre_trained_glove_model.h5") %>% 
  evaluate(x_test, y_test, verbose = 0)

