# Shiny WebApp
library(shiny)
library(slam)
library(tidyverse)
library(Matrix)
library(data.table)
library(DT)

# Loading clensed data
data_final <- readRDS(file = "C:/Users/bokhy/Documents/R-projects/Title_Recommender_with_market_basket_analysis/data_final.rds")

# Shiny App Deploy
# 1. Create 'past_orders_matrix"  
# Needed for Shiny server.R file for all the calculations

past_orders_matrix <- 
  data_final %>%
  # Select only needed variables
  select(machine_uuid, Game) %>% 
  # Unique transactions
  distinct() %>% 
  # Add a column of 1s
  mutate(value = 1) %>%
  # Spread into user-item format
  spread(Game, value, fill = 0) %>%
  select(-machine_uuid) %>% 
  # Convert to matrix
  as.matrix() %>% 
  # Convert to class "dgCMatrix"
  as("dgCMatrix")

past_orders_matrix
# Save file
saveRDS(past_orders_matrix, 
        file = "past_orders_matrix.rds")

# 2. Creating a unique items list --> Needed for Shiny ui.R file to make the products list available for selection
item_list <- data_final %>% 
  select(Game) %>% 
  unique()

# Save file
saveRDS(item_list, 
        file = "item_list.rds")

# 3. Improved Collaborative Filtering
# I am fitting the best performing model found above (the user-based CF) on the test titles
customer_play <- c("Joe & Mac: Caveman Ninja (Arcade)",
                   "Asteroids¢ç (Arcade)",
                   "Centipede¢ç (Arcade)",
                   "Fire Truck (Arcade)",
                   "64th Street - A Detective Story")

# 4. Put new_order in a user_item matrix format
new_order <- item_list %>%
  # Add a 'value' column with 1's for customer order items
  mutate(value = as.numeric(Game %in% customer_play)) %>%
  # Spread into sparse matrix format
  spread(key = Game, value = value) %>%
  # Change to a matrix
  as.matrix() %>% 
  # Convert to class "dgCMatrix"
  as("dgCMatrix")
new_order

# 5. Add the new_order to the past_orders_matrix as its first entry
# binding 2 matrices
all_orders_dgc <- t(rbind(new_order, past_orders_matrix))

# 6. Now, I need to set a number of parameters required by the Improved CF to work.
# Set range of items to calculate predictions for - here I select them all
items_to_predict <- 1:nrow(all_orders_dgc)
# Set current user to 1, which corresponds to new_order
users <- c(1)
# Set prediction indices
prediction_indices <- as.matrix(expand.grid(items_to_predict, users = users))
###########
# Load the algorithm implementations and similarity calculations.
# https://github.com/smartcat-labs/collaboratory/tree/master/R
source("C:/Users/bokhy/Documents/R-projects/Title_Recommender_with_market_basket_analysis/cf_algorithm.R")
source("C:/Users/bokhy/Documents/R-projects/Title_Recommender_with_market_basket_analysis/similarity_measures.R")

# fit the item-based CF model with the Improved CF and check the runtime.
recomm <- predict_cf(all_orders_dgc, prediction_indices,
                     "ubcf", FALSE, cal_cos, 3, FALSE, 4000, 2000)
recomm[,users] %>% 
  as.data.frame()

# Predict the new user
user_results <- sort(recomm[, users], decreasing = TRUE)[1:15]

user_predicted_ids <- names(user_results)
user_results <- data.table(Title = user_predicted_ids, 
                           Predicted_rating = user_results)
  
user_results %>% 
  filter(!Predicted_rating == 0) %>% 
  select(Title) %>% 
  datatable(class = "nowrap hover row-border", options = list(dom = 'tp', scrollX = TRUE, autoWidth = TRUE))