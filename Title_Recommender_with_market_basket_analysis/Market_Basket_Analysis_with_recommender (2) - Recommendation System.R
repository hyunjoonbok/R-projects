# We use
# Collaborative Filtering
# When a collaborative filtering is used, the recommendation system looks at either clients purchase history or clients product rating history. These are compiled into a user-item ratings matrix, with items (e.g. products) in the columns and users (e.g. customers or orders) in the rows. A measure of similarity is then calculated using methods like Cosine, Pearsons or Jaccard similarity to identify a hierarchy of probability to purchase.

# Two methodologies are made use of:
  
  # User-based collaborative filtering (UBCF) focuses on similarities between users market basket composition to find other users with similar baskets. Similarity measures are then calculated and used to shape the suggestions list.
  # Item-based collaboartive filtering (IBCF) employes a similar appsoach but focuses instead on similarities between items that are frequently purchased together.

# Loading clensed data
data_final <- readRDS(file = "C:/Users/bokhy/Documents/R-projects/Title_Recommender_with_market_basket_analysis/data_final.rds")

# Setting seed for reproducibility
# set.seed(seed = 0623) 

# 1. Modelling

##(1). Create the Rating Matrix
# We need binary rating matrix consisting of 0’s and 1’s, where 1’s indicate if the product was purchased. 
# This does not require normalisation.
ratings_matrix  <- data_final %>%
  select(machine_uuid, Game) %>% 
  # Remove duplicates
  distinct() %>% 
  # Add a column of 1s
  mutate(value = 1) %>%
  # Spread into user-item format
  spread(Game, value, fill = 0) %>%
  select(-machine_uuid) %>%
  # Convert to matrix
  as.matrix() %>%
  # Convert to recommenderlab class 'binaryRatingsMatrix'
  as("binaryRatingMatrix")

ratings_matrix

#2. Evaluation Scheme and Model Validation
scheme <- ratings_matrix  %>% 
  evaluationScheme(method = "cross",
                   k      = 5, # 5-fold cv
                   train  = 0.85, #85/15 train/valid split on our case
                   given  = -1) #given = -1 means that for the test users ‘all but 1’, randomly selected item is withheld for evaluation.
scheme

#3. Set up List of Algorithms
# One of recommenderlab main features is 
# the ability to estimate multiple algorithms in one go
algorithms <- list(
  "association rules" = list(name  = "AR", 
                             param = list(supp = 0.01, conf = 0.01)),
  "random items"      = list(name  = "RANDOM",  param = NULL),
  "popular items"     = list(name  = "POPULAR", param = NULL),
  "item-based CF"     = list(name  = "IBCF", param = list(k = 5)),
  "user-based CF"     = list(name  = "UBCF", 
                             param = list(method = "Cosine", nn = 500)),
  "SVD approximation" = list(name = "SVD", param = list(k = 50))
)
# 4. Estimate the Models
# pass scheme and algoritms to the evaluate() function, 
# select type = topNList to evaluate a Top N List of product recommendations 
# and specify how many recommendations to calculate with the parameter 
# n = c(1, 3, 5, 10, 15, 20).
results <- recommenderlab::evaluate(scheme, 
                                    algorithms, 
                                    type  = "topNList", 
                                    n     = c(1, 3, 5, 10, 15, 20)
)
results # Stored as list


# 5. Ready for Visualization
avg_conf_matr <- function(results) {
  # Pull into a list all confusion matrix information for one model 
  tmp <- results %>%
    getConfusionMatrix()  %>%  
    as.list() 
  # Calculate average value of 5 cross-validation rounds
  as.data.frame(Reduce("+",tmp) / length(tmp)) %>% 
    # Add a column to mark the number of recommendations calculated
    mutate(n = c(1, 3, 5, 10, 15, 20)) %>%
    # Select only columns needed and sorting out order 
    select('n', 'precision', 'recall', 'TPR', 'FPR') 
}

# map() function from the purrr package to get all results in a tidy format, ready for charting
results_tbl <- results %>%
  map(avg_conf_matr) %>% 
  # Turning into an unnested tibble
  enframe() %>%
  # Unnesting to have all variables on same level
  unnest()

# 6. ROC curve

# ROC curve, which plots the true positive rate (TPR) against the false positive rate (FPR)
results_tbl

results_tbl %>%
  ggplot(aes(FPR, TPR, 
             colour = fct_reorder2(as.factor(name), 
                                   FPR, TPR))) +
  geom_line() +
  geom_label(aes(label = n))  +
  labs(title = "ROC curves", colour = "Model") +
  theme_grey(base_size = 14)

# The "User-based collaborative filtering" model is the clear winner for this case
# as it achieves the highest TPR for any given level of FPR
# This means that the model is producing the highest number of relevant recommendations (true positives) 
# for the same level of non-relevant recommendations (false positives).


# Precision-Recall curve
results_tbl %>%
  ggplot(aes(recall, precision, 
             colour = fct_reorder2(as.factor(name),  
                                   precision, recall))) +
  geom_line() +
  geom_label(aes(label = n))  +
  labs(title = "Precision-Recall curves", colour = "Model") +
  theme_grey(base_size = 14)
# Another common way to compare classification models performance is with Precision Vs Recall curves. 
# Precision shows how sensitive models are to False Positives (i.e. recommending an item not very likely to be purchased) 
# whereas Recall (which is just another name for the TPR) looks at how sensitive models are to False Negatives 
# (i.e. do not suggest an item which is highly likely to be purchased).

# Normally, we care about accurately predicting which items are more likely to be purchased because this would have a positive impact on sales and revenue. In other words, 
# we want to maximise Recall (or minimise FNs) for the same level of Precision

# Here, The plot confirms that User-based Collaborative Filter (UBCF) is the best model because it has higher Recall for any given level of Precision. 
# This means that IBCF minimises FNs (i.e. not suggesting an item highly likely to be purchased) for all level of FPs.


# 7. Predictions For a New User
#(1) Let's create a test title
customer_play <- c("Best Bout Boxing",
                   "Burnin' Rubber¢â (Arcade)",
                   "Breakout¢ç (2600)",
                   "Fix-It Felix, Jr.",
                   "64th Street - A Detective Story")
#(2) Put this order in a format that recommenderlab accept.
new_order_rat_matrx <- data_final %>% 
  # Select item descriptions from retail dataset
  select(Game) %>% 
  unique() %>% 
  # Add a 'value' column with 1's for customer order items
  mutate(value = as.numeric(Game %in% customer_play)) %>% 
  # Spread into sparse matrix format
  spread(key = Game, value = value) %>% 
  # Change to a matrix
  as.matrix() %>% 
  # Convert to recommenderlab class 'binaryRatingsMatrix'
  as("binaryRatingMatrix")

new_order_rat_matrx  
#(3) Now, I can create a Recommender. 
# I use "getData" to retrieve training data and set method = “UBCF” to select the best performing model (“User-based collaborative filtering”).
recomm <- Recommender(getData(scheme, 'train'), 
                      method = "UBCF",  
                      param = list(k = 5))
recomm

#(4) Finally, I can pass the Recommender and the made-up order to the predict function to create a top 10 recommendation list for the new customer.
pred <- predict(recomm, 
                newdata = new_order_rat_matrx, 
                n       = 10)
as(pred, 'list')
