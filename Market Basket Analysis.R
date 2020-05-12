# Market Basket Analysis

# 0. Set working directory
getwd()
setwd("~/R-code-storage/market_basket_analysis/data")

# 1. Load Libraries ====

require(readr)
require(tidyverse)
require(tidyquant)
require(plotly)
require(devtools)
require(recommenderlab)
require(arules)
require(arulesViz)

# 2.Load Datasets ====

aisles <- read_csv("aisles.csv", col_names = TRUE)
departments <- read_csv("departments.csv", col_names = TRUE)
order_products_train <- read_csv("order_products__train.csv", col_names = TRUE)
orders <- read_csv("orders.csv", col_names = TRUE)
products <- read_csv("products.csv", col_names = TRUE)

glimpse(aisles)
glimpse(departments)
glimpse(order_products_train)
glimpse(orders)
glimpse(products)

# 3. EDA ====

combined_data <- order_products_train %>% 
  left_join(orders) %>% 
  left_join(products) %>% 
  left_join(aisles) %>% 
  left_join(departments) %>% 
  select(eval_set, user_id, # set the column order it displayed in the dataset
         contains("order"), contains("product"), 
         contains("aisle"), contains("department"), everything())
combined_data  
# 4. Business Question to ask ====

## 4.1 Which products are purchased most often? ====

purchase_frequency <- combined_data %>% 
  count(product_name,product_id, aisle, department) %>% 
  arrange(desc(n)) %>% 
  mutate(
    percentage = n/sum(n),
    cumulative_product_count = cumsum(percentage),
    popularity = ifelse(cumulative_product_count <= 0.5, "Yes", "No")
  ) %>% 
  rowid_to_column(var = 'rank') %>% 
  mutate(label_text = str_glue("Rank: {rank} 
                               Product: {product_name}")
  )

glimpse(purchase_frequency)

plot_purchase_frequency <- purchase_frequency %>% 
  slice(1:5000) %>% 
  ggplot(aes(x = rank, y = n)) + 
  geom_point(aes(size = n, color = popularity, text = label_text)) +
  theme_tq() +
  scale_color_tq() +
  theme(legend.position = "vertical",
        legend.direction = "right") +
  labs(title = "Purchase Frequency",
       subtitle = "Top Items takes most of the sales")
plot_purchase_frequency

ggplotly(plot_purchase_frequency)


## 4.2 Do cetain customers purchase more? ====

user_item_frequency <- combined_data %>% 
  count(user_id) %>%
  arrange(desc(n)) %>% 
  mutate(
    percentage = n/sum(n),
    cumulative_product_count = cumsum(percentage),
    popularity = ifelse(cumulative_product_count <= 0.5, "Yes", "No")
  ) %>% 
  rowid_to_column(var = 'rank')


user_item_frequency


plot_user_item_frequency <- user_item_frequency %>% 
  slice(1:5000) %>% 
  ggplot(aes(x = rank, y = n)) + 
  geom_point(aes(size = n, color = popularity)) +
  theme_tq_dark() +
  scale_color_tq() +
  theme(legend.position = "vertical",
        legend.direction = "right") +
  labs(title = "User Frequency",
       subtitle = "We do have some customers making large purchases")  

ggplotly(plot_user_item_frequency)


# 5 Combine user-item and transaction-item data ====

## 5.1 Top products ====
top_product <- purchase_frequency %>% 
  filter(popularity == "Yes") %>% 
  pull(product_name)
top_product

top_product_basket <- combined_data %>% 
  filter(product_name %in% top_product)
top_product_basket

## 5.2 Basket segmentation ====
top_user <- user_item_frequency %>% 
  filter(rank < 2700) %>% 
  pull(user_id)
top_user

market_basket <- combined_data %>% 
  filter(user_id %in% top_user)
market_basket


# 6. Market Basket Analysis ====

# 6.1 "Binary Ratings Matrix" ====
## - did basket contatin an item (Yes / No encolded as 1 / 0)

user_item <- market_basket %>% 
  select(user_id, product_name) %>% 
  mutate(value = -1) %>% 
  spread(product_name, value, fill = 0)

user_item_rlab <- user_item %>% 
  select(-user_id) %>% 
  as.matrix() %>% 
  as("binaryRatingMatrix")
user_item_rlab


# 6.2 Relationship with arules packages ====

user_item_rlab@data

user_item_rlab@data %>% summary()

user_item_rlab@data %>%  glimpse()


# 6.3 Create an evaluate skema ====
recommenderRegistry$get_entries()

eval_recipe <- user_item_rlab %>% 
  evaluationScheme(method = "cross-validation", k = 5, given = -1)

eval_recipe

# 6.4 Assiociation Rules (arules) ====

algorithm_list <- list(
  "assocation_rule_1" = list(name = "AR", param = list(supp=0.01, conf=0.01)),
  "assocation_rule_2" = list(name = "AR", param = list(supp=0.01, conf=0.1)),
  "assocation_rule_3" = list(name = "AR", param = list(supp=0.01, conf=0.5)),
  "assocation_rule_4" = list(name = "AR", param = list(supp=0.1, conf=0.5))
  )

# This takes a long time
results_rlab_arules <- eval_recipe %>% 
  recommenderlab::evaluate(
    method = algorithm_list,
    type = "topNList",
    n = 1:10
  )

plot(results_rlab_arules, annotate = TRUE)
# "assocation_rule_2" works the best!

# 6.5 All algorithms ====

algorithm_list <- list(
  "random_items" = list(name = "RANDOM", param = NULL),
  "popular_items" = list(name = "POPULAR", param = NULL),
  "User_based_cf" = list(name = "UBCF", param = list(method = "Cosine", nn=500)),
  "Item_based_cf" = list(name = "RAIBCF", param = list(k=5)),
  "arules2" = list(name = "AR", param = list(supp = 0.01, cof = 0.1))
)

results_rlab <- eval_recipe %>% 
  recommenderlab::evaluate(
    method = algorithm_list,
    type = "topNList",
    n = 1:10
  )


# 7.0 Models ====

# 7.1 Association rules ====
association_rules_model <- recommenderlab::Recommender(
  data = user_item_rlab,
  method = "AR",
  param = list(supp= 0.01, conf  = 0.01)
)
association_rules_model

# 7.2 User-based CF
user_based_model <- recommenderlab::Recommender(
  data = user_item_rlab,
  method = "UBCF",
  param = list(method="Cosine", nn  = 500)
)
user_based_model 

# 8.0 Relationships ====

rules <- association_rules_model@model$rule_base
rules

# 8.1 Interactive ====

inspectDT(rules)

plotly_arules(rules, method = "scatterplot", marker = list(size = ~lift),
              colors = c("blue","green"))


# 9.0 Prediction ====

# 9.1 New test data

new_basket <- c("Banana", "Organic Whole Milk")

new_basket_rlab <- tibble(items = user_item_rlab@data %>% colnames()) %>% 
  mutate(value = as.numeric(items %in% new_basket)) %>% 
  spread(key = items, value = value) %>% 
  as.matrix() %>% 
  as("binaryRatingMatrix")


new_basket_rlab

# 9.2 predict using association rules

predict_ar <- predict(association_rules_model, newdata = new_basket, n = 3)

tibble(items = predict_ar@itemLabels) %>% 
  slice(predict_ar@items[[1]])

# 9.3 predict using UBCF

predict_ubcf <- predict(user_based_model, newdata = new_basket, n = 3)

tibble(items = predict_ubcf@itemLabels) %>% 
  slice(predict_ubcf@items[[1]])



