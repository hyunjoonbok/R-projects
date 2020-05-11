# BALANCE DATASET WITH SMOTE
# Use it when our target variable is unbalnced (like 70% yes adn 30% no)
# The ubSMOTE() function from the unbalanced package implements SMOTE
require(unbalanced)
input  <- train_df %>% select(-went_on_backorder)
output <- train_df$went_on_backorder 
train_balanced <- ubSMOTE(input, output, perc.over = 200, perc.under = 200, k = 5)

# perc.over = 200: This is the percent of new instances generated for each rare instance. For example, if there were only 5 rare cases, the 200% percent over will synthetically generate and additional 10 rare cases.
# perc.under = 200: The percentage of majority classes selected for each SMOTE observations. If 10 additional observations were created through the SMOTE process, 20 majority cases would be sampled.
# k = 5: The number of nearest neighbors to select when synthetically generating new observations.

# Recombine the synthetic balanced data
train_df <- bind_cols(as.tibble(train_balanced$X), tibble(went_on_backorder = train_balanced$Y))
train_df