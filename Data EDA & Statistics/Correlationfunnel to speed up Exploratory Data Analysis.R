# Correlation Funnel on EDA #
# Sample code from "https://github.com/business-science/correlationfunnel"

library(correlationfunnel)
library(dplyr)

# Correlationfunnel package provides a succinct workflow and interactive visualization tools for understanding 
# which features have relationships to target (response).

# We will see that using 3 functions, we can quickly:

# 1. Transform the data into a binary format with binarize()

# 2. Perform correlation analysis using correlate()

# 3. Visualize the highest correlation features using plot_correlation_funnel()

# Result: Rather than spend hours looking at individual plots of capaign features and comparing them to which customers opted in to the TERM DEPOSIT product, in seconds we can discover which groups of customers have enrolled, drastically speeding up EDA.
data("marketing_campaign_tbl")


# Step 1: Convert to Binary Format
# Converting the continuous and categorical data into binary (0/1) format. 
# Only select any non-predictive features

# Numeric Features: Are binned into ranges
# Categorical Features: Are binned by one-hot encoding
marketing_campaign_binarized_tbl <- marketing_campaign_tbl %>%
  select(-ID) %>%
  binarize(n_bins = 4, thresh_infreq = 0.01)

marketing_campaign_binarized_tbl %>% glimpse()

# Step 2: Perform Correlation Analysis
# between the response (target = TERM_DEPOSIT_yes) and the rest of the features
marketing_campaign_correlated_tbl <- marketing_campaign_binarized_tbl %>%
  correlate(target = TERM_DEPOSIT__yes)

marketing_campaign_correlated_tbl

# Step 3: Visualize
marketing_campaign_correlated_tbl %>%
  plot_correlation_funnel(interactive = FALSE) # TRUE to get an interactive plot

# Step 4 : Examining the Results
## The most important features are towards the top !!## 
marketing_campaign_correlated_tbl %>%
  filter(feature %in% c("DURATION", "POUTCOME", "PDAYS", 
                        "PREVIOUS", "CONTACT", "HOUSING")) %>%
  plot_correlation_funnel(interactive = FALSE, limits = c(-0.4, 0.4))

# When the DURATION, the amount of time a prospect is engaged in marketing campaign material, is 319 seconds or longer.

# When POUTCOME, whether or not a prospect has previously enrolled in a product, is “success”.

# When CONTACT, the medium used to contact the person, is “cellular”

# When HOUSING, whether or not the contact has a HOME LOAN is “no”


# ("Data Explorer" Package - Automates Exploration and Data Treatment)