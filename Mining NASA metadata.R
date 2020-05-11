##======= Mining NaSa Metadata======##

## download the JSON file
require(jsonlite)
metadata <- fromJSON("https://data.nasa.gov/data.json")
names(metadata$dataset)

#1. Wrangling and tidying the data

require(dplyr)

nasa_title = data_frame(id = metadata$dataset$`_id`$`$oid`, title=metadata$dataset$title)
nasa_desc = data_frame(id = metadata$dataset$`_id`$`$oid`, description=metadata$dataset$description)
nasa_desc %>% select(description) %>% sample_n(4)

## We need to use unnest() from tidyr to select out keyword, because they are in a list-column.
require(tidyr)
nasa_keyword = data_frame(id = metadata$dataset$`_id`$`$oid`, keyword = metadata$dataset$keyword) %>% unnest(metadata$dataset$keyword)

## Now use tidytext's unnest_tokens() for the title and description fields 
## so that we can do the text analysis
# Remove stop_words too!

require(tidytext)
nasa_title_new = nasa_title %>% unnest_tokens(word,title) %>% anti_join(stop_words);nasa_title_new
nasa_desc_new = nasa_desc %>% unnest_tokens(word,description) %>% anti_join(stop_words);nasa_desc_new
## Now we have it in "tidy-text format" we have been working with throughout this book
## which is one-token-per-row

## Most common words in the NASA dataset titles?
nasa_title_new %>% count(nasa_title_new$word, sort = TRUE)
## Most common words in the NASA dataset description?
nasa_desc_new %>% count(nasa_desc_new$word, sort = TRUE)
## Most common Keywords? 
nasa_keyword %>% group_by(nasa_keyword$`metadata$dataset$keyword`) %>% count(sort=TRUE)


# 2. Word co-ocurrences and correlations
# words occur together in titles, description and keywords

## use pairwise_count() from the widyr package to count how many times each pair of words occurs together

require(widyr)
title_word_pairs = nasa_title_new %>% pairwise_count(word, id, sort= TRUE, upper= FALSE);title_word_pairs 
desc_word_pairs = nasa_desc_new %>% pairwise_count(word, id, sort= TRUE, upper= FALSE);desc_word_pairs 

## Lets' plot this network