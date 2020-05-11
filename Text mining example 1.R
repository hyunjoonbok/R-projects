# ==== 1. Text Mining

require(dplyr)
require(ggplot2)
require(tidytext)
require(tidyr)

## read text
lyrics <- readLines("sample.txt")
lyrics_df <- data_frame(Text = lyrics)
head(lyrics_df, n = 10)


## Unnest each token in each row
words <- lyrics_df %>% 
  unnest_tokens(output = word, input = Text)

## Remove stop words
words <- words %>% anti_join(stop_words)

wordcount <- words %>% count(word, sort = TRUE)

## Plot these unique words
wordcount %>% mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) + geom_col(fill = "blue") +
  coord_flip() + labs(x = "word", y = "count") +
  geom_text(aes(label = n), color = "white", hjust = 1.2)



# ==== 2. Sentiment analysis

# one of the lexicons
get_sentiments("nrc")

words_nrc <- wordcount %>% inner_join(get_sentiments("nrc"), by= "word") %>%
  filter(sentiment %in% c("positive","negative"))

# Plot with nrc lexicon
words_nrc %>% ggplot(aes(x = reorder(word,n), y=n, fill = sentiment)) +
  geom_bar(stat = "identity") + facet_wrap(~sentiment, scales = "free_y") +
  coord_flip()

# wordcloud
require(wordcloud)
words %>%
  count(word) %>%
  collect() %>%
  with(wordcloud(word, n))



## 3. Analyzing Bigram
bigrams <- lyrics_df %>% unnest_tokens(bigram, Text, token = "ngrams", n = 2)
