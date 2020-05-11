##1.  Tidy Text format


require(dplyr)
text_df = data_frame(line= 1:4,text=text);text_df

require(tidytext)
text_df %>% unnest_tokens(word,text)

require(janeaustenr)
require(stringr)

original_books = austen_books() %>% group_by(book) %>% mutate(linenumber = row_number(), chapter = cumsum(str_detect(text,regex("^chapter [\\divxlc]",ignore_case = TRUE)))) %>% ungroup()

#Make it tidy (one-token-per-row)
tidy_books = original_books %>% unnest_tokens(word,text)

#Get rid of top words
data("stop_words")
tidy_books = tidy_books %>% anti_join(stop_words)

##Finding most common word for fun using "count()" and create visulization
tidy_books %>% count(word,sort = T)

require(ggplot2)

tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()



#2. Sentiment analysis with tidy data
sentiments
#using NRC sentiment classification
get_sentiments("nrc")

#start by making tidy like one at the top
original_books = austen_books() %>% group_by(book) %>% mutate(linenumber = row_number(), chapter = cumsum(str_detect(text,regex("^chapter [\\divxlc]",ignore_case = TRUE)))) %>% ungroup()
tidy_books = original_books %>% unnest_tokens(word,text)

#Get rid of stop words
data("stop_words")
tidy_books = tidy_books %>% anti_join(stop_words)

nrcjoy <- get_sentiments("nrc") %>% filter(sentiment == "joy")
tidy_books = tidy_books %>% filter(book == 'Emma') %>% inner_join(nrcjoy) %>% count(word, sort = TRUE)


## Most common positive and negative words using "bing" sentiment category
bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts



#3 .
bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

#4 Wordclouds
require(wordcloud)
tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

## 
##############################

##Analyzing word and document frequency

## how to quantify what a document is about. Can we do this by looking at the words that make up the document?
## We do this by TF-IDF

library(dplyr)
library(janeaustenr)
library(tidytext)

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>%
  ungroup()


total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))
book_words <- left_join(book_words, total_words);book_words

library(ggplot2)
ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

#Zipf's law states that the frequency that a word appears is inversely proportional to its rank.
freq_by_rank <- book_words %>% 
  group_by(book) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total);freq_by_rank

## LEts's do TF-IDF (decreasing the weight for commonly used words and increasing the weight for words that are not used very much!!)
book_words <- book_words %>% bind_tf_idf(word, book, n);book_words

book_words %>%
  arrange(desc(tf_idf))

plot_austen <- book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

plot_austen %>% 
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

## Word Cloud

library(wordcloud)

tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 50))


## Tokenizing by n-gram (When we set n to 2, we are examining pairs of two consecutive words, often called "bigrams)
library(dplyr)
library(tidytext)
library(janeaustenr)

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams

# Separate Bigram into word 1 and word 2
require(tidyr)
bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ");bigrams_united

bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

## # filter for only relatively common combinations
require(igraph)
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame();bigram_graph

require(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


## Topic modeling
#Topic modeling is a method for unsupervised classification of such documents, 
# similar to clustering on numeric data, which finds natural groups of items even when we're not sure what we're looking for.

require(topicmodels)
data("AssociatedPress")

#use the LDA() function from the topicmodels package, setting k = 2, to create a two-topic LDA model.
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234));ap_lda
# Now interpret the model using tidying functions from the tidytext package.
require(tidytext)
require(broom)
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics
