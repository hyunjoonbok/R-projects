## Text Mining using Sparklyr
require(gutenbergr)

gutenberg_works()  %>%
  filter(author == "Twain, Mark") %>%
  pull(gutenberg_id) %>%
  gutenberg_download() %>%
  pull(text) %>%
  writeLines("mark_twain.txt")

readLines("mark_twain.txt", 10) 

# CONNECT TO SPARK

sc <- spark_connect(master = "local", version = "2.1.0")

# SPARK_READ_TEXT()
twain_path <- paste0("file:///", getwd(), "/mark_twain.txt")
twain <-  spark_read_text(sc, "twain", path = twain_path) 

# Data transformation

all_words <- twain %>%
  filter(nchar(line) > 0)

# REGEXP_REPLACE

all_words <- all_words %>%
  mutate(line = regexp_replace(line, "[_\"\'():;,.!?\\-]", " ")) 

# FT_TOKENIZER()

all_words <- all_words %>%
  ft_tokenizer(input.col = "line",
               output.col = "word_list")

head(all_words, 4)

# FT_STOP_WORDS_REMOVER()

all_words <- all_words %>%
  ft_stop_words_remover(input.col = "word_list",
                        output.col = "wo_stop_words")

head(all_words, 4)

# EXPLODE()
# The Hive UDF explode performs the job of unnesting the tokens into their own row

all_words <- all_words %>%
  mutate(word = explode(wo_stop_words)) %>%
  select(word) %>%
  filter(nchar(word) > 2)

head(all_words, 4)

# COMPUTE()
# compute() will operate this transformation and cache the results in Spark memory

all_words <- all_words %>%
  compute("all_words")


# Data Analysis
# WORDS USED THE MOST
word_count <- all_words %>%
  group_by(word) %>%
  tally() %>%
  arrange(desc(n));word_count


require(wordcloud)
word_count %>%
  head(100) %>%
  collect() %>%
  with(wordcloud(
    word, 
    n,
    colors = c("#999999", "#E69F00", "#56B4E9","#56B4E9")))

