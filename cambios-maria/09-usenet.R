## ----echo = FALSE-----------------------------------------------------------------------------------------------
library(knitr)
opts_chunk$set(message = FALSE, warning = FALSE, cache = TRUE)
options(width = 100, dplyr.width = 150)
library(ggplot2)
library(methods)
theme_set(theme_light())


## Note that this step may take several minutes to read all the documents.


## ----libraries--------------------------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)
library(readr)


## ----eval = FALSE-----------------------------------------------------------------------------------------------
## training_folder <- "data/20news-bydate/20news-bydate-train/"
## 
## # Define a function to read all files from a folder into a data frame
## read_folder <- function(infolder) {
##   tibble(file = dir(infolder, full.names = TRUE)) %>%
##     mutate(text = map(file, read_lines)) %>%
##     transmute(id = basename(file), text) %>%
##     unnest(text)
## }
## 
## # Use unnest() and map() to apply read_folder to each subfolder
## raw_text <- tibble(folder = dir(training_folder, full.names = TRUE)) %>%
##   mutate(folder_out = map(folder, read_folder)) %>%
##   unnest(cols = c(folder_out)) %>%
##   transmute(newsgroup = basename(folder), id, text)


## ----raw_text, depends = "libraries", echo = FALSE--------------------------------------------------------------
load("data/raw_text.rda")


## ----dependson = "raw_text"-------------------------------------------------------------------------------------
raw_text


## ----messagecounts, dependson="raw_text", fig.cap = "Number of messages from each newsgroup"--------------------
library(ggplot2)

raw_text %>%
  group_by(newsgroup) %>%
  summarize(messages = n_distinct(id)) %>%
  ggplot(aes(newsgroup, messages)) +
  geom_col() +
  coord_flip()


## ----cleaned_text1, dependson = "raw_text"----------------------------------------------------------------------
library(stringr)

# must occur after the first occurrence of an empty line,
# and before the first occurrence of a line starting with --
cleaned_text <- raw_text %>%
  group_by(newsgroup, id) %>%
  filter(cumsum(text == "") > 0,
         cumsum(str_detect(text, "^--")) == 0) %>%
  ungroup()


## We also choose to manually remove two messages, `9704` and `9985` that contained a large amount of non-text content.


## ----cleaned_text2, dependson = "cleaned_text1"-----------------------------------------------------------------
cleaned_text <- cleaned_text %>%
  filter(str_detect(text, "^[^>]+[A-Za-z\\d]") | text == "",
         !str_detect(text, "writes(:|\\.\\.\\.)$"),
         !str_detect(text, "^In article <"),
         !id %in% c(9704, 9985))


## ----usenet_words, dependson = "cleaned_text2"------------------------------------------------------------------
library(tidytext)

usenet_words <- cleaned_text %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word)


## ----words_by_newsgroup, dependson = "usenet_words"-------------------------------------------------------------
usenet_words %>%
  count(word, sort = TRUE)

words_by_newsgroup <- usenet_words %>%
  count(newsgroup, word, sort = TRUE) %>%
  ungroup()

words_by_newsgroup


## ----tf_idf, dependson = "words_by_usergroup"-------------------------------------------------------------------
tf_idf <- words_by_newsgroup %>%
  bind_tf_idf(word, newsgroup, n) %>%
  arrange(desc(tf_idf))

tf_idf


## ----scitfidf, dependson = "tf_idf", fig.width=9, fig.height=8, fig.cap = "The 12 terms with the highest tf-idf within each of the science-related newsgroups"----
tf_idf %>%
  filter(str_detect(newsgroup, "^sci\\.")) %>%
  group_by(newsgroup) %>%
  top_n(12, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = newsgroup)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ newsgroup, scales = "free") +
  ylab("tf-idf") +
  coord_flip()


## ---- dependson = "tf_idf", echo = FALSE, fig.width=9, fig.height=8, eval = FALSE, echo = FALSE-----------------
## plot_tf_idf <- function(d) {
##   d %>%
##     group_by(newsgroup) %>%
##     top_n(10, tf_idf) %>%
##     mutate(word = reorder(word, tf_idf)) %>%
##     ggplot(aes(word, tf_idf, fill = newsgroup)) +
##     geom_col(show.legend = FALSE) +
##     facet_wrap(~ newsgroup, scales = "free") +
##     ylab("tf-idf") +
##     coord_flip()
## }
## 
## tf_idf %>%
##   filter(str_detect(newsgroup, "^rec\\.")) %>%
##   plot_tf_idf()


## ----newsgroup_cors, dependson = "words_by_newsgroup"-----------------------------------------------------------
library(widyr)

newsgroup_cors <- words_by_newsgroup %>%
  pairwise_cor(newsgroup, word, n, sort = TRUE)

newsgroup_cors


## ----newsgroupcorsnetwork, dependson = "newsgroup_cors", fig.width = 8, fig.height = 8, fig.cap = "A network of Usenet groups based on the correlation of word counts between them, including only connections with a correlation greater than .4"----
library(ggraph)
library(igraph)
set.seed(2017)

newsgroup_cors %>%
  filter(correlation > .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation, width = correlation)) +
  geom_node_point(size = 6, color = "lightblue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


## ----sci_dtm, dependson = "usenet_words"------------------------------------------------------------------------
# include only words that occur at least 50 times
word_sci_newsgroups <- usenet_words %>%
  filter(str_detect(newsgroup, "^sci")) %>%
  group_by(word) %>%
  mutate(word_total = n()) %>%
  ungroup() %>%
  filter(word_total > 50)

# convert into a document-term matrix
# with document names such as sci.crypt_14147
sci_dtm <- word_sci_newsgroups %>%
  unite(document, newsgroup, id) %>%
  count(document, word) %>%
  cast_dtm(document, word, n)


## ----sci_lda, dependson = "sci_dtm"-----------------------------------------------------------------------------
library(topicmodels)
sci_lda <- LDA(sci_dtm, k = 4, control = list(seed = 2016))


## ----usenettopicterms, dependson = "sci_lda", fig.cap = "The top 8 words from each topic fit by LDA on the science-related newsgroups"----
sci_lda %>%
  tidy() %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered()


## ----usenetassignments, dependson = "sci_lda", fig.cap = "Distribution of gamma for each topic within each Usenet newsgroup"----
sci_lda %>%
  tidy(matrix = "gamma") %>%
  separate(document, c("newsgroup", "id"), sep = "_") %>%
  mutate(newsgroup = reorder(newsgroup, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ newsgroup) +
  labs(x = "Topic",
       y = "# of messages where this was the highest % topic")


## ----eval=FALSE-------------------------------------------------------------------------------------------------
## newsgroup_sentiments <- words_by_newsgroup %>%
##   inner_join(get_sentiments("afinn"), by = "word") %>%
##   group_by(newsgroup) %>%
##   summarize(value = sum(value * n) / sum(n))
## 
## newsgroup_sentiments %>%
##   mutate(newsgroup = reorder(newsgroup, value)) %>%
##   ggplot(aes(newsgroup, value, fill = value > 0)) +
##   geom_col(show.legend = FALSE) +
##   coord_flip() +
##   ylab("Average sentiment value")


## ----newsgroupsentiments, dependson = "words_by_newsgroup", echo = FALSE, fig.width=7, fig.height=6, fig.cap = "Average AFINN value for posts within each newsgroup"----
load("data/afinn.rda")
newsgroup_sentiments <- words_by_newsgroup %>%
  inner_join(afinn, by = "word") %>%
  group_by(newsgroup) %>%
  summarize(value = sum(value * n) / sum(n))

newsgroup_sentiments %>%
  mutate(newsgroup = reorder(newsgroup, value)) %>%
  ggplot(aes(newsgroup, value, fill = value > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("Average sentiment value")


## ----eval=FALSE-------------------------------------------------------------------------------------------------
## contributions <- usenet_words %>%
##   inner_join(get_sentiments("afinn"), by = "word") %>%
##   group_by(word) %>%
##   summarize(occurences = n(),
##             contribution = sum(value))
## 
## contributions


## ----contributions, dependson = "newsgroup_sentiments", echo=FALSE----------------------------------------------
contributions <- usenet_words %>%
  inner_join(afinn, by = "word") %>%
  group_by(word) %>%
  summarize(occurences = n(),
            contribution = sum(value))

contributions


## ----usenetcontributions, dependson = "contributions", fig.width=6, fig.height=6, fig.cap = "Words with the greatest contributions to positive/negative sentiment values in the Usenet text"----
contributions %>%
  top_n(25, abs(contribution)) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip()


## ----eval=FALSE-------------------------------------------------------------------------------------------------
## top_sentiment_words <- words_by_newsgroup %>%
##   inner_join(get_sentiments("afinn"), by = "word") %>%
##   mutate(contribution = value * n / sum(n))
## 
## top_sentiment_words


## ----top_sentiment_words, dependson = "words_by_newsgroup", echo=FALSE------------------------------------------
top_sentiment_words <- words_by_newsgroup %>%
  inner_join(afinn, by = "word") %>%
  mutate(contribution = value * n / sum(n))

top_sentiment_words


## ----newsgroupsentiment, fig.height = 10, fig.width = 10, dependson = "top_sentiment_words", echo = FALSE, fig.cap = "The 12 words that contributed the most to sentiment scores within each of six newsgroups"----
top_sentiment_words %>%
  filter(str_detect(newsgroup, "^(talk|alt|misc)")) %>%
  group_by(newsgroup) %>%
  top_n(12, abs(contribution)) %>%
  ungroup() %>%
  mutate(newsgroup = reorder(newsgroup, contribution),
         word = reorder(paste(word, newsgroup, sep = "__"), contribution)) %>%
  ggplot(aes(word, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  facet_wrap(~ newsgroup, scales = "free") +
  coord_flip() +
  labs(x = "",
       y = "Sentiment value * # of occurrences")


## ----eval=FALSE-------------------------------------------------------------------------------------------------
## sentiment_messages <- usenet_words %>%
##   inner_join(get_sentiments("afinn"), by = "word") %>%
##   group_by(newsgroup, id) %>%
##   summarize(sentiment = mean(value),
##             words = n()) %>%
##   ungroup() %>%
##   filter(words >= 5)


## ----sentiment_messages, echo=FALSE-----------------------------------------------------------------------------
sentiment_messages <- usenet_words %>%
  inner_join(afinn, by = "word") %>%
  group_by(newsgroup, id) %>%
  summarize(sentiment = mean(value),
            words = n()) %>%
  ungroup() %>%
  filter(words >= 5)


## As a simple measure to reduce the role of randomness, we filtered out messages that had fewer than five words that contributed to sentiment.


## ----dependson = "sentiment_messages"---------------------------------------------------------------------------
sentiment_messages %>%
  arrange(desc(sentiment))


## ----print_message, dependson = "cleaned_text"------------------------------------------------------------------
print_message <- function(group, message_id) {
  result <- cleaned_text %>%
    filter(newsgroup == group, id == message_id, text != "")
  
  cat(result$text, sep = "\n")
}

print_message("rec.sport.hockey", 53560)


## ----dependson = "sentiment_messages"---------------------------------------------------------------------------
sentiment_messages %>%
  arrange(sentiment)

print_message("rec.sport.hockey", 53907)


## ----usenet_bigrams, dependson = "cleaned_text"-----------------------------------------------------------------
usenet_bigrams <- cleaned_text %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)


## ----usenet_bigram_counts, dependson = "usenet_bigrams"---------------------------------------------------------
usenet_bigram_counts <- usenet_bigrams %>%
  count(newsgroup, bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")


## ----eval=FALSE-------------------------------------------------------------------------------------------------
## negate_words <- c("not", "without", "no", "can't", "don't", "won't")
## 
## usenet_bigram_counts %>%
##   filter(word1 %in% negate_words) %>%
##   count(word1, word2, wt = n, sort = TRUE) %>%
##   inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
##   mutate(contribution = value * n) %>%
##   group_by(word1) %>%
##   top_n(10, abs(contribution)) %>%
##   ungroup() %>%
##   mutate(word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>%
##   ggplot(aes(word2, contribution, fill = contribution > 0)) +
##   geom_col(show.legend = FALSE) +
##   facet_wrap(~ word1, scales = "free", nrow = 3) +
##   scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
##   xlab("Words preceded by a negation") +
##   ylab("Sentiment value * # of occurrences") +
##   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
##   coord_flip()


## ----negatewords, dependson = "usenet_bigram_counts", fig.width=8, fig.height=10, echo=FALSE, fig.cap = "Words that contributed the most to sentiment when they followed a 'negating' word"----
negate_words <- c("not", "without", "no", "can't", "don't", "won't")

usenet_bigram_counts %>%
  filter(word1 %in% negate_words) %>%
  count(word1, word2, wt = n, sort = TRUE) %>%
  inner_join(afinn, by = c(word2 = "word")) %>%
  mutate(contribution = value * n) %>%
  group_by(word1) %>%
  top_n(10, abs(contribution)) %>%
  ungroup() %>%
  mutate(word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>%
  ggplot(aes(word2, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free", nrow = 3) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  xlab("Words preceded by a negation") +
  ylab("Sentiment value * # of occurrences") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()

