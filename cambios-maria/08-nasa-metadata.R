## ----echo = FALSE-----------------------------------------------------------------------------------------------
library(knitr)
opts_chunk$set(message = FALSE, warning = FALSE, cache = TRUE,
               cache.lazy = FALSE)
options(width = 100, dplyr.width = 100)
library(ggplot2)
library(methods)
theme_set(theme_light())


## What is metadata? Metadata is a term that refers to data that gives information about other data; in this case, the metadata informs users about what is in these numerous NASA datasets but does not include the content of the datasets themselves.


## ----eval=FALSE-------------------------------------------------------------------------------------------------
## library(jsonlite)
## metadata <- fromJSON("https://data.nasa.gov/data.json")
## names(metadata$dataset)


## ----download, echo=FALSE---------------------------------------------------------------------------------------
load("data/metadata.rda")
names(metadata$dataset)


## ----class, dependson = "download"------------------------------------------------------------------------------
class(metadata$dataset$title)
class(metadata$dataset$description)
class(metadata$dataset$keyword)


## ----title, dependson = "download", message=FALSE---------------------------------------------------------------
library(dplyr)

nasa_title <- tibble(id = metadata$dataset$`_id`$`$oid`, 
                         title = metadata$dataset$title)
nasa_title


## ----desc, dependson = "download", dplyr.width = 150------------------------------------------------------------
nasa_desc <- tibble(id = metadata$dataset$`_id`$`$oid`, 
                        desc = metadata$dataset$description)

nasa_desc %>% 
  select(desc) %>% 
  sample_n(5)


## ----keyword, dependson = "download"----------------------------------------------------------------------------
library(tidyr)

nasa_keyword <- tibble(id = metadata$dataset$`_id`$`$oid`, 
                           keyword = metadata$dataset$keyword) %>%
  unnest(keyword)

nasa_keyword


## ----unnest, dependson = c("title","desc")----------------------------------------------------------------------
library(tidytext)

nasa_title <- nasa_title %>% 
  unnest_tokens(word, title) %>% 
  anti_join(stop_words)

nasa_desc <- nasa_desc %>% 
  unnest_tokens(word, desc) %>% 
  anti_join(stop_words)


## ----dependson = "unnest"---------------------------------------------------------------------------------------
nasa_title
nasa_desc


## ----dependson = "unnest"---------------------------------------------------------------------------------------
nasa_title %>%
  count(word, sort = TRUE)


## ----dependson = "unnest"---------------------------------------------------------------------------------------
nasa_desc %>% 
  count(word, sort = TRUE)


## We can do this by making a list of custom stop words and using `anti_join()` to remove them from the data frame, just like we removed the default stop words that are in the tidytext package. This approach can be used in many instances and is a great tool to bear in mind.


## ----my_stopwords, dependson = "unnest"-------------------------------------------------------------------------
my_stopwords <- tibble(word = c(as.character(1:10), 
                                    "v1", "v03", "l2", "l3", "l4", "v5.2.0", 
                                    "v003", "v004", "v005", "v006", "v7"))
nasa_title <- nasa_title %>% 
  anti_join(my_stopwords)
nasa_desc <- nasa_desc %>% 
  anti_join(my_stopwords)


## ----dependson = "keyword"--------------------------------------------------------------------------------------
nasa_keyword %>% 
  group_by(keyword) %>% 
  count(sort = TRUE)


## ----toupper, dependson = "keyword"-----------------------------------------------------------------------------
nasa_keyword <- nasa_keyword %>% 
  mutate(keyword = toupper(keyword))


## ----title_word_pairs, dependson = "my_stopwords"---------------------------------------------------------------
library(widyr)

title_word_pairs <- nasa_title %>% 
  pairwise_count(word, id, sort = TRUE, upper = FALSE)

title_word_pairs


## ----desc_word_pairs, dependson = "my_stopwords"----------------------------------------------------------------
desc_word_pairs <- nasa_desc %>% 
  pairwise_count(word, id, sort = TRUE, upper = FALSE)

desc_word_pairs


## ----plottitle, dependson = "title_word_pairs", fig.height=6, fig.width=9, fig.cap="Word network in NASA dataset titles"----
library(ggplot2)
library(igraph)
library(ggraph)

set.seed(1234)
title_word_pairs %>%
  filter(n >= 250) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()


## ----plotdesc, dependson = "desc_word_pairs", fig.height=6, fig.width=9, fig.cap="Word network in NASA dataset descriptions"----
set.seed(1234)
desc_word_pairs %>%
  filter(n >= 5000) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "darkred") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()



## ----plotcounts, dependson = "toupper", fig.height=7, fig.width=9, fig.cap="Co-occurrence network in NASA dataset keywords"----
keyword_pairs <- nasa_keyword %>% 
  pairwise_count(keyword, id, sort = TRUE, upper = FALSE)

keyword_pairs

set.seed(1234)
keyword_pairs %>%
  filter(n >= 700) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()


## These are the most commonly co-occurring words, but also just the most common keywords in general.


## ----keyword_cors, dependson = "toupper"------------------------------------------------------------------------
keyword_cors <- nasa_keyword %>% 
  group_by(keyword) %>%
  filter(n() >= 50) %>%
  pairwise_cor(keyword, id, sort = TRUE, upper = FALSE)

keyword_cors


## ----plotcors, dependson = "keyword_cors", fig.height=8, fig.width=12, fig.cap="Correlation network in NASA dataset keywords"----
set.seed(1234)
keyword_cors %>%
  filter(correlation > .6) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()


## ----desc_tf_idf, dependson = "my_stopwords"--------------------------------------------------------------------
desc_tf_idf <- nasa_desc %>% 
  count(id, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, id, n)


## ----dependson = "desc_tf_idf"----------------------------------------------------------------------------------
desc_tf_idf %>% 
  arrange(-tf_idf)


## Notice we have run into an issue here; both $n$ and term frequency are equal to 1 for these terms, meaning that these were description fields that only had a single word in them. If a description field only contains one word, the tf-idf algorithm will think that is a very important word.


## ----full_join, dependson = c("desc_tf_idf", "toupper")---------------------------------------------------------
desc_tf_idf <- full_join(desc_tf_idf, nasa_keyword, by = "id")


## ----plottfidf, dependson = "full_join", fig.width=10, fig.height=7, fig.cap="Distribution of tf-idf for words from datasets labeled with selected keywords"----
desc_tf_idf %>% 
  filter(!near(tf, 1)) %>%
  filter(keyword %in% c("SOLAR ACTIVITY", "CLOUDS", 
                        "SEISMOLOGY", "ASTROPHYSICS",
                        "HUMAN HEALTH", "BUDGET")) %>%
  arrange(desc(tf_idf)) %>%
  group_by(keyword) %>%
  distinct(word, keyword, .keep_all = TRUE) %>%
  top_n(15, tf_idf) %>% 
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  ggplot(aes(word, tf_idf, fill = keyword)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~keyword, ncol = 3, scales = "free") +
  coord_flip() +
  labs(title = "Highest tf-idf words in NASA metadata description fields",
       caption = "NASA metadata from https://data.nasa.gov/data.json",
       x = NULL, y = "tf-idf")


## ----word_counts, dependson = "my_stopwords"--------------------------------------------------------------------
my_stop_words <- bind_rows(stop_words, 
                           tibble(word = c("nbsp", "amp", "gt", "lt",
                                               "timesnewromanpsmt", "font",
                                               "td", "li", "br", "tr", "quot",
                                               "st", "img", "src", "strong",
                                               "http", "file", "files",
                                               as.character(1:12)), 
                                      lexicon = rep("custom", 30)))

word_counts <- nasa_desc %>%
  anti_join(my_stop_words) %>%
  count(id, word, sort = TRUE) %>%
  ungroup()

word_counts


## ----desc_dtm, dependson = "word_counts"------------------------------------------------------------------------
desc_dtm <- word_counts %>%
  cast_dtm(id, word, n)

desc_dtm


## ---- eval = FALSE----------------------------------------------------------------------------------------------
## library(topicmodels)
## 
## # be aware that running this model is time intensive
## desc_lda <- LDA(desc_dtm, k = 24, control = list(seed = 1234))
## desc_lda


## ----desc_lda, echo=FALSE---------------------------------------------------------------------------------------
library(topicmodels)
load("data/desc_lda.rda")
desc_lda


## ----tidy_lda, dependson = "desc_lda"---------------------------------------------------------------------------
tidy_lda <- tidy(desc_lda)

tidy_lda


## ----top_terms, dependson = "tidy_lda"--------------------------------------------------------------------------
top_terms <- tidy_lda %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms


## ----plotbeta, dependson = "top_terms", fig.width=12, fig.height=16, fig.cap="Top terms in topic modeling of NASA metadata description field texts"----
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Top 10 terms in each LDA topic",
       x = NULL, y = expression(beta)) +
  facet_wrap(~ topic, ncol = 4, scales = "free")


## ----lda_gamma, dependson = "desc_lda"--------------------------------------------------------------------------
lda_gamma <- tidy(desc_lda, matrix = "gamma")

lda_gamma


## ----plotgammaall, dependson = "lda_gamma", fig.width=7, fig.height=5, fig.cap="Probability distribution in topic modeling of NASA metadata description field texts"----
ggplot(lda_gamma, aes(gamma)) +
  geom_histogram() +
  scale_y_log10() +
  labs(title = "Distribution of probabilities for all topics",
       y = "Number of documents", x = expression(gamma))


## ----plotgamma, dependson = "lda_gamma", fig.width=10, fig.height=12, fig.cap="Probability distribution for each topic in topic modeling of NASA metadata description field texts"----
ggplot(lda_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 4) +
  scale_y_log10() +
  labs(title = "Distribution of probability for each topic",
       y = "Number of documents", x = expression(gamma))


## ----lda_join, dependson = c("lda_gamma", "toupper")------------------------------------------------------------
lda_gamma <- full_join(lda_gamma, nasa_keyword, by = c("document" = "id"))

lda_gamma


## ----top_keywords, dependson = "lda_join"-----------------------------------------------------------------------
top_keywords <- lda_gamma %>% 
  filter(gamma > 0.9) %>% 
  count(topic, keyword, sort = TRUE)

top_keywords


## ----plottopkeywords, dependson = "top_keywords", fig.width=16, fig.height=16, fig.cap="Top keywords in topic modeling of NASA metadata description field texts"----
top_keywords %>%
  group_by(topic) %>%
  top_n(5, n) %>%
  ungroup %>%
  mutate(keyword = reorder_within(keyword, n, topic)) %>%
  ggplot(aes(keyword, n, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  labs(title = "Top keywords for each LDA topic",
       x = NULL, y = "Number of documents") +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~ topic, ncol = 4, scales = "free")

