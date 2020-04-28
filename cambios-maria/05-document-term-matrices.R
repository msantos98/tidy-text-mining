# Chunk inicial -----------------------------------------------------------

library(ggplot2)
library(methods)
theme_set(theme_light())
library(purrr)
library(tm)
library(topicmodels)
library(dplyr)
library(tidytext)
library(tidyr)
library(Matrix)
library(janeaustenr)
library(stringr)
library(knitr)



# Sección 5.1 -------------------------------------------------------------

# Document-term matrix: usualmente son matrices sparse donde,
# * Cada fila representa un documento del corpus
# * Cada columna, un término
# * Cada valor contiene el número de apariciones de cada término en el documento correspondiente

data("AssociatedPress", package = "topicmodels")
AssociatedPress

terms <- Terms(AssociatedPress) # Accedemos a los términos
head(terms)

# Convertimos la DTM a un tidy dataframe
# Solamente se mantienen los elementos con valores no nulos: no hay filas con count = 0

ap_td <- tidy(AssociatedPress)
ap_td

# Hacemos un sentiment analysis con bing

ap_sentiments <- ap_td %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))

ap_sentiments

# Lo mostramos gráficamente

ap_sentiments %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 200) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip()


# Pasamos a otro ejemplo: Discursos presidenciales de inauguración
# Es una DFM (Document-Feature Matrix)
data("data_corpus_inaugural", package = "quanteda")
inaug_dfm <- quanteda::dfm(data_corpus_inaugural, verbose = FALSE)

inaug_dfm

# Pasamos la DFM a un tidy dataframe

inaug_td <- tidy(inaug_dfm)
inaug_td

# Queremos ver las palabras más específicas de los discursos: hacemos un tf-idf

inaug_tf_idf <- inaug_td %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

inaug_tf_idf

unique(inaug_tf_idf$document) # Vemos que hay muchos autores de discursos.

# Seleccionamos 4 oradores (Roosevelt, Lincoln, Kennedy, Obama) y visualizamos top 10 de tf_idf

speeches <- c("1933-Roosevelt", "1861-Lincoln",
              "1961-Kennedy", "2009-Obama")

inaug_tf_idf %>%
  filter(document %in% speeches) %>%
  group_by(document) %>%
  top_n(10, tf_idf) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, tf_idf, document)) %>%
  ggplot(aes(term, tf_idf, fill = document)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ document, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = "",
       y = "tf-idf")

# Vamos a visualizar, para una selección de palabras, la frecuencia de éstas por año
# Usamos complete() para incluir entradas con count = 0

# Contamos el número de palabras por año:

year_term_counts <- inaug_td %>%
  extract(document, "year", "(\\d+)", convert = TRUE) %>% # Sacamos el año
  complete(year, term, fill = list(count = 0)) %>% # Añadimos las palabras con count = 0
  group_by(year) %>%
  mutate(year_total = sum(count))

# Visualizamos el número de veces que una selección de palabras fueron dichas por año (en proporción)

year_term_counts %>%
  filter(term %in% c("god", "america", "foreign", "union", "constitution", "freedom")) %>%
  ggplot(aes(year, count / year_total)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ term, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("% frequency of word in inaugural address")



# Sección 5.2 -------------------------------------------------------------

# Pasamos el tidy dataframe a DTM

ap_td %>%
  cast_dtm(document, term, count)

# Pasamos el tidy dataframe a DFM

ap_td %>%
  cast_dfm(document, term, count)

# Pasamos el tidy dataframe a una sparse matrix

m <- ap_td %>%
  cast_sparse(document, term, count)

class(m) # Es de la clase matriz
dim(m)

# Creamos una DTM de los libros de Jane Austen

austen_dtm <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word) %>%
  cast_dtm(book, word, n)

austen_dtm



# Sección 5.3 -------------------------------------------------------------

# Empezamos otro ejemplo: 50 artículos del servicio de noticias Reuters

data("acq")
acq

# Accedemos al primer documento:

acq[[1]]

# Pasamos de una estructura como de lista a tidy dataframe (también se incluyen los metadatos)

acq_td <- tidy(acq)
acq_td

# Tokenizamos

acq_tokens <- acq_td %>%
  select(-places) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

# Calculamos las palabras más comunes

acq_tokens %>%
  count(word, sort = TRUE)

# También hacemos tf-idf

acq_tokens %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))


# Pasamos a un nuevo ejemplo: los artículos más relevantes para nueve acciones tecnológicas principales
#   Microsoft, Apple, Google, Amazon, Facebook, Twitter, IBM, Yahoo y Netflix.

load("data/stock_articles.rda")
stock_articles

# Convertimos cada objeto WebCorpus (uno para cada compañía) a un tidy dataframe

stock_tokens <- stock_articles %>%
  mutate(corpus = map(corpus, tidy)) %>%
  unnest(cols = (corpus)) %>%
  unnest_tokens(word, text) %>%
  select(company, datetimestamp, word, id, heading)

stock_tokens

# Hacemos tf_idf y mostramos el top 8

stock_tf_idf <- stock_tokens %>%
  count(company, word) %>%
  filter(!str_detect(word, "\\d+")) %>%
  bind_tf_idf(word, company, n) %>%
  arrange(-tf_idf)

stock_tf_idf %>%
  group_by(company) %>%
  top_n(8, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = company)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ company, scales = "free") +
  coord_flip() +
  labs(x = "Word",
       y = "tf-idf")

# Vamos a hacer sentiment analysis con AFINN.
# Primero veamos qué tal va AFINN como diccionario para nuestro ejemplo de stocks

load("data/afinn.rda")
stock_tokens %>%
  anti_join(stop_words, by = "word") %>%
  count(word, id, sort = TRUE) %>%
  inner_join(afinn, by = "word") %>%
  group_by(word) %>%
  summarize(contribution = sum(n * value)) %>%
  top_n(12, abs(contribution)) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, contribution)) +
  geom_col() +
  coord_flip() +
  labs(y = "Frequency of word * AFINN value")

# Como AFINN no va bien para este caso, utilizamos un nuevo lexicon: the Loughran and McDonald dictionary of financial
#  sentiment terms. Divide las palabras en 6 sentimientos: “positive”, “negative”, “litigious”, “uncertain”,
#   “constraining”, and “superfluous”

load("data/loughran.rda")
stock_tokens %>%
  count(word) %>%
  inner_join(loughran, by = "word") %>%
  group_by(sentiment) %>%
  top_n(5, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ sentiment, scales = "free") +
  ylab("Frequency of this word in the recent financial articles")

# Ahora ya podemos hacer el sentimetn analysis con the Loughran and McDonald dictionary of financial sentiment terms

stock_sentiment_count <- stock_tokens %>%
  inner_join(loughran, by = "word") %>%
  count(sentiment, company) %>%
  spread(sentiment, n, fill = 0)

stock_sentiment_count

# Hacemos un analisis de sentimiento general para las compañías

stock_sentiment_count %>%
  mutate(score = (positive - negative) / (positive + negative)) %>%
  mutate(company = reorder(company, score)) %>%
  ggplot(aes(company, score, fill = score > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = "Company",
       y = "Positivity score among 20 recent news articles")

