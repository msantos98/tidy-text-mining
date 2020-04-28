# Chunk inicial -----------------------------------------------------------

library(knitr)
library(ggplot2)
library(dplyr)
library(tidytext)
library(janeaustenr)
library(tidyr)
library(textdata)
library(igraph)
library(ggraph)
library(gutenbergr)
library(stringr)
library(widyr)
theme_set(theme_light())

# Sección 4.1 -------------------------------------------------------------

# Tokenizamos por bigramas los libros de Jane Austen

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams

# Calculamos los bigramas más utilizados

austen_bigrams %>%
  count(bigram, sort = TRUE)

# Separamos los bigramas en 2 columnas

bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Filtramos los bigramas

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# Volvemos a contar una vez hecho el filtrado

bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

bigram_counts

# Volvemos a unir los bigramas en una sola columna, separados por un espacio

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united


# Esta vez tokenizamos por trigramas y filtramos para quedarnos con palabras significativas

austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

# Buscamos los nombres de calles más populares de los libros de Jane Austen
# En ingles las calles son Nombre street, por eso buscamos en la segunda columna

bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)

# Hacemos tf-idf de los bigramas

bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

# Representamos el tf-idf de los bigramas

bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  group_by(book) %>%
  top_n(12, tf_idf) %>%
  ungroup() %>%
  mutate(bigram = reorder(bigram, tf_idf)) %>%
  ggplot(aes(bigram, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ book, ncol = 2, scales = "free") +
  coord_flip() +
  labs(y = "tf-idf of bigram to novel",
       x = "")

# Calculamos la frecuencia con la que las palabras van precedidas por la
#   palabra not

bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

# Usaremos AFINN para hacer sentiment analysis de los bigramas
# AFINN daba puntuación de -5 a 5

AFINN <- get_sentiments("afinn")
AFINN

# Examinamos las palabras precedidas más frecuentemente por "not" y que estaban asociadas
#   a un sentimiento

not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)

not_words

# Visualizamos la contribución de las palabras

not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment value * number of occurrences") +
  coord_flip()

# El texto es mucho más positivo de lo que realmente debe ser (por like y help)

# "not" no es la única palabra que invierte el sentido del sentimiento
# Tomamos las 4 palabras de negación más usuales
# Hacemos el join anterior, pero con todas las negation_words

negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE)

# Lo visualizamos

negated_words %>%
  mutate(contribution = n * value,
         word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>%
  group_by(word1) %>%
  top_n(12, abs(contribution)) %>%
  ggplot(aes(word2, contribution, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free") +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  xlab("Words preceded by negation term") +
  ylab("Sentiment value * # of occurrences") +
  coord_flip()


# Vamos a ver las relaciones en forma de grafo

bigram_counts # Recuperamos los conteos originales

bigram_graph <- bigram_counts %>% # Creamos el grafo
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph

# Visualizamos el grafo

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# Hacemos el grafo más bonito para que se entienda mejor

set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


# Vamos a crear una función que automatice el conteo de bigramas y su visualización
#   estas funciones serán `count_bigrams()` y `visualize_bigrams()`

count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

# Nuevo ejemplo: King James Version of the Bible
# Es el libro 10 del Proyecto Gutenberg

kjv <- gutenberg_download(10)

kjv_bigrams <- kjv %>%
  count_bigrams() # Utilizamos nuestra nueva función

kjv_bigrams %>% 
  filter(n > 40, # Nos deshacemos de las combinaciones raras (<= 40 apariciones)
         !str_detect(word1, "\\d"), # No considera palabras que contengan números
         !str_detect(word2, "\\d")) %>%
  visualize_bigrams()


# Sección 4.2 -------------------------------------------------------------

# Consideramos el libro Pride & Prejudice dividido cada 10 líneas

austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

austen_section_words

# Contamos palabras comunes de palabras co-apareciendo en la misma sección

word_pairs <- austen_section_words %>%
  pairwise_count(word, section, sort = TRUE)

word_pairs

# Observamos las palabras que más aparecen con Darcy

word_pairs %>%
  filter(item1 == "darcy")

# Calculamos correlación entre pares de palabras
# Lo haremos con el phi coeficient (muy utilizado en correlación binaria)
# Phi coeficient es el equivalente a Coeficiente Pearson, pero con datos binarios

word_cors <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors

# Buscamos las palabras más correlacionadas con "pounds"

word_cors %>%
  filter(item1 == "pounds")

# Visualizamos las 6 palabras más correlacionadas a "elizabeth", "pounds", "married" y "pride"
word_cors %>%
  filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

# Usamos grafos para visualizar correlaciones

set.seed(2016)

word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

