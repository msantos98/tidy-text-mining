# Chunk inicial -----------------------------------------------------------

library(knitr)
library(ggplot2)
library(methods)
library(scales)
theme_set(theme_light())
library(topicmodels)
library(tidytext)
library(dplyr)
library(tidyr)
library(gutenbergr)
library(stringr)
library(scales)



# Sección 6.1 -------------------------------------------------------------

data("AssociatedPress")
AssociatedPress

# Creamos un modelo LDA de 2 topics:
# Especificamos una semilla para que los datos sean predecibles

ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda

# Extraemos la probabilidad de palabra por topic, que es la beta del modelo

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

# Buscamos los 10 términos más comunes para cada topic

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

# Vamos a considerar ahora los términos con mayor diferencia en beta entre el topic 1 y el 2
# Lo haremos basándonos en el logaritmo en base 2 de beta2 / beta 1 (así, la diferencia es simétrica)
# Nos quedamos con aquellos términos que tienen una probabilidad mayor a 0.001 en alguno de los topics

beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread

# Representamos las 10 palabras con las mayores diferencias

beta_spread %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  labs(y = "Log2 ratio of beta in topic 2 / topic 1") +
  coord_flip()


# Vamos ahora a calcular las probabilidades de tema por documento, que es la gamma del modelo LDA

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

# Vamos a estudiar el documento 6 para ver que la mayoría de sus palabras pertenecen al topic 2 (political / national news)

tidy(AssociatedPress) %>%
  filter(document == 6) %>%
  arrange(desc(count))



# Sección 6.2 -------------------------------------------------------------

# Suponiendo que los siguientes 4 libros han sido mezclados y con los capítulos se ha hecho un solo y enorme montón
#   vamos a usar topic models para devolver cada capítulo a su respectivo libro
# Obtenemos los libros del Proyecto Gutemberg

titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
            "Pride and Prejudice", "Great Expectations")

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

# Tratamos cada capítulo como un documento separado

by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

# Tokenizamos por palabras
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

# Contamos las palabras por capítulo y quitamos las stop words
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()

word_counts

# Convertimos a document-term matrix. Lo necesitamos para poder hacer LDA
chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)

chapters_dtm

# Hacemos un LDA con 4 topics

chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))
chapters_lda

# Examinamos las probabilidades de palabras por topic (beta)
chapter_topics <- tidy(chapters_lda, matrix = "beta")
chapter_topics

# Vemos el top 5 de probabilidades de palabras en cada topic

top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

# Visualizamos ese top 5 de cada topic

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()


# Veamos qué topics están asociados a cada capítulo (gamma)

chapters_gamma <- tidy(chapters_lda, matrix = "gamma")
chapters_gamma


# Con esto podemos ver como nuestro modelo ha distinguido los 4 libros

chapters_gamma <- chapters_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)

chapters_gamma

chapters_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title)

# Vemos que para todos los libros, excepto "Great Expectations", estñan asociados a un solo topic. En cuanto a Great Expectations,
#   parece ser que alguno de sus capítulos también se ha asociado al topic 1, aunque en su mayoría, todos son del topic 4

#¿Hay algún caso en el que el tema más asociado con un capítulo perteneciera a otro libro?
# Primero, buscamos el tema más asociado a cada capítulo

chapter_classifications <- chapters_gamma %>%
  group_by(title, chapter) %>%
  top_n(1, gamma) %>%
  ungroup()

chapter_classifications

# Ahora lo comparamos con el topic más asociado a cada libro

book_topics <- chapter_classifications %>%
  count(title, topic) %>%
  group_by(title) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = title, topic)

# Buscamos aquellos capítulos de cada libro que no coinciden con el topic general asociado, respectivamente
chapter_classifications %>%
  inner_join(book_topics, by = "topic") %>%
  filter(title != consensus)

# Vemos qué palabras de qué documentos han sido asignadas a qué topics

assignments <- augment(chapters_lda, data = chapters_dtm)
assignments

# Vemos qué palabras fueron clasificadas de forma incorrecta

assignments <- assignments %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE) %>%
  inner_join(book_topics, by = c(".topic" = "topic"))

assignments


# Mostramos una matriz de confusión

assignments %>%
  count(title, consensus, wt = count) %>%
  group_by(title) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(consensus, title, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "red", label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Book words were assigned to",
       y = "Book words came from",
       fill = "% of assignments")

# Cuales fueron las palabras mal clasificadas?

wrong_words <- assignments %>%
  filter(title != consensus)

wrong_words

wrong_words %>%
  count(title, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))

# Vemos que hay palabras mal clasificadas porque flopson está en el cluster de Pride and Prejudice, pero esta palabra solo
#   aparece en Great Expectations
word_counts %>%
  filter(word == "flopson")



# Sección 6.3 -------------------------------------------------------------

# Trata de otra implementación de LDA. Ver libro en caso de interés, porque ahora, interés, lo que se dice interés, poco o nada

