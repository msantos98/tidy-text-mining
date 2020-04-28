# Chunk inicial -----------------------------------------------------------

library(knitr)
library(ggplot2)
library(tidytext)
library(janeaustenr)
library(dplyr)
library(stringr)
library(tidyr)
library(RColorBrewer)
library(wordcloud)
library(reshape2)
theme_set(theme_light())



# Sección 2.0 -------------------------------------------------------------

# Esquema sobre como tratar contenido emocional

knitr::include_graphics("images/tidyflow-ch-2.png")

# *Esta imagen muestra un esquema de cómo implementar sentiment analysis usando principios de tidy data


# Sección 2.1 -------------------------------------------------------------

# Tenemos 3 opciones:
# * AFINN (scores between -5 and 5)
# * bing (positive / negative)
# * nrc (positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise and trust)

# get_sentiments("afinn")
load("data/afinn.rda")
afinn # Otra forma de cargar los datos

get_sentiments("bing")

load("data/nrc.rda")
nrc


# Sección 2.2 -------------------------------------------------------------

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

# Filtramos por el sentimiento joy

nrc_joy <- nrc %>%
  filter(sentiment == "joy")

# Llevamos a cabo sentiment analysis

tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

# Separamos sentimientos positivo y negativo en 2 columnas
# También contamos cuantas palabras positivas y negativas hay en secciones de 80 líneas (lo que hemos llamado index)

jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# Representamos gráficamente

ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")


# Sección 2.3 -------------------------------------------------------------

# Trabajamos ahora con Pride & Prejudice

pride_prejudice <- tidy_books %>%
  filter(book == "Pride & Prejudice")

pride_prejudice

# Volvemos a considerar index = 80 líneas

# Con AFINN

afinn <- pride_prejudice %>%
  inner_join(afinn) %>%
  group_by(index = linenumber %/% 80) %>%
  summarise(sentiment = sum(value)) %>%
  mutate(method = "AFINN")

# Con bing y nrc

bing_and_nrc <- bind_rows(pride_prejudice %>%
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          pride_prejudice %>%
                            inner_join(nrc %>%
                                         filter(sentiment %in% c("positive",
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

# Juntamos todo y representamos gráficamente

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")

# Veamos cuántas palabras positivas y cuántas negativas hay en los lexicons

nrc %>%
     filter(sentiment %in% c("positive",
                             "negative")) %>%
  count(sentiment)

get_sentiments("bing") %>%
  count(sentiment)


# Sección 2.4 -------------------------------------------------------------

# Calculamos cuánto contribuye cada palabra a cada sentimiento

bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

# Lo representamos gráficamente

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

# Eliminamos anomalías y las añadimos a las stopwords customizadas

custom_stop_words <- bind_rows(tibble(word = c("miss"),  lexicon = c("custom")),
                               stop_words)

custom_stop_words


# Sección 2.5 -------------------------------------------------------------

# Hacemos una nube de palabras

tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# Diferenciamos sentimientos positivo y negativo

tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)


# Sección 2.6 -------------------------------------------------------------

# Tokenizamos por frases

PandP_sentences <- tibble(text = prideprejudice) %>%
  unnest_tokens(sentence, text, token = "sentences")

# Observamos una de ellas:

PandP_sentences$sentence[2]

# Tokenizamos por capítulos

austen_chapters <- austen_books() %>%
  group_by(book) %>%
  unnest_tokens(chapter, text, token = "regex", # Usamos un regex pattern
                pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
  ungroup()

austen_chapters %>%
  group_by(book) %>%
  summarise(chapters = n())

# Buscamos los capítulos más negativos de cada una de las novelas de Jane Austen
# Utilizamos bing
# Normalizamos los capítulos por número de palabras

bingnegative <- get_sentiments("bing") %>%
  filter(sentiment == "negative")

wordcounts <- tidy_books %>%
  group_by(book, chapter) %>%
  summarize(words = n())

tidy_books %>%
  semi_join(bingnegative) %>%
  group_by(book, chapter) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("book", "chapter")) %>%
  mutate(ratio = negativewords/words) %>%
  filter(chapter != 0) %>%
  top_n(1) %>%
  ungroup()

