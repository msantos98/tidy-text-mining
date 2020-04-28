# Chunk inicial -----------------------------------------------------------

library(knitr)
library(ggplot2)
library(dplyr)
library(janeaustenr)
library(tidytext)
library(gutenbergr)
library(forcats)
library(stringr)
theme_set(theme_light())


# Sección 3.0 -------------------------------------------------------------

# Term frequency (tf): Cómo de frecuente es una palabra en un documento
# Inverse document frequency (idf): Disminuye el peso para palabras utilizadas comunmente
#   y aumenta el peso de palabras no muy utilizadas en una colección de documentos
# tf-idf: Frecuencia ajustada de un término por cómo de raro es utilizarlo. Se utiliza para medir cómo de importante
#   es una palabra para un documento de una colección.

# Calcular tf-idf intenta encontrar las palabras que son importante (comunes) en un texto, pero no muy comunes en general
# El idf y, con ello, el tf-idf es muy bajo (cercano a 0) para palabras que ocurren en muchos documentos de una
#   colección. Así es como el ajuste disminuye el peso de palabras muy comunes.
# El idf y, con ello, el tf-idf tomará un valor mayor para palabras que ocurren en menos de los documentos de la colección



# Sección 3.1 -------------------------------------------------------------

# ¿Cuáles son las palabras más utilizadas en las novelas de Jane Austen?

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)

# Calculamos el número total de palabaras por libro

total_words <- book_words %>%
  group_by(book) %>%
  summarize(total = sum(n))

# Unimos ambos dataframes

book_words <- left_join(book_words, total_words)

book_words

# Vemos la distribución de n/total (la term frequency) para cada libro
# Cada uno de los facets tiene una distribución similar, con muchas palabras que
#   ocurren raramente y pocas que ocurren frecuentemente

ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")


# Sección 3.2 -------------------------------------------------------------

# Zip's Law: La frecuencia con la que una palabra aparece es inversamente proporcional a su ranking

# Examinamos la Zip's Law para las novelas de Jane Austen

freq_by_rank <- book_words %>%
  group_by(book) %>%
  mutate(rank = row_number(),
         term_frequency = n/total)

freq_by_rank

# Visualizamos la Zip's Law

freq_by_rank %>%
  ggplot(aes(rank, term_frequency, color = book)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

# Veamos el exponente de la regresión potencial

rank_subset <- freq_by_rank %>%
  filter(rank < 500,
         rank > 10)

lm(log10(term_frequency) ~ log10(rank), data = rank_subset)

# La pendiente se aproxima a -1

# Añadamos el modelo ajustado de la regresión potencial

freq_by_rank %>%
  ggplot(aes(rank, term_frequency, color = book)) +
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

# Jane Austen utiliza un porcentaje más bajo de las palabras más comunes que muchas colecciones de lenguaje


# Sección 3.3 -------------------------------------------------------------

# Para usar la función bind_tf_idf necesitamos la columna de términos/tokens (en nuestro caso, palabras),
#   una columna de documentos (libros en nuestro ejemplo) y una columna de la frecuencia del término por
#   documento (en nuestro caso, a dicha columna la hemos denominado n)

book_words <- book_words %>%
  bind_tf_idf(word, book, n)

book_words

# Vemos los términos con un mayor tf-idf

book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# Algunos de los valores de idf son los mismos para diferentes términos porque hay 6 documentos en este corpus
#   y estamos viendo el valor numérico para ln(6/1), ln(6/2)...

# Visualizamos los términos con un mayor tf-idf por libro
# Estas son las 15 palabras más importantes para cada novela (todo nombres propios)

book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(book) %>%
  top_n(15) %>%
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()

# Medir tf-idf nos ha mostrado que Jane Austen ha utilizado un lenguaje similar en sus 6 novelas y que
#   lo que distingue a cada novela del resto de la colección son los nombres propios, los nombres de las personas
#   los lugares.


# Sección 3.4 -------------------------------------------------------------

# Cambiamos de ejemplo: Libros clásicos de física

# Cargamos los libros:  Discourse on Floating Bodies by Galileo Galilei, Treatise on Light by Christiaan Huygens,
#   Experiments with Alternate Currents of High Potential and High Frequency by Nikola Tesla y Relativity: The
#   Special and General Theory by Albert Einstein.

physics <- gutenberg_download(c(37729, 14725, 13476, 30155),
                              meta_fields = "author")

# Tokenizamos y contamos cuántas veces aparece cada palabra en cada libro

physics_words <- physics %>%
  unnest_tokens(word, text) %>%
  count(author, word, sort = TRUE)

physics_words

# Como los libros tienen diferentes tamaños (en cuanto a número de palabras), hacemos tf-idf

plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  mutate(word = fct_reorder(word, tf_idf)) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))
# Representamos el top 15

plot_physics %>%
  group_by(author) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()

# Nos fijamos en la legra k que aparece en el texto de Einstein varias veces
# También está el problema de co y ordinate/s
# En el lubro de Huygens, está el problema de los nombres de figuras matemáticas (AB, RC...)

physics %>%
  filter(str_detect(text, "_k_")) %>% # Vemos en qué líneas aparece esa k de la que hablábamos
  select(text)

physics %>%
  filter(str_detect(text, "RC")) %>%
  select(text)


# Hacemos una mejor limpieza para obtener un gráfico más signficativo

mystopwords <- tibble(word = c("eq", "co", "rc", "ac", "ak", "bn",
                               "fig", "file", "cg", "cb", "cm",
                               "ab", "_k", "_k_", "_x"))

physics_words <- anti_join(physics_words, mystopwords,
                           by = "word")

plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  mutate(word = str_remove_all(word, "_")) %>%
  group_by(author) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, author)) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo",
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))

ggplot(plot_physics, aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip() +
  scale_x_reordered()

