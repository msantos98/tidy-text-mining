temas = c("01-tidy-text.Rmd",
          "02-sentiment-analysis.Rmd",
          "03-tf-idf.Rmd",
          "04-word-combinations.Rmd",
          "05-document-term-matrices.Rmd",
          "06-topic-models.Rmd",
          "07-tweet-archives.Rmd",
          "08-nasa-metadata.Rmd",
          "09-usenet.Rmd",
          "10-references.Rmd"
          )

library(knitr)
for(i in temas){
  purl(i)
}

