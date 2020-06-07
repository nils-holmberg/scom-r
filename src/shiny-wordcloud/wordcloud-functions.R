library(quanteda)
library(stopwords)
#library(readtext)
library(tm)
library(tidyverse)
library(tidytext)
library(stringr)
library(wordcloud)
library(wordcloud2)

some_test <- function(socm_df) {
#clean
socm_df = socm_df %>%
mutate(text=str_replace_all(text, "[^[:alnum:]]", " "))

#corpus
socm_corp <- corpus(socm_df, text_field="text")
ndoc(socm_corp)
#docnames(socm_corp) <- paste(socm_df$type, 1:nrow(socm_df), sep="_")
print(socm_corp)
head(docvars(socm_corp))
#docvars(socm_corp, field="rownum") <- 1:nrow(docvars(socm_corp))
#change unit of texts
#corp_sent <- corpus_reshape(corp, to = 'sentences')
#subset
socm_corp <- corpus_subset(socm_corp, lang=="sv")

}


