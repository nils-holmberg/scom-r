library(quanteda)
library(stopwords)
#library(readtext)
library(tm)
library(tidyverse)
library(tidytext)
library(stringr)
#library(shiny)
library(wordcloud)
library(wordcloud2)

get_cloud <- function(socm_df) {
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

#tokenize
socm_toks = tokens(socm_corp, 
what = "word",
remove_punct = TRUE,
remove_symbols = TRUE,
remove_numbers = TRUE,
remove_url = TRUE,
remove_separators = TRUE)
#head(kwic(socm_toks, pattern='psy*'))
socm_toks <- tokens_select(socm_toks, pattern=stopwords('en'), selection='remove')
socm_toks <- tokens_select(socm_toks, pattern=stopwords::stopwords('sv'), selection='remove')
#toks_ngram <- tokens_ngrams(toks, n = 2:4)
socm_toks = tokens_wordstem(socm_toks, language = quanteda_options("language_stemmer"))
socm_toks = tokens_wordstem(socm_toks, language = "sv")
#stemDocument(kompis, language="swedish")

#document feature matrix
socm_dfm = dfm(socm_toks, remove_punct=TRUE)
ndoc(socm_dfm); nfeat(socm_dfm)
#socm_dfm = dfm_select(socm_dfm, pattern=stopwords::stopwords('en'), selection='remove')
socm_dfm = dfm_select(socm_dfm, min_nchar=5, selection='remove')
socm_dfm = dfm_trim(socm_dfm, min_termfreq=10, selection='remove')
socm_dfm = dfm_trim(socm_dfm, max_docfreq=0.1, docfreq_type="prop", selection='remove')
ndoc(socm_dfm); nfeat(socm_dfm)
#dfm_wordstem(x, language = quanteda_options("language_stemmer"))
#dfm_wordstem(x, language = "sv")

#word frequency
socm_freq = textstat_frequency(socm_dfm)#, n = 5, groups = "lang")
#
#socm_dfm %>% 
#textstat_frequency(n = 15) %>% 
#ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
#geom_point() +
#coord_flip() +
#labs(x = NULL, y = "frequency") +
#theme_minimal()
#
#set.seed(132)
#textplot_wordcloud(socm_dfm, max_words=200)

df = as.data.frame(socm_freq[,1:2])
return(df)
}


