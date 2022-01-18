#clear workspace
rm(list=ls())


#~ ##################################################################### 200618
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
packageVersion("wordcloud2")

#load data
fp = "/home/sol-nhl/dev/bash/text-csv/csv/home-exam.csv"
docs_df = read.csv(fp, header=T, sep="\t", quote='', strip.white=TRUE, stringsAsFactors=FALSE, encoding="UTF-8")
#docs_df = read.csv(url("http://sgsdm.isk.lu.se/scom/org-emo.csv"), header=T, sep="\t", strip.white=TRUE, stringsAsFactors=FALSE, encoding="UTF-8")

#clean
#docs_df = docs_df %>%
#mutate(text=str_replace_all(text, "[^[:alnum:]]", " "))

#group concat sentences by subject and page
docs_df = docs_df %>%
mutate(subj_page=paste(subj, page, sep="-"))
#
docs_df = docs_df %>%
group_by(subj_page) %>% 
#mutate(docs=paste0(text, collapse=" ")) %>%
summarise(docs=paste0(text, collapse=" "))
#ungroup()
#filter out short documents
docs_df = docs_df %>%
filter(nchar(docs)>256)
#mutate(legth=nchar(docs))
#corpus
docs_corp = corpus(docs_df, text_field="docs")
#reshape, to=c("sentences", "paragraphs", "documents")
docs_corp = corpus_reshape(docs_corp, to=c("sentences"), use_docvars=TRUE)
#and back..
docs_corp = corpus_reshape(docs_corp, to=c("documents"), use_docvars=TRUE)
#tokenize
docs_toks = tokens(docs_corp, 
what = "word",
remove_punct = TRUE,
remove_symbols = TRUE,
remove_numbers = TRUE,
remove_url = TRUE,
remove_separators = TRUE)
#
docs_toks = tokens_tolower(docs_toks, keep_acronyms = FALSE)
#stopwords
docs_toks <- tokens_select(docs_toks, pattern=stopwords::stopwords('sv'), selection='remove')
#lemmatize
docs_toks = tokens_wordstem(docs_toks, language="sv")
#keywords in context, across all docs
head(kwic(docs_toks, pattern='psy*'))
#subset tokens
tmp = tokens_subset(docs_toks, subj_page=="alquieremma_83788_462914-1")
#keywords in context, specific subject
head(kwic(tmp, pattern='samsk*'))




#~ ##################################################################### 200616
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
packageVersion("wordcloud2")

#load data
fp = "/home/sol-nhl/dev/bash/text-csv/csv/home-exam.csv"
docs_df = read.csv(fp, header=T, sep="\t", quote='', strip.white=TRUE, stringsAsFactors=FALSE, encoding="UTF-8")
#socm_df = read.csv(url("http://sgsdm.isk.lu.se/scom/org-emo.csv"), header=T, sep="\t", strip.white=TRUE, stringsAsFactors=FALSE, encoding="UTF-8")

#clean
#docs_df = socm_df %>%
#mutate(text=str_replace_all(text, "[^[:alnum:]]", " "))

#group concat sentences by subject and page
tmp = docs_df %>%
mutate(subj_page=paste(subj, page, sep="-"))
#
tmp = tmp %>%
group_by(subj_page) %>% 
#mutate(docs=paste0(text, collapse=" ")) %>%
summarise(docs=paste0(text, collapse=" "))
#ungroup()
#filter out short documents
tmp = tmp %>%
filter(nchar(docs)>256)
#mutate(legth=nchar(docs))
#corpus
tmp = corpus(tmp, text_field="docs")
#reshape, to=c("sentences", "paragraphs", "documents")
tmp = corpus_reshape(tmp, to=c("sentences"), use_docvars=TRUE)
#and back
tmp = corpus_reshape(tmp, to=c("documents"), use_docvars=TRUE)
#tokenize
docs_toks = tokens(tmp, 
what = "word",
remove_punct = TRUE,
remove_symbols = TRUE,
remove_numbers = TRUE,
remove_url = TRUE,
remove_separators = TRUE)
#
docs_toks = tokens_tolower(docs_toks, keep_acronyms = FALSE)
#stopwords
docs_toks <- tokens_select(docs_toks, pattern=stopwords::stopwords('sv'), selection='remove')
#lemmatize
docs_toks = tokens_wordstem(docs_toks, language="sv")
#keywords in context
head(kwic(docs_toks, pattern='psy*'))



#corpus
docs_corp <- corpus(docs_df, text_field="text")
ndoc(docs_corp)
#docnames(socm_corp) <- paste(socm_df$type, 1:nrow(socm_df), sep="_")
print(docs_corp)
head(docvars(docs_corp))
#docvars(socm_corp, field="rownum") <- 1:nrow(docvars(socm_corp))
#change unit of texts
#corp_sent <- corpus_reshape(corp, to = 'sentences')
#subset
socm_corp <- corpus_subset(socm_corp, lang=="sv")

#tokenize, lemmatize
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
socm_dfm %>% 
textstat_frequency(n = 15) %>% 
ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
geom_point() +
coord_flip() +
labs(x = NULL, y = "frequency") +
theme_minimal()
#
set.seed(132)
textplot_wordcloud(socm_dfm, max_words=200)


df = as.data.frame(socm_freq[,1:2])

#library(wordcloud)
wordcloud(words=df$feature, freq=df$frequency, min.freq=1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"), scale=c(4.0, 0.5))

#library(wordcloud2)
wordcloud2(data=df, size=1.0, minSize=0.1, color='random-dark')
wordcloud2(data=df, size=1.0, minSize=0.1, shape = 'pentagon')
