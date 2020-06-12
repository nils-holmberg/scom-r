
#https://www.r-bloggers.com/word-clouds-for-management-presentations-a-workflow-with-r-quanteda/

#~ ##################################################################### 200608
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

#load data
fp = "csv/org-emo.csv"
#socm_df = read.csv(fp, header=T, sep="\t", strip.white=TRUE, stringsAsFactors=FALSE, encoding="UTF-8")
socm_df = read.csv(url("http://sgsdm.isk.lu.se/scom/org-emo.csv"), header=T, sep="\t", strip.white=TRUE, stringsAsFactors=FALSE, encoding="UTF-8")

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

#~ ##################################################################### 200604
#install.packages("shiny", lib="~/lib/r-cran")
#https://shiny.rstudio.com/
#sgsdm.isk.lu.se:3838/scom-r/src/shiny-kmeans/
library(shiny)
#runExample("09_upload"

#clear workspace
rm(list=ls())
#import dataset, 2000 cases
df = read.csv('csv/org-emo.csv', header=T, sep="\t", strip.white=TRUE, stringsAsFactors=FALSE)
#load functions
source("src/shiny-wordcloud/app.R")
source("src/shiny-wordcloud/wordcloud-functions.R")
#
#source("/tmp/scom/shiny-examples/066-upload-file/server.R")
#source("/tmp/scom/shiny-examples/066-upload-file/ui.R")

#df = get_cloud(df)

#ui <- fluidPage()
#server <- function(input, output) {}
shinyApp(ui=ui, server=server)


#~ ##################################################################### 200604
#wordclouds, social media
#install.packages(c("wordcloud", "wordcloud2", "RColorBrewer"), lib="~/lib/r-cran")
library(quanteda)
library(stopwords)
#library(readtext)
library(tm)
library(tidyverse)
library(tidytext)
library(stringr)

#load data
fp = "csv/org-emo.csv"
#socm_df = read.csv(fp, header=T, sep="\t", strip.white=TRUE, stringsAsFactors=FALSE)
socm_df = read.csv(url("http://sgsdm.isk.lu.se/scom/org-emo.csv"), header=T, sep="\t", strip.white=TRUE, stringsAsFactors=FALSE, encoding="UTF-8")

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

library(wordcloud)
wordcloud(words=df$feature, freq=df$frequency, min.freq=1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"), scale=c(4.0, 0.5))

library(wordcloud2)
wordcloud2(data=df, size=1.0, minSize=0.1, color='random-dark')
wordcloud2(data=df, size=1.0, minSize=0.1, shape = 'pentagon')


#~ ##################################################################### 200602
#quanteda workflow
#install.packages("stopwords", lib="~/lib/r-cran")
#https://shiny.rstudio.com/

library(quanteda)
library(stopwords)
#library(readtext)
library(tm)
library(tidyverse)
library(tidytext)
library(stringr)
library(pins)
board_register("kaggle", token="~/doc/env/kaggle/kaggle.json")
#pin(iris, description="some-test", board="kaggle")
#load, explore
#paths <- pins::pin_get("nltkdata/movie-review", "kaggle")
#paths <- pins::pin_get("nilsholmberg/scom-socm", "kaggle")
#read as tibble
paths <- pins::pin_get("datasnaek/mbti-type", "kaggle")
if(F) {
mbti_dt <- readr::read_csv(paths[1])
#cols, doc id
mbti_dt <- mbti_dt %>% rename(mbti=type, text=posts)
mbti_dt <- mutate(mbti_dt, id=1:nrow(mbti_dt))
mbti_dt <- mbti_dt %>%
mutate(id=paste0(mbti, "_", id))
}
#paths[1]
fp = "/home/sol-nhl/.cache/pins/kaggle/datasnaek/mbti-type/mbti_1.csv"
mbti_df = read.csv(fp, header=T, strip.white=TRUE, stringsAsFactors=FALSE)
mbti_corp <- corpus(mbti_df, text_field="posts")
ndoc(mbti_corp)
docid <- paste(mbti_df$type, 1:nrow(mbti_df), sep="_")
docnames(mbti_corp) <- docid
print(mbti_corp)
head(docvars(mbti_corp))
docvars(mbti_corp, field="rownum") <- 1:nrow(docvars(mbti_corp))
#change unit of texts
#corp_sent <- corpus_reshape(corp, to = 'sentences')
#print(corp_sent)

#tokenize
mbti_toks = tokens(mbti_corp, remove_punct=TRUE)
head(kwic(mbti_toks, pattern='psy*'))
mbti_toks <- tokens_select(mbti_toks, pattern=stopwords('en'), selection='remove')
mbti_toks <- tokens_select(mbti_toks, pattern=stopwords::stopwords('sv'), selection='remove')
#toks_ngram <- tokens_ngrams(toks, n = 2:4)
mbti_toks = tokens_wordstem(mbti_toks, language = quanteda_options("language_stemmer"))

#document feature matrix
mbti_dfm = dfm(mbti_toks, remove_punct=TRUE)
ndoc(mbti_dfm); nfeat(mbti_dfm)
mbti_dfm = dfm_select(mbti_dfm, pattern=stopwords::stopwords('en'), selection='remove')
mbti_dfm = dfm_select(mbti_dfm, min_nchar=5, selection='remove')
mbti_dfm = dfm_trim(mbti_dfm, min_termfreq=10, selection='remove')
mbti_dfm = dfm_trim(mbti_dfm, max_docfreq=0.1, docfreq_type="prop", selection='remove')
ndoc(mbti_dfm); nfeat(mbti_dfm)
dfm_wordstem(x, language = quanteda_options("language_stemmer"))


#frequency
mbti_freq = textstat_frequency(mbti_dfm)#, n = 5, groups = "lang")
#
mbti_dfm %>% 
textstat_frequency(n = 15) %>% 
ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
geom_point() +
coord_flip() +
labs(x = NULL, y = "frequency") +
theme_minimal()
#
set.seed(132)
textplot_wordcloud(mbti_dfm, max_words=100)


set.seed(1234)
df = as.data.frame(mbti_freq[1:200,1:2])
df = as.data.frame(mbti_freq[seq(1,2000,20),1:2])

library(wordcloud)
wordcloud(words=df$feature, freq=df$frequency, min.freq=1, max.words=100, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"), scale=c(3, 0.5))

library(wordcloud2)
wordcloud2(data=df, size=1, minSize=0.1, color='random-dark')
wordcloud2(data=df, size = 0.7, shape = 'pentagon')

wordcloud2(data=df, size = 1, minSize = 0, gridSize =  0,
fontFamily = 'Segoe UI', fontWeight = 'bold',
color = 'random-dark', backgroundColor = "cyan",
minRotation = -pi/4, maxRotation = pi/4, shuffle = TRUE,
rotateRatio = 0.4, shape = 'circle', ellipticity = 0.65,
widgetsize = NULL, figPath = NULL, hoverFunction = NULL)
