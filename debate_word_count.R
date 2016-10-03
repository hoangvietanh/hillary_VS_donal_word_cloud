# install.packages("tm")
# install.packages("SnowballC")
# install.packages("wordcloud")
# install.packages("RColorBrewer")

# load library
library(tm)
library(SnowballC)
library(wordcloud)
library("RColorBrewer")

#get data
setwd("c:/debate/")
donalpath="c:/debate/donal.txt"
hillarypatch="c:/debate/hillary.txt"
donal = readLines(donalpath)
hillary =readLines(hillarypatch)

#donal from here
#clean data, word to lower case, remove puntuation, remove stop words, remove whitespace, stem document
donal <- Corpus(VectorSource(donal))
donal <- tm_map(donal, PlainTextDocument)
donal <- tm_map(donal, content_transformer(tolower))
donal <- tm_map(donal, removePunctuation)
donal <- tm_map(donal, removeWords, stopwords("english"))
donal <- tm_map(donal, removeWords, c("the", "this","and","but"))
donal <- tm_map(donal, stripWhitespace)
donal <- tm_map(donal, stemDocument)

#build frequency
dtm.donal <- TermDocumentMatrix(donal)
m.donal <- as.matrix(dtm.donal)
v.donal <- sort(rowSums(m.donal),decreasing=TRUE)
d.donal <- data.frame(word = names(v.donal),freq=v.donal)

#words cloud
set.seed(1234)
wordcloud(words = d.donal$word, freq = d.donal$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#bar plot
barplot(d.donal[1:10,]$freq, las = 2, names.arg = d.donal[1:10,]$word,
         col ="lightblue", main ="Donal Trump most frequent words",
         ylab = "Word frequencies")

#hillary from here
hillary <- Corpus(VectorSource(hillary))
hillary <- tm_map(hillary, PlainTextDocument)
hillary <- tm_map(hillary, content_transformer(tolower))
hillary <- tm_map(hillary, removePunctuation)
hillary <- tm_map(hillary, removeWords, stopwords("english"))
hillary <- tm_map(hillary, removeWords, c("the", "this","and","but"))
hillary <- tm_map(hillary, stripWhitespace)
hillary <- tm_map(hillary, stemDocument)

dtm.hillary <- TermDocumentMatrix(hillary)
m.hillary <- as.matrix(dtm.hillary)
v.hillary <- sort(rowSums(m.hillary),decreasing=TRUE)
d.hillary <- data.frame(word = names(v.hillary),freq=v.hillary)

set.seed(1234)
wordcloud(words = d.hillary$word, freq = d.hillary$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


barplot(d.hillary[1:10,]$freq, las = 2, names.arg = d.hillary[1:10,]$word,
        col ="lightblue", main ="Hillary Cliton most frequent words",
        ylab = "Word frequencies")

#hillary word count 5085
# Donal word count 8340
