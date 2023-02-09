install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")

library(tm)
library(SnowballC)
library(wordcloud)

library(readr)



testea <- as.list(palavrasebook) 
auxCorpus <- Corpus(VectorSource(testea))
auxCorpus <- tm_map(auxCorpus, PlainTextDocument)

auxCorpus <- tm_map(auxCorpus, removePunctuation) 

auxCorpus <- tm_map(auxCorpus, removeWords, stopwords('pt')) 

#auxCorpus <- tm_map(auxCorpus, stemDocument)

wordcloud(auxCorpus$content,max.words=500,colors=c("blue","red"))