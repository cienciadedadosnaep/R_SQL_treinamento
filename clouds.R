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


d <- palavrasseeufossecientista_v00


set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


tabelassa <- get_sidra(136,geo = 'City',geo.filter = '2927408')


# install.packages("treemapify")
library(treemapify)
# install.packages("ggplot2")
library(ggplot2)
library(dplyr)



tabelassa %>% 
  filter(`Cor ou raça` != c('Total')) %>%
  summarise( `Cor ou raça`,Valor = round(Valor*100/sum(Valor),1)) %>%
  ggplot( aes(area = Valor, fill =`Cor ou raça` , label = Valor),
          color=brewer.pal(6, "Dark2")) +
  geom_treemap() +
  geom_treemap_text()


