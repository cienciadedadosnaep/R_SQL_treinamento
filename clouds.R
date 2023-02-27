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


library(readr)
palavrasseeufossecientista_v00 <- read_csv("data/palavrasseeufossecientista_v00.txt")


d <- palavrasseeufossecientista_v00


set.seed(1)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.33, 
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
  geom_treemap_text() +
  labs(caption = "Fonte de dados: SIDRA/PNAD 2010")+
  ggtitle("Distribuição (%) raça/cor - Salvador BA")+
  theme(axis.text.x=element_text(size=16, angle=0, vjust=.8, hjust=0.8)) +
  theme(axis.title.y = element_text(color = "black",size = 16))+
  theme(axis.title.x = element_text(color = "black",size = 16))+
  theme(axis.text.y=element_text(size=16)) +
  theme(axis.text = element_text(size = 16))  +
  theme(legend.text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 16)) 
#  theme(legend.position = "none") 



