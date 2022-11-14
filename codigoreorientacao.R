#00 - biblioteca

library(readr)

library(dplyr)

#01 - Leitura Banco de Dados


# variaveis que correpondem a datas
datas <- read_csv("data/datas.csv", col_types = cols(data = col_datetime(format = "%d/%m/%Y ")))
# dados da avaliacao da reorientacao de CD 
dadoscd2022 <- read_csv("data/dadoscd2022.csv")
# dados da avaliacao anual de PI
dadospi2022 <- read_csv("data/dadospi2022.csv")

dadospicd <- full_join(dadoscd2022,dadospi2022,"ID")


## Qual e a situacao do IDEB das escolas parceiras e da cidade

colegiosp_siglas <- c("CEHMC","CEY","CSB","CEMCN",
                      "CEEV","CEMATF","CEES","CEMD",
                      "CEMN","CENTRAL","CMCJ","IFBA")

codigos_inep_parceiras <- c(29192790,29182182,29181550,29193699,
                            29182158,29185394,29180490,29181500,
                            29182131,29182123,29190061,29196442)

dfesc_parc <- data.frame(codigos_inep_parceiras,colegiosp_siglas)


idebssa %>% filter(id_escola %in% dfesc_parc$codigos_inep_parceiras) %>%
            filter(ano %in% c(2015)) %>%
            select(id_escola,ideb)


## Mapa das escolas com respectivos IDEBs 

##  Notas e frequencia
library(ggplot2)
library(bbplot)
dadoscd2022 %>% 
  mutate(qualitativog1 = factor(qualitativog1,levels=c( 
                            "INSATISFATORIO","REGULAR",
                            "BOM","MUITO BOM","OTIMO"))) %>%
  filter(grupo %in% c('G1')) %>% 
  filter(quantitativog1>0) %>%
#  select(freq_perc,`%p`) %>% 
  ggplot(aes(x=grupo,y = quantitativog1,fill=grupo))+
  geom_jitter(aes(colour=qualitativog1,size=freq_perc,fill=grupo),width=0.1)+
  geom_boxplot(width=0.2,alpha=0.2)
#  xlim(0, 2)+
#  bbc_style()
  ggsave('figuras/boxplotg1g2.png')

  
  ## Mapa das escolas com respectivos IDEBs 
  
  ##  Notas e frequencia
  library(ggplot2)
  library(bbplot)
  dadoscd2022 %>% 
    mutate(qualitativog1 = factor(qualitativog1,levels=c( 
      "INSATISFATORIO","REGULAR",
      "BOM","MUITO BOM","OTIMO"))) %>%
    filter(grupo %in% c('G1')) %>% 
    filter(quantitativog1>0) %>%
    #  select(freq_perc,`%p`) %>% 
    ggplot(aes(x=grupo,y = quantitativog1,fill=grupo))+
    geom_jitter(aes(colour=qualitativog1,size=freq_perc,fill=grupo),width=0.1)+
    geom_boxplot(width=0.2,alpha=0.2)
  #  xlim(0, 2)+
  #  bbc_style()
  ggsave('figuras/boxplotg1g2.png')
  
  
  
  
    
  ##  Notas e frequencia
  library(ggplot2)
  library(bbplot)
  dadoscd2022 %>% 
    mutate(qualitativog2 = factor(qualitativog2,levels=c( 
      "INSATISFATORIO","REGULAR",
      "BOM","MUITO BOM","OTIMO"))) %>%
    filter(grupo %in% c('G1','G2')) %>% 
    filter(quantitativog2>0) %>%
    #  select(freq_perc,`%p`) %>% 
    ggplot(aes(x=grupo,y = quantitativog2,fill=grupo))+
    geom_jitter(aes(colour=qualitativog2,size=freq_perc,fill=grupo),width=0.1)+
    geom_boxplot(width=0.2,alpha=0.2)
  #  xlim(0, 2)+
  #  bbc_style()
  ggsave('figuras/boxplotg1g2.png')  
##
  
  dadoscd2022 %>% 
    mutate(qualitativog1 = factor(qualitativog1,levels=c( 
      "INSATISFATORIO","REGULAR",
      "BOM","MUITO BOM","OTIMO"))) %>%
    filter(grupo %in% c('G2')) %>% 
    filter(quantitativog1>0) %>% 
    select(nome,ID,quantitativog1)
  
  ## Numeros gerais do relatorio
  
  ### Total de estudantes do G1 e G2
  #### inicio de 2022
  dadoscd2022 %>% select(grupo) %>% 
    group_by(grupo) %>% 
    summarise(n_part =n()) 
  #### final de 2022
  dadospi2022 %>% select(grupo) %>% 
    group_by(grupo) %>% 
    summarise(n_part =n())
  
  ### Estudante do G2 com frequência inferior a 75%
  
  dadoscd2022 %>% filter(grupo %in% c('G2')) %>%
    filter(freq_perc < c(70)) %>%
    select(grupo,nome,ID)
  
  #      1 G2    Henryque de Jesus dos Santos A011 
  # motivos Faltas devido assalto, ficou sem celular
  # machucou o pé
  
  
  
  
  
  
