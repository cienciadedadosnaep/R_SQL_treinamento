

library(ggplot2)
library(bbplot)

  
#01 - Leitura Banco de Dados
  
  
# Variaveis que correpondem a datas
  datas <- read_csv("data/datas.csv", 
                    col_types = cols(data = col_datetime(format = "%d/%m/%Y ")))
# Dados da avaliacao da reorientacao de CD 
  dadoscd2022 <- read_csv("data/dadoscd2022.csv")
# Dados da avaliacao anual de PI
  dadospi2022 <- read_csv("data/dadospi2022.csv")
# Reuniao das duas tabelas  
  dadospicd <- full_join(dadoscd2022,dadospi2022,"ID")
# Qual e a situacao do IDEB das escolas parceiras e da cidade
  colegiosp_siglas <- c("CEHMC","CEY","CSB","CEMCN",
                        "CEEV","CEMATF","CEES","CEMD",
                        "CEMN","CENTRAL","CMCJ","IFBA")
  codigos_inep_parceiras <- c(29192790,29182182,29181550,29193699,
                              29182158,29185394,29180490,29181500,
                              29182131,29182123,29190061,29196442)
  dfesc_parc <- data.frame(codigos_inep_parceiras,colegiosp_siglas)

  
#Boxplot G1
  
  dadoscd2022 %>% 
    mutate(qualitativog1 = factor(qualitativog1,levels=c("OTIMO","MUITO BOM","BOM",
                                                         "REGULAR","INSATISFATORIO"))) %>%
    filter(grupo %in% c('G1')) %>% 
    filter(quantitativog1>0) %>%
    #  select(freq_perc,`%p`) %>% 
    ggplot(aes(x=grupo,y = quantitativog1,fill=grupo))+
    geom_jitter(aes(colour=qualitativog1,size=freq_perc),width=0.1)+
    geom_boxplot(width=0.2,alpha=0.02) +
    xlab("")+
    ylab("%")+
    labs(caption = "Fonte de dados: Projeto Ciência de Dados na Educação Pública ")+
    ggtitle("Avaliações de Ciência de Dados")+
    theme(axis.text.x=element_text(size=16, angle=0, vjust=.8, hjust=0.8)) +
    theme(axis.title.y = element_text(color = "black",size = 16))+
    theme(axis.title.x = element_text(color = "black",size = 16))+
    theme(axis.text.y=element_text(size=16)) +
    theme(axis.text = element_text(size = 16))  +
    theme(legend.text = element_text(size = 14)) +
    theme(legend.title = element_text(size = 16)) +
    theme(legend.position = "none")+
    scale_color_manual(values=c("#303600","#a5c500" ,"#d4c78c","#5ec6f2","003468"),
                       labels = c("OTIMO","MUITO BOM","BOM","REGULAR","INSATISFATORIO"))+
    ylim(0, 100)
  #  bbc_style()
  ggsave('figuras/boxplot_G1_QUANT_CD_QUALI_CD.png')
  
  
  #Boxplot G2
  
  dadoscd2022 %>% 
    mutate(qualitativog2 = factor(qualitativog2,levels=c("OTIMO","MUITO BOM","BOM",
                                                         "REGULAR","INSATISFATORIO"))) %>%
    filter(grupo %in% c('G2')) %>% 
    filter(quantitativog2>0) %>%
    #  select(freq_perc,`%p`) %>% 
    ggplot(aes(x=grupo,y = quantitativog2,fill=grupo))+
    geom_jitter(aes(colour=qualitativog2,size=freq_perc),width=0.1)+
    geom_boxplot(width=0.2,alpha=0.02) +
    xlab("")+
    ylab("%")+
    labs(caption = "Fonte de dados: Projeto Ciência de Dados na Educação Pública ")+
    ggtitle("Avaliações de Ciência de Dados")+
    theme(axis.text.x=element_text(size=16, angle=0, vjust=.8, hjust=0.8)) +
    theme(axis.title.y = element_text(color = "black",size = 16))+
    theme(axis.title.x = element_text(color = "black",size = 16))+
    theme(axis.text.y=element_text(size=16)) +
    theme(axis.text = element_text(size = 16))  +
    theme(legend.text = element_text(size = 14)) +
    theme(legend.title = element_text(size = 16)) +
    theme(legend.position = "none")+
    scale_color_manual(values=c("#303600","#a5c500" ,"#d4c78c","#5ec6f2","003468"),
                       labels = c("OTIMO","MUITO BOM","BOM","REGULAR","INSATISFATORIO"))+
    ylim(0, 100)
  #  bbc_style()
  ggsave('figuras/boxplot_G2_QUANT_CD_QUALI_CD.png')
  