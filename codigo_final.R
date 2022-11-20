library(ggplot2)
library(bbplot)
library(readr)
library(dplyr)

  
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
    geom_boxplot(width=0.2,alpha=0.02) +
    geom_jitter(aes(colour=qualitativog1,size=freq_perc),width=0.1)+
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
#    theme(legend.position = "none")+
    scale_color_manual(name="",values=c("#303600","#a5c500" ,"#d4c78c","#5ec6f2","003468"),
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
    geom_boxplot(width=0.2,alpha=0.02) +
    geom_jitter(aes(colour=qualitativog2,size=freq_perc),width=0.1)+
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
#    theme(legend.position = "none")+
    scale_color_manual(name="",values=c("#303600","#a5c500" ,"#d4c78c","#5ec6f2","003468"),
                       labels = c("OTIMO","MUITO BOM","BOM","REGULAR","INSATISFATORIO"))+
    ylim(0, 100)
  #  bbc_style()
  ggsave('figuras/boxplot_G2_QUANT_CD_QUALI_CD.png')
  
  
  
  
  
  #Boxplot G2 com anotações de PI
  
  dadoscd2022 %>% 
    mutate(qualitativog2 = factor(qualitativog2,levels=c("OTIMO","MUITO BOM","BOM",
                                                         "REGULAR","INSATISFATORIO"))) %>%
    filter(grupo %in% c('G2')) %>% 
    filter(quantitativog2>0) %>%
    #  select(freq_perc,`%p`) %>% 
    ggplot(aes(x=grupo,y = quantitativog2,fill=grupo))+
    geom_boxplot(width=0.2,alpha=0.02) +
    geom_jitter(aes(colour=qualitativog2,size=freq_perc),width=0.1)+
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
    #    theme(legend.position = "none")+
    scale_color_manual(name="",values=c("#303600","#a5c500" ,"#d4c78c","#5ec6f2","003468"),
                       labels = c("OTIMO","MUITO BOM","BOM","REGULAR","INSATISFATORIO"))+
    ylim(0, 100)
  #  bbc_style()
  ggsave('figuras/boxplot_G2_QUANT_CD_QUALI_CD.png')
  
  #############################################################################
  
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  library(bbplot)
  
# Idade dos estudantes do grupo G1
dados_pessoais %>% filter(grupo %in% c('G1')) %>%  
  summarise(idade = 
              round(as.numeric(as_date("2022-11-18")
                               -as_date(na.omit(nascimento)))/365)) %>% 
  ggplot(aes(x=idade))  + geom_dotplot(binwidth = 0.5) + bbc_style()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank() )+
  ylim(0,0.4)
ggsave('figuras/dotplot_idade_G1_CD.png')


# Idade dos estudantes do grupo G2

dados_pessoais %>% filter(grupo %in% c('G2')) %>%  
  summarise(idade = 
              round(as.numeric(as_date("2022-11-18")
                               -as_date(na.omit(nascimento)))/365)) %>% 
  ggplot(aes(x=idade))  + geom_dotplot(binwidth = 0.5) + bbc_style()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank() )+
  ylim(0,0.02)+
  xlim(13,19)
ggsave('figuras/dotplot_idade_G2_CD.png')


# Idade dos estudantes do grupo G1 e G2

dados_pessoais %>%
  group_by(grupo) %>% 
  filter(grupo %in% c("G1","G2")) %>%
  summarise(idade = 
              round(as.numeric(as_date("2022-11-18")
                               -as_date(na.omit(nascimento)))/365)) %>% 
  ggplot(aes(x=idade,fill=grupo))  + geom_dotplot(binwidth = 0.4) + bbc_style()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank() )+
  ylim(0,0.02)+
  xlim(13,19)+ 
  scale_fill_manual(name="",values=c("#303600","#a5c500"),
                     labels = c("G1","G2"))
  
ggsave('figuras/dotplot_idade_G1_G2_CD.png')


######################################################################

ID_G1 <- dadospicd %>% filter(grupo_pi %in% c("G1")) %>% select(ID) 
ID_G2 <- dadospicd %>% filter(grupo_pi %in% c("G2")) %>% select(ID)

dados_pessoais %>%
  group_by(Sexo) %>%
    filter(grupo %in% c(as.vector(ID_G1$ID))) %>%
  filter(Sexo %in% c("F","M")) %>%
  summarise(idade = 
              round(as.numeric(as_date("2022-11-18")
                               -as_date(na.omit(nascimento)))/365)) %>% 
  ggplot(aes(x=idade,fill=Sexo))  + geom_dotplot(binwidth = 0.4) + bbc_style()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank() )+
  ylim(0,0.02)+
  xlim(13,19)+ 
  scale_fill_manual(name="",values=c("#d4c78c","003468"),
                    labels = c("F","M"))

ggsave('figuras/dotplot_idade_G1_SEXO_CD.png')




dadospicd %>% 
  mutate(qualitativog1 = factor(qualitativog1,levels=c("OTIMO","MUITO BOM","BOM",
                                                       "REGULAR","INSATISFATORIO"))) %>%
  filter(grupo %in% c("G1")) %>%
  filter(quantitativog1>0) %>%
#  filter(quantitativog2>0) %>%
  #  select(freq_perc,`%p`) %>% 
  ggplot(aes(x=grupo,y = quantitativog1))+
  geom_boxplot(width=0.2,alpha=0.02) +
  geom_jitter(aes(colour=COD_PROJ),width=0.25,size=6)+
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
  #    theme(legend.position = "none")+
#  scale_color_manual(name="",values=c("#303600","#a5c500" ,"#d4c78c","#5ec6f2","003468"),
#                     labels = c("OTIMO","MUITO BOM","BOM","REGULAR","INSATISFATORIO"))+
  ylim(0, 100)
#  bbc_style()


########################################################################

library(ggrepel)

werner_colors = c("#f1e9cd",
                 "#cbc8b7",
                 "#241f20",
                 "#7994b5",
                 "#6c6d94",
                 "#93b778",
                 "#e6d058",
                 "#d17c3f",
                 "#711518",
                 "#7a4b3a",
                 "#657abb",
                 "#ab924b",
                 "#c76b4a",
                 "#b74a70",
                 "#c39e6d")


set.seed(42)
dadospicd %>% 
  mutate(qualitativog1 = factor(qualitativog1,levels=c("OTIMO","MUITO BOM","BOM",
                                                       "REGULAR","INSATISFATORIO"))) %>%
  filter(grupo %in% c("G1")) %>%
  filter(quantitativog1>0) %>%
#  filter(COD_PROJ %in% c("P09")) %>%
  #  filter(quantitativog2>0) %>%
  #  select(freq_perc,`%p`) %>% 
  ggplot(aes(x=nota_perc,y = quantitativog1,colour=COD_PROJ))+
  geom_point(size=6)+
#  geom_boxplot(width=0.2,alpha=0.02) +
#  geom_jitter(aes(colour=COD_PROJ),width=0.25,size=6)+
  xlab("Nota PI (%)")+
  ylab("NOTA CD (%)")+
  theme(axis.text.x=element_text(size=16, angle=0, vjust=.8, hjust=0.8)) +
  theme(axis.title.y = element_text(color = "black",size = 16))+
  theme(axis.title.x = element_text(color = "black",size = 16))+
  theme(axis.text.y=element_text(size=16)) +
  theme(axis.text = element_text(size = 16))  +
  theme(legend.text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.position = "none")+
  ylim(25, 100)+
  xlim(25, 100)+
  bbc_style()+
#annotate(
#  geom = "curve", 
#  x = dadospicd$nota_perc+sign(rnorm(n = 1,0,1))*rpois(1,3), 
#  y = dadospicd$quantitativog1, 
#  xend = dadospicd$nota_perc+sign(rnorm(n = 1,0,1))*rpois(1,3), 
#  yend = dadospicd$quantitativog1+sign(rnorm(n = 1,0,1))*rpois(1,3),
#  curvature = .3, 
#  arrow = arrow(length = unit(2, "mm"))) +
#  annotate(geom = "text", 
#           x = dadospicd$nota_perc, 
#           y = dadospicd$quantitativog1, 
#           label = dadospicd$COD_PROJ, hjust = "center")+
  geom_label_repel(aes(label=COD_PROJ), 
                   xlim=c(25,100), ylim=c(25,100))+
  theme(legend.position = "none")+
    scale_color_manual(name="",values=werner_colors,
                       labels = c("P01","P02","P03","P04","P05","P06",
                                  "P07","P08","P09","P10","P11","P12",
                                  "P13","P14","P15"))+
  labs(title = "Avaliações",
       subtitle = "Ciência de Dados e Práticas Investigativas",
       caption = "Fonte de dados: Projeto Ciência de Dados na Educação Pública")


ggsave('figuras/dispersao_Notax_PI_Notay_CD_class_proj.png')




    