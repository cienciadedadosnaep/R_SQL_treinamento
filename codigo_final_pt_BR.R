library(ggplot2)
library(bbplot)
library(readr)
library(ggrepel)
library(dplyr)
library(lubridate)
library(formattable)
library(tidyr)

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


#Boxplot G1  AVALIACAO - REORIENTACAO CIENCIA DE DADOS

dadoscd2022 %>% 
  mutate(qualitativog1 = factor(qualitativog1,levels=c("OTIMO","MUITO BOM","BOM",
                                                       "REGULAR","INSATISFATORIO"))) %>%
  filter(grupo %in% c('G1')) %>% 
  filter(quantitativog1>0) %>%
  #  select(freq_perc,`%p`) %>% 
  #    ggplot(aes(x=grupo,y = quantitativog1,fill=grupo))+
  ggplot(aes(x=grupo,y = quantitativog1))+
  geom_boxplot(width=0.2,alpha=0.02) +
  geom_jitter(aes(colour=qualitativog1,size=freq_perc),width=0.1)+
  xlab("")+
  ylab("%")+
  labs(caption = "Fonte: Projeto Ciência de Dados na Educação Pública")+
  ggtitle("Avaliações de Ciência de Dados")+
  theme(axis.text.x=element_text(size=16, angle=0, vjust=.8, hjust=0.8)) +
  theme(axis.title.y = element_text(color = "black",size = 16))+
  theme(axis.title.x = element_text(color = "black",size = 16))+
  theme(axis.text.y=element_text(size=16)) +
  theme(axis.text = element_text(size = 16))  +
  theme(legend.text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 16)) +
  scale_size_continuous(name="Frequência (%)")+
  scale_color_manual(name="Conceito",values=c("#303600","#a5c500" ,"#d4c78c","#5ec6f2","003468"),
                     labels = c("Ótimo","Muito Bom","Bom","Regular","Insatisfatório"))+
  ylim(0, 100)
#  bbc_style()
ggsave('figuras/boxplot_G1_QUANT_CD_QUALI_CD_ptBR.png')


#Boxplot G2  AVALIACAO - REORIENTACAO CIENCIA DE DADOS

dadoscd2022 %>% 
  mutate(qualitativog2 = factor(qualitativog2,levels=c("OTIMO","MUITO BOM","BOM",
                                                       "REGULAR","INSATISFATORIO"))) %>%
  filter(grupo %in% c('G2')) %>% 
  filter(quantitativog2>0) %>%
  #  select(freq_perc,`%p`) %>% 
  ggplot(aes(x=grupo,y = quantitativog2))+
  geom_boxplot(width=0.2,alpha=0.02) +
  geom_jitter(aes(colour=qualitativog2,size=freq_perc),width=0.1)+
  xlab("")+
  ylab("%")+
  labs(caption = "Fonte: Projeto Ciência de Dados na Educação Pública")+
  ggtitle("Avaliações de Ciência de Dados")+
  theme(axis.text.x=element_text(size=16, angle=0, vjust=.8, hjust=0.8)) +
  theme(axis.title.y = element_text(color = "black",size = 16))+
  theme(axis.title.x = element_text(color = "black",size = 16))+
  theme(axis.text.y=element_text(size=16)) +
  theme(axis.text = element_text(size = 16))  +
  theme(legend.text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 16)) +
  scale_size_continuous(name="Frequência (%)")+
  scale_color_manual(name="Conceito",values=c("#303600","#a5c500" ,"#d4c78c","#5ec6f2","003468"),
                     labels = c("Ótimo","Muito Bom","Bom","Regular","Insatisfatório"))+
  ylim(0, 100)
#    bbc_style()
ggsave('figuras/boxplot_G2_QUANT_CD_QUALI_CD_ptBR.png')





#Boxplot G2  AVALIACAO - REORIENTACAO CIENCIA DE DADOS C/ ANOTACOES PI

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
# IDADE DOS ESTUDANTES DO GRUPO G1

dados_pessoais %>% filter(grupo %in% c('G1')) %>%  
  summarise(idade = 
              round(as.numeric(as_date("2022-11-18")
                               -as_date(na.omit(nascimento)))/365)) %>% 
  ggplot(aes(x=idade))  + geom_dotplot(binwidth = 0.5) + bbc_style()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank() )+
  ylim(0,0.4)
ggsave('figuras/dotplot_idade_G1_CD.png')



##############################################################################
# IDADE DOS ESTUDANTES DO GRUPO G2

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

###############################################################################
# IDADE DOS ESTUDANTES DO GRUPO G1 e G2

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


###############################################################################
# DOTPLOT BOXPLOT DO QUANTITATIVO G1 POR SEXO 

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
  ylim(0, 100)
#  bbc_style()


########################################################################
# PALETA E CORES DE WERNER

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
                           
#################################################################################
# REORIENTACOES DE PI E CE E IDENTIFICACAO DO PROJETO 
set.seed(42)
dadospicd %>% 
  mutate(qualitativog1 = factor(qualitativog1,levels=c("OTIMO","MUITO BOM","BOM",
                                                       "REGULAR","INSATISFATORIO"))) %>%
  filter(grupo %in% c("G1")) %>%
  filter(quantitativog1>0) %>%
  ggplot(aes(x=nota_perc,y = quantitativog1,colour=COD_PROJ))+
  geom_point(size=6)+
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

################################################################################
# BOXPLOT DOS DADOS DAS AVALIACOES DE REORIENTACAO 

dadospicd %>% 
  mutate(qualitativog1 = factor(qualitativog1,levels=c("OTIMO","MUITO BOM","BOM",
                                                       "REGULAR","INSATISFATORIO"))) %>%
  filter(grupo %in% c("G1")) %>%
  filter(quantitativog1>0) %>%
  ggplot(aes(x=nota_perc,y = quantitativog1,colour=escola))+
  geom_point(size=6)+
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
  geom_label_repel(aes(label=escola), 
                   xlim=c(25,100), ylim=c(25,100))+
  theme(legend.position = "none")+
  labs(title = "Avaliações",
       subtitle = "Ciência de Dados e Práticas Investigativas",
       caption = "Fonte de dados: Projeto Ciência de Dados na Educação Pública")


ggsave('figuras/dispersao_Notax_PI_Notay_CD_class_escola.png')


#########################################################################
# BOXPLOT DADOS DAS AVALIACOES DE REORIENTACAO G1 DESAGEGADO POR ESCOLA

dadospicd %>% 
  mutate(qualitativog1 = factor(qualitativog1,levels=c("OTIMO","MUITO BOM","BOM",
                                                       "REGULAR","INSATISFATORIO"))) %>%
  filter(grupo %in% c("G1")) %>%
  filter(quantitativog1>0) %>%
  #  filter(COD_PROJ %in% c("P09")) %>%
  #  filter(quantitativog2>0) %>%
  #  select(freq_perc,`%p`) %>% 
  ggplot(aes(x=nota_perc,y = quantitativog1,colour=escola))+
  geom_point(size=)+
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
  geom_label_repel(aes(label=escola), 
                   xlim=c(25,100), ylim=c(25,100))+
  theme(legend.position = "none")+
  labs(title = "Avaliações",
       subtitle = "Ciência de Dados e Práticas Investigativas",
       caption = "Fonte de dados: Projeto Ciência de Dados na Educação Pública")


ggsave('figuras/dispersao_Notax_PI_Notay_CD_class_escola_idade.png')


#########################################################################
# JUNCAO DE DADAS TABELAS DE DADOS PESSOAIS E CD E IA

# Reuniao das duas tabelas  
dadospicdpessoais<- full_join(dadospicd,dados_pessoais,"ID")


# Idade dos estudantes do grupo G1
dadospicdpessoais %>% 
  #  group_by(escola) %>%
  summarise(grupo.x=grupo.x,escola=escola,idade = 
              round(as.numeric(as_date("2022-11-18")
                               -as_date((nascimento)))/365)) %>%
  filter(grupo.x %in% c("G1")) %>%
  filter(idade>0) %>%
  group_by(escola) %>%
  ggplot(aes(x=idade,y = escola))+
  geom_jitter(aes(colour=escola,size=8),width=0.1)+
  bbc_style()+
  xlab("Idade")+
  ylab("")+
  theme(axis.text.x=element_text(size=16, angle=0, vjust=.8, hjust=0.8)) +
  theme(axis.title.y = element_text(color = "black",size = 16))+
  theme(axis.title.x = element_text(color = "black",size = 16))+
  theme(axis.text.y=element_text(size=16)) +
  theme(axis.text = element_text(size = 16))  +
  theme(legend.text = element_text(size = 14)) +
  theme(legend.title = element_text(size = 16)) +
  theme(legend.position = "none")


ggsave('figuras/dotplot_idade_escola_G1_CD.png')


###############################################################################
# TABELA DE PROJETOS 

dadospicdpessoais %>% select(COD_PROJ,projetos) %>%
  distinct() %>%
  na.omit() %>%
  arrange(COD_PROJ) %>%
  formattable(align = c("l","l")) 


###############################################################################
# BOXPLOT AVALIACAO - REORIENTACAO G1 DESAGREGADO POR SEXO
dadospicdpessoais %>% 
  mutate(qualitativog1 = factor(qualitativog1,levels=c("OTIMO","MUITO BOM","BOM",
                                                       "REGULAR","INSATISFATORIO"))) %>%
  filter(grupo.x %in% c('G1')) %>% 
  filter(quantitativog1>0) %>%
  #  select(freq_perc,`%p`) %>% 
  ggplot(aes(x=sexo,y = quantitativog1,fill=sexo))+
  geom_boxplot(width=0.2,alpha=0.02) +
  geom_jitter(aes(colour=sexo,size=freq_perc),width=0.1)+
  xlab("sexo")+
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
  ylim(25, 100)+
  bbc_style()+
  theme(legend.position = "none")

ggsave('figuras/boxplot_G1_QUANT_CD_QUALI_CD_SEXO.png')



