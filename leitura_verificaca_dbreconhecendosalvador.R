# Library 
library(DBI)
library(RMySQL)
library(pool)
library(sqldf)
library("googleVis")
library("lubridate")

# Dicoinario de dados 
# https://docs.google.com/spreadsheets/d/1sGOqAJOPrLquRpBKeDcDKPjHloSWJdpgwXEkhMuNYok/edit?usp=sharing


# Settings
db_user <- 'root'
db_password <- 'root'
db_name <- 'mydb'
db_host <- '127.0.0.1' # for local access
db_port <- 3309

# 3. Read data from db

## docentes
mydb <-  dbConnect(MySQL(), user = db_user, password = db_password,
                   dbname = db_name, host = db_host, port = db_port)
s <- paste0("select * from ", "docentes_bas_med")
rs <- dbSendQuery(mydb, s)
dfdocente <- NULL
dfdocente <-  fetch(rs, n = -1)


## escolas
#mydb <-  dbConnect(MySQL(), user = db_user, password = db_password,
#                   dbname = db_name, host = db_host, port = db_port)
escola <- paste0("select * from ", "escolas_bas_med")
rescolas <- dbSendQuery(mydb, escola)

dfesc<- NULL
dfesc <-  fetch(rescolas, n = -1)

## nomes escolas
#mydb <-  dbConnect(MySQL(), user = db_user, password = db_password,
#                   dbname = db_name, host = db_host, port = db_port)
nescola <- paste0("select * from ", "nome_escola_bas_med")
nrescolas <- dbSendQuery(mydb, nescola)

dfnesc<- NULL
dfnesc <-  fetch(nrescolas, n = -1)


## alunos escolas
#mydb <-  dbConnect(MySQL(), user = db_user, password = db_password,
#                   dbname = db_name, host = db_host, port = db_port)
alunos <- paste0("select * from ", "alunos_bas_med")
nalunos <- dbSendQuery(mydb, alunos)

dfaluno<- NULL
dfaluno <-  fetch(nalunos, n = -1)



#######################################################################
library(dplyr)
names(dfaluno)

teste <- dfaluno %>% group_by(nu_ano_censo,tp_cor_raca,tp_sexo) %>%
                                  summarise(n=n())  %>% 
                                  filter(nu_ano_censo %in% c(2019))          


tp_cor_raca <- c(0,1,2,3,4,5)
tp_cor_raca_nome <- c("Não declarada",
"Branca",
"Preta",
"Parda",
"Amarela",
"Indígena")

dfcod <- data.frame(tp_cor_raca,tp_cor_raca_nome)

tp_sexo <- c(1,2)
tp_sexo_name <- c("Masculino","Feminino")

dfsexocod <- data.frame(tp_sexo_name,tp_sexo)


teste2019 <- full_join(teste,dfsexocod,by="tp_sexo") %>% full_join(dfcod,by="tp_cor_raca")



#######################################################################

library(webr)
library(ggplot2)

names(teste2019) <- c("ano","nracacor","nsexo","n","sexo","raca")
PieDonut(teste2019, aes(sexo,raca, count=n), 
         title = "Distribuição percentual dos estudantes por raça/cor e sexo")

##########################################################################


#Taxa de aprovacao - INEP - Base dos dados
library(readr)
escola_ano_escolar <- read_csv("~/R/data_ed_bancodedados/escola_ano_escolar.csv")

# Ideb - INEP - Base dos dados
escola_ano_escolar <- read_csv("~/R/data_ed_bancodedados/escola_ano_escolar.csv")


##########################################################################
## Codigo escolas projeto
## Cidade Jequie 

# Dicionario tp_dependencia
#1 - Federal
#2 - Estadual
#3 - Municipal
#4 - Privada

# Dicionario tp_situacao_funcionamento
#1 - Em Atividade
#2 - Paralisada
#3 - Extinta (ano do Censo)
#4 - Extinta em Anos Anteriores

dfesc %>% 
  filter(nu_ano_censo %in% c(2019)) %>%
  group_by(tp_dependencia,tp_situacao_funcionamento) %>% 
  summarise(n())

library(ggplot2)
library(tidyverse)

##############Ensino Fundamental total############################################

TDI_ESCOLAS_2021 %>% filter(CO_MUNICIPIO %in% c(2927408)) %>%
                     filter(NO_DEPENDENCIA %in% c("Municipal","Estadual")) %>%
#                     group_by(NO_DEPENDENCIA) %>%
                     arrange(NO_DEPENDENCIA)%>%
                     select(NO_DEPENDENCIA,FUN_CAT_0,MED_CAT_0) %>%
                     mutate(Rede = factor(NO_DEPENDENCIA,c("Municipal","Estadual"))) %>%
                     filter(!is.na(FUN_CAT_0)) %>%
                     ggplot(aes(x=Rede,
                                y=FUN_CAT_0,fill=NO_DEPENDENCIA))+
                     geom_boxplot(width=0.25)+
                     labs(x = "Rede Escolar",
                          y = "%",
                          fill=' ',
                          title = 'Taxas de Distorção Idade-série (2021)',
                          subtitle = 'Ensino Fundamental')+
                     theme(title = element_text(size = 12,colour = "Blue"))+
                     theme(axis.title.x=element_text(size=11)) +
                     theme(axis.title.y=element_text(size=11)) +
                     theme(axis.text=element_text(face="bold", color="Blue",size =12))+
                     coord_cartesian(ylim = c(0, 100))+
                     theme(legend.position="none")+
                     scale_fill_manual(values=c("gray","lightblue"))
ggsave('figuras/TDI_EF_TOTAL.png')

##############Rede Municipal Ensino Fundamental por série############################################

TDI_ESCOLAS_2021 %>% filter(CO_MUNICIPIO %in% c(2927408)) %>%
  filter(NO_DEPENDENCIA %in% c("Municipal")) %>%
  select(FUN_AI_CAT_0,FUN_AF_CAT_0) %>%
  gather(key="EF",value = "TDI") %>%
  mutate(ETAPAS = factor(EF,c("FUN_AI_CAT_0","FUN_AF_CAT_0"))) %>%
  mutate(ETAPAS = fct_recode(ETAPAS,c("AI"="FUN_AI_CAT_0"))) %>%
  mutate(ETAPAS = fct_recode(ETAPAS,c("AF"="FUN_AF_CAT_0"))) %>%
  filter(!is.na(TDI)) %>%
    ggplot(aes(x=ETAPAS,
             y=TDI,fill=EF))+
  geom_boxplot(width=0.25)+
  labs(x = "Etapa Ensino Fundamental",
       y = "%",
       fill=' ',
       title = 'Taxas de Distorção Idade-série (2021)',
       subtitle = 'Ensino Fundamental - Rede Municipal')+
  theme(title = element_text(size = 12,colour = "Blue"))+
  theme(axis.title.x=element_text(size=11)) +
  theme(axis.title.y=element_text(size=11)) +
  theme(axis.text=element_text(face="bold", color="Blue",size =12))+
  coord_cartesian(ylim = c(0, 100))+
  theme(legend.position="none")+
  scale_fill_manual(values=c("lightblue","lightblue"))
ggsave('figuras/TDI_EF_AI_AF_MUNICIPAL.png')


############## Rede Estadual Ensino Fundamental por série############################################

TDI_ESCOLAS_2021 %>% filter(CO_MUNICIPIO %in% c(2927408)) %>%
  filter(NO_DEPENDENCIA %in% c("Estadual")) %>%
  select(FUN_AI_CAT_0,FUN_AF_CAT_0) %>%
  gather(key="EF",value = "TDI") %>%
  mutate(ETAPAS = factor(EF,c("FUN_AI_CAT_0","FUN_AF_CAT_0"))) %>%
  mutate(ETAPAS = fct_recode(ETAPAS,c("AI"="FUN_AI_CAT_0"))) %>%
  mutate(ETAPAS = fct_recode(ETAPAS,c("AF"="FUN_AF_CAT_0"))) %>%
  filter(!is.na(TDI)) %>%
  ggplot(aes(x=ETAPAS,
             y=TDI,fill=EF))+
  geom_boxplot(width=0.25)+
  labs(x = "Etapa Ensino Fundamental",
       y = "%",
       fill=' ',
       title = 'Taxas de Distorção Idade-série (2021)',
       subtitle = 'Ensino Fundamental - Rede Estadual')+
  theme(title = element_text(size = 12,colour = "Blue"))+
  theme(axis.title.x=element_text(size=11)) +
  theme(axis.title.y=element_text(size=11)) +
  theme(axis.text=element_text(face="bold", color="Blue",size =12))+
  coord_cartesian(ylim = c(0, 100))+
  theme(legend.position="none")+
  scale_fill_manual(values=c("gray","gray"))
ggsave('figuras/TDI_EF_AI_AF_ESTADUAL.png')


###############Ensino Medio total#############################################

TDI_ESCOLAS_2021 %>% filter(CO_MUNICIPIO %in% c(2927408)) %>%
  filter(NO_DEPENDENCIA %in% c("Municipal","Estadual")) %>%
  #                     group_by(NO_DEPENDENCIA) %>%
  arrange(NO_DEPENDENCIA)%>%
  select(NO_DEPENDENCIA,FUN_CAT_0,MED_CAT_0) %>%
  mutate(Rede = factor(NO_DEPENDENCIA,c("Municipal","Estadual"))) %>%
  filter(!is.na(MED_CAT_0)) %>%
    ggplot(aes(x=Rede,
             y=MED_CAT_0,fill=NO_DEPENDENCIA))+
  geom_boxplot(width=0.25)+
  labs(x = "Rede Escolar",
       y = "%",
       fill=' ',
       title = 'Taxas de Distorção Idade-série (2021)',
       subtitle = 'Ensino Médio')+
  theme(title = element_text(size = 12,colour = "Blue"))+
  theme(axis.title.x=element_text(size=11)) +
  theme(axis.title.y=element_text(size=11)) +
  theme(axis.text=element_text(face="bold", color="Blue",size =12))+
  coord_cartesian(ylim = c(0, 100))+
  theme(legend.position="none")+
  scale_fill_manual(values=c("gray","lightblue"))
ggsave('figuras/TDI_EM_ESTADUAL.png')






##############Ensino Fundamental total############################################

TDI_ESCOLAS_2021 %>% filter(CO_MUNICIPIO %in% c(2927408)) %>%
  filter(NO_DEPENDENCIA %in% c("Municipal","Estadual")) %>%
                       group_by(NO_DEPENDENCIA) %>%
  arrange(NO_DEPENDENCIA)%>%
  select(NO_DEPENDENCIA,FUN_CAT_0,MED_CAT_0) %>%
  mutate(Rede = factor(NO_DEPENDENCIA,c("Municipal","Estadual"))) %>%
  filter(!is.na(FUN_CAT_0)) %>%
  ggplot(aes(
    x=Rede,
    y=FUN_CAT_0
             ,fill=NO_DEPENDENCIA
))+
  geom_jitter(aes(colour = NO_DEPENDENCIA),width=0.1)+
    geom_boxplot(width=0.2,alpha=0.2)+
  labs(
    x = "Rede Escolar",
       y = "%",
       fill=' ',
       title = 'Taxas de Distorção Idade-série (2021)',
       subtitle = 'Ensino Fundamental')+
  theme(title = element_text(size = 12,colour = "Blue"))+
  theme(axis.title.x=element_text(size=11)) +
  theme(axis.title.y=element_text(size=11)) +
  theme(axis.text=element_text(face="bold", color="Blue",size =12))+
  coord_cartesian(ylim = c(0, 100))+
  theme(legend.position="none")+
  scale_fill_manual(values=c("gray","lightblue"))
ggsave('figuras/TDI_EF_TOTAL_jitter.png')








##teste bbc

library(bbplot)

TDI_ESCOLAS_2021 %>% filter(CO_MUNICIPIO %in% c(2927408)) %>%
  filter(NO_DEPENDENCIA %in% c("Municipal","Estadual")) %>%
  group_by(NO_DEPENDENCIA) %>%
  arrange(NO_DEPENDENCIA)%>%
  select(NO_DEPENDENCIA,FUN_CAT_0,MED_CAT_0) %>%
  mutate(Rede = factor(NO_DEPENDENCIA,c("Municipal","Estadual"))) %>%
  filter(!is.na(FUN_CAT_0)) %>%
  ggplot(aes(
    x=Rede,
    y=FUN_CAT_0
    ,fill=NO_DEPENDENCIA
  ))+
  geom_jitter(aes(colour = NO_DEPENDENCIA),width=0.1,size=2)+
  geom_boxplot(width=0.2,alpha=0.2)+
  labs(
    x = "Rede Escolar",
    y = "%",
    fill=' ',
    title = 'TDI (2021)',
    subtitle = 'Ensino Fundamental')+
  geom_label(aes(x = 1.250, y = 36.5, 
                 label = "mediana 2x maior"), 
             hjust = 0.2, 
             vjust = 0.9, 
             colour = "#555555", 
             fill = "#FFFFFF", 
             alpha=0.0002,
             label.size = NA, 
             family="Helvetica", 
             size = 6)+
  geom_curve(aes(x = 1.15, y = 18, xend = 1.5, yend = 32), 
             colour = "#555555", 
             curvature = 0.2,
             size=0.5,arrow = arrow(length = unit(0.03, "npc")))+
  geom_curve(aes(x = 1.5, y = 38, xend = 1.85, yend = 46), 
             colour = "#555555", 
             curvature = -0.2,
             size=0.5,
             size=0.5,arrow = arrow(length = unit(0.03, "npc")))+
bbc_style()+
  theme(legend.position="none")


ggsave('figuras/TDI_EF_TOTAL_jitter.png')








#############################################################
  escola_ano_escolar %>%
  filter(id_municipio %in% c(2927408)) %>%
  mutate(ano = as.character(ano),ano_escolar=as.character(ano_escolar)) %>%
  group_by(ano,ano_escolar) %>% select(ano,ano_escolar,taxa_aprovacao) %>%
  filter(!is.na(taxa_aprovacao)) %>%
  summarise(mediana=median(taxa_aprovacao)) %>%
  ggplot(aes(x=ano,y=ano_escolar,fill=mediana))+
  geom_tile()+
  labs(
    x = "ano",
    y = "ano escolar",
    fill='TAE',
    title = 'Taxa de aprovação',
    subtitle = 'Ensino Fundamental - Salvador/BA')
  bbc_style()
  ggsave('figuras/TAE_EF_TOTAL.png')
  


#######################################IDEB#####################################
library(ggplot2)
library(ggjoy)
library(bbplot)
  
  idebssa <-  escola %>% filter(id_municipio %in% c(2927408))  
  idebssa %>%
    filter(ano<2021) %>%
    filter(anos_escolares %in% c("finais (6-9)")) %>%
    mutate(ano = as.factor(ano)) %>%
    mutate(id_escola = as.factor(id_escola)) %>%
    group_by(ano,id_escola) %>%
    ggplot(aes(x=ideb, y=ano)) +
    geom_joy(scale = 5, rel_min_height = 0.01) +
    scale_x_continuous(expand = c(0.00, 0)) +
    scale_y_discrete(expand = c(0.00, 0))+
    #    scale_y_discrete(labels = Ano)+
    xlab("")+
    ylab("")+
    labs(caption = "Fonte de dados:  Instituto Nacional 
       de Estudos e Pesquisas Educacionais Anísio Teixeira (Inep)")+
    ggtitle("Ideb das escolas de Salvador (anos finais)")+
    theme(axis.text.x=element_text(size=16, angle=0, vjust=.8, hjust=0.8)) +
    theme(axis.title.y = element_text(color = "black",size = 16))+
    theme(axis.title.x = element_text(color = "black",size = 16))+
    theme(axis.text.y=element_text(size=16)) +
    theme(axis.text = element_text(size = 16))  +
    theme(legend.text = element_text(size = 14)) +
    theme(legend.title = element_text(size = 16)) +
    theme(legend.position = "none")
#  +
#    bbc_style()
    ggsave('figuras/idebaf.png')
  
  
    idebssa %>%
      filter(ano<2021) %>%
      filter(anos_escolares %in% c("iniciais (1-5)")) %>%
      mutate(ano = as.factor(ano)) %>%
      mutate(id_escola = as.factor(id_escola)) %>%
      group_by(ano,id_escola) %>%
      ggplot(aes(x=ideb, y=ano)) +
      geom_joy(scale = 5, rel_min_height = 0.01) +
      scale_x_continuous(expand = c(0.00, 0)) +
      scale_y_discrete(expand = c(0.00, 0))+
      #    scale_y_discrete(labels = Ano)+
      xlab("")+
      ylab("")+
      labs(caption = "Fonte de dados:  Instituto Nacional 
       de Estudos e Pesquisas Educacionais Anísio Teixeira (Inep)")+
      ggtitle("Ideb das escolas de Salvador (anos iniciais)")+
      theme(axis.text.x=element_text(size=16, angle=0, vjust=.8, hjust=0.8)) +
      theme(axis.title.y = element_text(color = "black",size = 16))+
      theme(axis.title.x = element_text(color = "black",size = 16))+
      theme(axis.text.y=element_text(size=16)) +
      theme(axis.text = element_text(size = 16))  +
      theme(legend.text = element_text(size = 14)) +
      theme(legend.title = element_text(size = 16)) +
      theme(legend.position = "none")
    #  +
    #    bbc_style()
    ggsave('figuras/idebai.png')  
    
##########################finais e iniciais####################################    
    

    idebssa %>%
      filter(ano<2021) %>%
      filter(anos_escolares %in% c("iniciais (1-5)","finais (6-9)")) %>%
      mutate(ano = as.factor(ano)) %>%
      mutate(id_escola = as.factor(id_escola)) %>%
      mutate(`Ensino Fundamental` = anos_escolares) %>%
      group_by(ano,id_escola,anos_escolares) %>%
      ggplot(aes(x=ideb, y=ano,fill=`Ensino Fundamental`)) +
      geom_joy(scale = 5, rel_min_height = 0.01) +
      scale_x_continuous(expand = c(0.00, 0)) +
      scale_y_discrete(expand = c(0.00, 0))+
      #    scale_y_discrete(labels = Ano)+
      xlab("Ideb")+
      ylab("")+
      labs( subtitle = "Ensino Fundamental anos:", caption = "Fonte de dados:  Instituto Nacional 
       de Estudos e Pesquisas Educacionais Anísio Teixeira (Inep)")+
      ggtitle("Ideb das escolas de Salvador")+
      theme(axis.text.x=element_text(size=16, angle=0, vjust=.8, hjust=0.8)) +
      theme(axis.title.y = element_text(color = "black",size = 16))+
      theme(axis.title.x = element_text(color = "black",size = 16))+
      theme(axis.text.y=element_text(size=16)) +
      theme(axis.text = element_text(size = 16))  +
      theme(legend.text = element_text(size = 14)) +
      theme(legend.title = element_text(size = 16)) +
      theme(legend.position = "bottom")  +
        bbc_style()
    ggsave('figuras/idebaif.png')        
    
    
    
    
    
    
    
  
  
  
  
  
  
  