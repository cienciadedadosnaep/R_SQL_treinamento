# Library 
library(DBI)
library(RMySQL)
library(pool)
library(sqldf)
library("googleVis")
library("lubridate")

# Settings
db_user <- 'root'
db_password <- 'root'
db_name <- 'mydb'
db_host <- '127.0.0.1' # for local access
db_port <- 3309

# 3. Read data from db
mydb <-  dbConnect(MySQL(), user = db_user, password = db_password,
                   dbname = db_name, host = db_host, port = db_port)

s <- paste0("select id_aluno,nu_ano,nu_idade,tp_sexo from ", "alunos_bas_med")

rs <- dbSendQuery(mydb, s)

df<- NULL
df <-  fetch(rs, n = -1)

library(dplyr)
library(ggplot2)
df %>% filter(nu_ano>2000) %>% 
          group_by(nu_idade) %>% 
          summarise(n = n()) %>% ggplot(aes(x=nu_idade,y=n)) +
                                 geom_col()




