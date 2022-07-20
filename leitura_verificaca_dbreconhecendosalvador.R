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

## docentes
mydb <-  dbConnect(MySQL(), user = db_user, password = db_password,
                   dbname = db_name, host = db_host, port = db_port)
s <- paste0("select * from ", "docentes_bas_med")
rs <- dbSendQuery(mydb, s)
df<- NULL
df <-  fetch(rs, n = -1)


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



