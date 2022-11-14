#00 - Leitura 

#01 - Leitura Banco de Dados

library(readr)

# variaveis que correpondem a datas
datas <- read_csv("data/datas.csv", col_types = cols(data = col_datetime(format = "%d/%m/%Y ")))
# dados da avaliacao da reorientacao de CD 
dadoscd2022 <- read_csv("data/dadoscd2022.csv")
# dados da avaliacao anual de PI
dadospi2022 <- read_csv("data/dadospi2022.csv")



## Qual e a situacao do IDEB das escolas parceiras e da cidade

colegiosp_siglas <- c("CEHMC","CEY","CSB","CEMCN",
                      "CEEV","CEMATF","CEES","CEMD",
                      "CEMN","CENTRAL","CMCJ","IFBA")
codigos_inep_parceiras <- c(29192790,29182182,29181550,29193699,
                            29182158,29185394,29180490,29181500,
                            29182131,29182123,29190061,29196442)


## Mapa das escolas com respectivos IDEBs 

##