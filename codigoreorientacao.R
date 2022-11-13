#01 - Leitura banco de dados

library(readr)

# variaveis que correpondem a datas
datas <- read_csv("data/datas.csv", col_types = cols(data = col_datetime(format = "%d/%m/%Y ")))
# dados da avaliacao da reorientacao de CD 
dadoscd2022 <- read_csv("data/dadoscd2022.csv")
# dados da avaliacao anual de PI
dadospi2022 <- read_csv("data/dadospi2022.csv")
