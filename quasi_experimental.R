# Dados 

library(readr)
library(dplyr)

resultadoCD2021 <- read_csv("data/resultado_CD.csv")
resultadoCD2022 <- dadoscd2022 <- read_csv("data/dadoscd2022_V00.csv")


notas2021 <- resultadoCD2021 %>% select(ID,QUANTITATIVO_R)

notas2022 <- resultadoCD2022 %>% select(ID,quantitativog1,quantitativog2)



tabela <- full_join(notas2021,notas2022,"ID")


# metodo

library(tidysynth)





