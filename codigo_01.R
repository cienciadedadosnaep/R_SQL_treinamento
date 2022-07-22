#Quais perguntas podemos disparar para a nossa investigação:
 # a) Quantos alunos matriculados há na escola por ano?
 # b) Qual a quantidade de estudantes por turno?
 # c) Percentual de estudantes que tem a idade certa para aquele ano escolar?
 # d) Como os dados mais recentes estão registrados em relação a raça/cor e sexo para cada escola?
 # e) Qual é a formação dos diretores das escolas?
 # etc.


# Manipulação do banco de dados para o central
library(dplyr)

central <- dfnesc %>% filter(no_entidade == "EE - COLEGIO ESTADUAL DA BAHIA CENTRAL")
aluno_central <- dfaluno %>% filter(nome_escola_id == 472)
docente_centra <- dfdocente %>% filter(nome_escola_id == 472)
df_central <- dfesc %>%  filter(nome_escola_id == 472)


as.factor(aluno_central$nu_ano_censo) #coluna sem repetições

letra_a <-aluno_central %>% group_by(nu_ano_censo) %>% select(tp_nacionalidade) %>% 
  summarise(sum(tp_nacionalidade))
names(letra_a) <- c("ano","Total")

library(ggplot2)
letra_a %>% ggplot()+
  geom_bar(aes( ano,Total,fill="red"),stat="identity") 

letra_a$ano <- as.character(letra_a$ano)
letra_a %>% ggplot()+
  geom_bar(aes( ano,Total,fill="red"),stat="identity")
