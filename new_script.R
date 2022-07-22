## Importando bibliotecas:
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)

## Filtrando a Escola Mário Costa Neto
dfnesc %>% filter(no_entidade == 'EE - COLEGIO ESTADUAL MARIO COSTA NETO')

## Respondendo as perguntas proposta:
# 1- Quantos alunos matriculados há na escola no ano de 2019?

sum_alunos <- dfaluno %>% filter(nome_escola_id == 425) %>%  filter(nu_ano_censo == 2019)
sum_docentes <- dfdocente %>% filter(nome_escola_id ==425) %>% filter(nu_ano_censo == 2019)
escola <- dfesc %>% filter(nome_escola_id ==425) %>% filter(nu_ano_censo == 2019)

write_csv2(sum_alunos, file = 'data/alunos.csv')
write_csv2(sum_docentes, file = 'data/docentes.csv')
write_csv2(escola, file= 'data/escola.csv')

nrow(sum_alunos)
### O total de alunos matriculado no ano de 2019 é de 697.

# 2- Quantidade de alunos por sexo?
sexo <- group_by(sexo)
sexo_1 <-sum_alunos %>%  filter(tp_sexo == 1)
sexo_2 <-sum_alunos %>%  filter(tp_sexo == 2)
## Gráfico de pizza
ggplot(geom_bar())

### Exitem 393 alunos do sexo 1 e 304 do sexo 2.

