library(dplyr)

dir()
setwd("meus projetos/alura-cluster-com-R/cluster_R-data")


#### 1.2 Conhecendo a base de dados/ Definindo o problema ####

filmes <- read.csv('movies.csv',stringsAsFactors = F)
View(filmes)

## lendo os dados pre-processados
filmes_transf <- read.csv2('movies_transf.csv')
View(filmes_transf)

## Para usar o scale todas as colunas devem ser numéricas.
## Vamos usar o dplyr para isso o %>% significa concatenação de atributos (param)
## o sinal de menos em frente aos nomes das colunas significam que queremos selecionar tudo exceto 
## aquela colunas.
filmes_transf <- filmes_transf %>%
                  select(-movieId, -titulo)


## normalizar as bases de dados utilizando scale e transformar em um data frame.
dados_normalizados <- data.frame(scale(filmes_transf))
