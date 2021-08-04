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

## usando clusterização com kmeans com a mesma semente de geração de números aleatórios
set.seed(1987)
## usaremos 3 para o número de clusters ou 3 centróides. 
resultado_cluster <- kmeans(dados_normalizados, centers = 3)

## para mostrar a classificação de cada filme nos clusters achados de 1 a 3.
resultado_cluster$cluster

## para mostrar um df usa-se o view. Nesse caso vamos verificar os clusters means ou centros (?) por gênero de filme.
View(resultado_cluster$centers)

## para analisar vamos observar os dados gerais dos clusters (https://smolski.github.io/livroavancado/analise-de-clusters.html)
print(resultado_cluster)

## Esse é um vetor com a soma dos quadrados dentro dos clusters
resultado_cluster$withinss

## K-means clustering with 3 clusters of sizes 6797, 158, 1637
resultado_cluster$size

## Como o número de filmes em cada cluster é bastante desigual e a soma dos quadrados também está muito diferente
## provavelmente teremos que refazer com outro número de clusters
## Vamos observar agora com gráficos.

install.packages('cluster')

library(cluster)

## clusplot deve receber os dados originais e o vetor de cluster que achamos com 3.
## as distâncias entre as linhas de cada cluster indica a quantidade de elementos que cada cluster possui.
## mais próximo mais dados, mais distante menos.
clusplot(filmes_transf, resultado_cluster$cluster,
         color = TRUE, shade = TRUE)

install.packages('fpc')

library(fpc)

plotcluster(x = dados_normalizados, resultado_cluster$cluster, ignorenum = T)

## Demonstrar os resultados de forma mais simples para um usuário comum.
centros <- resultado_cluster$centers
View(centros)

install.packages('reshape2')

library(reshape2)

## Função melt para fundir os dados, converter colunas em linhas (transposição)
centros_2 <- melt(centros)
View(centros_2)

## Alterar o nome das colunas para ficar mais intuitivo.
colnames(centros_2) <- c('cluster', 'gênero', 'centro')

## encode a vector as a factor (the terms ‘category’ and ‘enumerated type’ are also used for factors)
centros_2$cluster <- as.factor(centros_2$cluster)

install.packages('ggplot2')

library(ggplot2)

## “facet_grid” para gerar os gráficos de forma separada para cada cluster
ggplot(data = centros_2) +
  geom_bar(aes(x = gênero, y = centro, fill = cluster), stat = 'identity') +
  facet_grid(cluster ~ .)
