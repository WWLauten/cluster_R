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

## install.packages('cluster')

library(cluster)

## clusplot deve receber os dados originais e o vetor de cluster que achamos com 3.
## as distâncias entre as linhas de cada cluster indica a quantidade de elementos que cada cluster possui.
## mais próximo mais dados, mais distante menos.
clusplot(filmes_transf, resultado_cluster$cluster,
         color = TRUE, shade = TRUE)

## install.packages('fpc')

library(fpc)

plotcluster(x = dados_normalizados, resultado_cluster$cluster, ignorenum = T)

## Demonstrar os resultados de forma mais simples para um usuário comum.
centros <- resultado_cluster$centers
View(centros)

## install.packages('reshape2')

library(reshape2)

## Função melt para fundir os dados, converter colunas em linhas (transposição)
centros_2 <- melt(centros)
View(centros_2)

## Alterar o nome das colunas para ficar mais intuitivo.
colnames(centros_2) <- c('cluster', 'gênero', 'centro')

## encode a vector as a factor (the terms ‘category’ and ‘enumerated type’ are also used for factors)
centros_2$cluster <- as.factor(centros_2$cluster)

## install.packages('ggplot2')

library(ggplot2)

## “facet_grid” para gerar os gráficos de forma separada para cada cluster
ggplot(data = centros_2) +
  geom_bar(aes(x = gênero, y = centro, fill = cluster), stat = 'identity') +
  facet_grid(cluster ~ .)

## Técnicas e estatísticas para selecionar melhor o número de clusters
## uma das técnicas é a Elbow (Cotovelo) ou SSE (do inglês, sum of squared error). 
## Para utilizá-la vamos criar um vetor chamado “range_k”
## esse vetor irá representar quantidade de clusters que vamos testar, que será de 1 a 25
range_k <- c(1:25)
## Vamos criar um objeto chamado “soma_quadrados”, o qual irá armazenar a soma dos quadrados de cada simulação.
soma_quadrados <- 0

## Vamos simular com vários números de clusters. 
## nstart	= if centers is a number, how many random sets should be chosen? Vamos usar 25. 
for (i in range_k) {
  cluster <- kmeans(dados_normalizados, centers = i, nstart = 25)
  ## Within cluster sum of squares by cluster. Vamos somar a soma dos quadrados de cada cluster para cada iteração.
  soma_quadrados[i] <- sum(cluster$withinss)  
} 

## Colocando em um gráfico para melhor visualização.
plot(range_k, soma_quadrados, type = 'b',
     xlab = 'Número de clusters',
     ylab = 'Soma dos quadrados')
## Adds an axis to the current plot, allowing the specification of the side, position, labels, and other options.
axis(side = 1, at = range_k, labels = range_k)
## Colocando uma linha onde achamos o 1.o elbow depois do 2.º cluster (o 1.º é desprezado). IMHO seria o próprio 3.
abline(v = 5, col = 'red')

## Como pudemos perceber no método anterior, nem sempre é fácil indicar o ponto de inflexão 
## com número de clusters mais adequado. Para complementar a análise do Elbow, outro método 
## para verificar o número mais adequado de cluster é o Silhouette, esse método consiste em calcular a média da silhueta.
rm(range_k) ## liberando memória.
range_k <- c(2:15)
medias_silhoutte <- c(0)

## usando a mesma semente de geração de números aleatórios
set.seed(1987)
for (i in range_k) {
  clusters <- kmeans(dados_normalizados, centers = i)
  ## dist computes and returns the distance matrix computed by using the specified distance measure to compute the
  ## distances between the rows of a data matrix
  silhueta <- silhouette(clusters$cluster, dist(dados_normalizados))
  medias_silhoutte[i] <- mean(silhueta[,3])  
} 

## Colocando em um gráfico para melhor visualização.
plot(medias_silhoutte, type = 'b',
     xlab = 'Número de clusters',
     ylab = 'Média Silhoutte')
## Adds an axis to the current plot, allowing the specification of the side, position, labels, and other options.
axis(side = 1, at = range_k, labels = range_k)

## O método silhueta consiste em calcular a média para cada escolha, que pode variar de -1 até +1, 
## quanto mais perto de -1 significa que a quantidade de cluster não está boa e quanto mais próximo de 1 significa 
## que a divisão da quantidade de cluster ficou boa.
## Vamos pegar apenas 12 clusters, foi a primeira das avaliações mais altas que aconteceram, mais próximas de 1,
## o resultado desse ponto está próximo de 0.3, como o de 15 clusters, mas como foi a primeira que aconteceu, então vamos
## utilizar esse valor.
set.seed(1987)
resultado_cluster <- kmeans(dados_normalizados, centers = 12)

centros <- resultado_cluster$centers
## Função melt para converter colunas em linhas (transposição)
centros_2 <- melt(centros)
## Alterar o nome das colunas para ficar mais intuitivo.
colnames(centros_2) <- c('cluster', 'gênero', 'centro')
## encode a vector as a factor (the terms ‘category’ and ‘enumerated type’ are also used for factors)
centros_2$cluster <- as.factor(centros_2$cluster)

## “facet_grid” para gerar os gráficos de forma separada para cada cluster
ggplot(data = centros_2) +
  geom_bar(aes(x = gênero, y = centro, fill = cluster), stat = 'identity') +
  facet_grid(cluster ~ .)

## Criar uma nova coluna chamada cluster e vamos atribuir o resultado que nós tivemos dos clusters
filmes$cluster <- resultado_cluster$cluster

## Nós vamos recuperar apenas a coluna cluster, que nós só queremos saber qual cluster ele pertence, 
## vamos salvar isso dentro de uma variável chamada agrupamento
agrupamento <- filmes[filmes$title == 'Toy Story (1995)', 'cluster']

## Vamos recuperar apenas os títulos, vamos executar, para retornar 
## apenas os dez primeiros filmes, a função “sample” para pegar apenas 10 filmes
filmes[filmes$cluster == agrupamento, 'title'] %>%
  sample(10)
