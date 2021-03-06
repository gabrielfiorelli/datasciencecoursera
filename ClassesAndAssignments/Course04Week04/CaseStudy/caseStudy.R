# https://www.coursera.org/learn/exploratory-data-analysis/lecture/IH1Y2/clustering-case-study
# Clustering Case Study

#----------------------------------------#
rm(list = ls())
setwd("C:/Users/gabriel.fiorelli/datasciencecoursera/ClassesAndAssignments/Course04Week04/CaseStudy")

load("Data/samsungData.rda")

names(samsungData)[1:12]
table(samsungData$activity)

samsungData <- transform(samsungData, activity = factor(activity))
samsungDataSubject1 <- subset(samsungData, subject == 1)

# Usando dplyr: Filtrando as linhas da coluna subject e selecionando somente as colunas que interessam
# library(dplyr)
# samsungData <- transform(samsungData, activity = factor(activity))
# samsungDataSubject1 <- filter(samsungDataSubject1, subject == 1)
# samsungDataSubject1 <- select(samsungDataSubject1, c(tBodyAcc.mean...X, tBodyAcc.mean...Y, tBodyAcc.mean...Z, activity))

#----------------------------------------#
#----------------------------------------#
# PLOT 1
# Average acceleration for first subject

par(mfrow=c(1,2), mar = c(5, 4, 1, 1))

# "tBodyAcc.mean...X" = acelera��o do corpo no eixo X
# O gr�fico mostra a acelera��o do corpo nas atividades da legenda no eixo X.
plot(samsungDataSubject1[, 1], pch=19, col = samsungDataSubject1$activity, ylab = names(samsungDataSubject1)[1])

# "tBodyAcc.mean...Y" = acelera��o do corpo no eixo Y
# O gr�fico mostra a acelera��o do corpo nas atividades da legenda no eixo Y.
plot(samsungDataSubject1[, 2], pch=19, col = samsungDataSubject1$activity, ylab = names(samsungDataSubject1)[2])

legend("bottomright", legend = unique(samsungDataSubject1$activity), col = unique(samsungDataSubject1$activity), pch = 1)

#----------------------------------------#
#----------------------------------------#
# PLOT 2
# Dendogram
# Clustering based just on average acceleration

source("myplclust.R")
distanceMatrix <- dist(samsungDataSubject1[, 1:3])
hclustering <- hclust(distanceMatrix)
par(mfrow=c(1, 1), mar = c(5, 4, 1, 1))
myplclust(hclustering, lab.col = unclass(samsungDataSubject1$activity))

#----------------------------------------#
#----------------------------------------#
# PLOT 3
# Max acceleration for the first subject

par(mfrow=c(1, 2), mar = c(5, 4, 1, 1))

plot(samsungDataSubject1[,10], pch = 19, col = samsungDataSubject1$activity, ylab = names(samsungDataSubject1)[10])
plot(samsungDataSubject1[,11], pch = 19, col = samsungDataSubject1$activity, ylab = names(samsungDataSubject1)[11])

# Este caso se mostra melhor do que a m�dia para notar atividades.

#----------------------------------------#
#----------------------------------------#
# PLOT 4
# Dendogram
# Clustering based just on Max acceleration

source("myplclust.R")
distanceMatrix <- dist(samsungDataSubject1[, 10:12])
hclustering <- hclust(distanceMatrix)
par(mfrow=c(1, 1), mar = c(5, 4, 1, 1))
myplclust(hclustering, lab.col = unclass(samsungDataSubject1$activity))

# J� basicamente divide os dados em dois grandes clusters sendo um "Em movimento" e outro "Sem movimento".
# No entanto ainda existe muito dado agrupado de diferentes categorias.


#----------------------------------------#
#----------------------------------------#
# PLOT 5
# SVD

svd1 = svd(scale(samsungDataSubject1[, -c(562, 563)]))
par(mfrow = c(1, 2))
plot(svd1$u[, 1], col = samsungDataSubject1$activity, pch = 19)
plot(svd1$u[, 2], col = samsungDataSubject1$activity, pch = 19)

# O SVD foi feito excluindo as vari�veis de dimens�o (subject (562) e activity(563))
# O primeiro praticamente consegue separar os dados entre movimento en n�o movimento.

#----------------------------------------#
#----------------------------------------#
# PLOT 6
# Maximum Contributor

# Second Right Singular Vector
# Qual das features est� contribuindo em maior parte para a varia��o dos dados?
plot(svd1$v[, 2], pch = 19)

# Eixo X � um ponto para cada vari�vel (coluna) da tabela.
# Quanto mais longe do centro (0,00), maior a contribui��o da vari�vel.


#----------------------------------------#
#----------------------------------------#
# PLOT 7
# NEW - Clustering with Maximum Contributor (ver PLOT 6)

par(mfrow = c(1, 1))
# Retorna a coluna com maior vari�ncia
maxContrib <- which.max(svd1$v[, 2])
# Pega as colunas 10, 11 e 12 e mais a 296, que foi a que teve maior vari�ncia
distanceMatrix <- dist(samsungDataSubject1[, c(10:12, maxContrib)])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering, lab.col = unclass(samsungDataSubject1$activity))
legend("topright", legend = unique(samsungDataSubject1$activity), col = unique(samsungDataSubject1$activity), pch = 1)
# Agora, na altura 1,0, j� � poss�vel ver claramente 4 clusters:
## O walkdown
## O walk
## O Walkup
## O terceiro misturado com: Standing, Sitting, Laying
## Ou seja, falta separar as atividades de n�o se mover.

# As 4 vari�veis usadas para gerar o cluster, ou seja, que apresentaram maior explica��o da varia��o dos dados at� aqui foram:
names(head(samsungDataSubject1[, c(10:12, maxContrib)]))
# [1] "tBodyAcc.max...X"      "tBodyAcc.max...Y"      "tBodyAcc.max...Z"      "fBodyAcc.meanFreq...Z"
# O maxContributor foi
names(samsungData)[maxContrib]
#[1] "fBodyAcc.meanFreq...Z"


## D�VIDAS
# O que � exatamente o D, U e V dos svd?
head(svd1$d)
head(svd1$u)
# Usado no SVD
head(svd1$v)
# Usado no Max Contributor


#----------------------------------------#
#----------------------------------------#
# PLOT 8
# K-means Clustering (nstart = 1, first try)

kClust <- kmeans(samsungDataSubject1[, -c(562, 563)], centers = 6)
table(kClust$cluster, samsungDataSubject1$activity)
# O pr�prio Kmeans teve alguns problemas para separar os 6 clusters que sabemos que existem nestes dados.
# 1 - Laying
# 2 - Walkup
# 3 - Laying/Sitting
# 4 - Laying/Sitting
# 5 - Walk/Walkdown
# 6 - Sitting/Standing


#----------------------------------------#
#----------------------------------------#
# PLOT 9
# K-means Clustering (nstart = 100, first try)
kClust <- kmeans(samsungDataSubject1[, -c(562, 563)], centers = 6, nstart = 100)
table(kClust$cluster, samsungDataSubject1$activity)

#----------------------------------------#
#----------------------------------------#
# GERAL
# Cada cluster acaba tendo um resultado para este data set com 500 dimens�es
# Tamb�m podemos ver qual delas guia mais o centro do cluster
# Isto nos da uma boa ideia do que � mais importante para classificar as pessoas dentro deste cluster.
# Ou seja, n�o precisar�amos pegar as 500 vari�veis dispon�veis para conseguir definir se o conjunto de dados de uma determianda pessoa indica que ela est� andando, sentada e etc.

#----------------------------------------#
#----------------------------------------#
# Cluster 1 Variable Center (Laying)

layingCat <- 1
plot(kClust$centers[layingCat, 1:10], pch = 19, ylab = "Cluster Center", xlab = "")
# Primeiro plotando somente as 10 primeiras, veremos que algumas delas de um valor positivo (algumas das medidas)
plot(kClust$centers[layingCat,], pch = 19, ylab = "Cluster Center", xlab = "")
# Assim veremos o resultado para todas elas.


walking <- 4
plot(kClust$centers[walking, 1:10], pch = 19, ylab = "Cluster Center", xlab = "")
# Primeiro plotando somente as 10 primeiras, veremos que algumas delas de um valor positivo (algumas das medidas)
plot(kClust$centers[walking,], pch = 19, ylab = "Cluster Center", xlab = "")
# Assim veremos o resultado para todas elas.
# Olhar para estas vari�veis pode realmente nos dar umas dicas a respeito de quais s�o as vari�veis que s�o importantes.


# Esta � uma demonstra��o b�sica de como pegar um dataset com muitas vari�veis
# e usar t�cnicas de clusters, k-means e etc para tentar entender como gerar as categoriza��es
# necess�rias usando menos dados.
# Foi razoavelmente f�cil distinguir quais vari�veis indicam movimento e separam o movimento de parado.
# � poss�vel continuar a an�lise e entender tamb�m como separar, atrav�s destes dados os tr�s tipos de "parado".