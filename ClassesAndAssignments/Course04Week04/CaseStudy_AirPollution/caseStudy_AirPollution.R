# https://www.coursera.org/learn/exploratory-data-analysis/lecture/hVteM/air-pollution-case-study
# Air Pollution Case Study


#----------------------------------------#
#----------------------------------------#
# RESUMO

# A ideia � come�ar uma an�lise explorat�ria mostrando o b�sico.
# Isto nos mostrar� como s�o os dados e o que poderemos fazer com eles.


#----------------------------------------#
#----------------------------------------#
# ASKING THE QUESTION

# Quando come�amos a analisar os dados j� temos uma ideia, mesmo que mais abrangente do que estamos buscando.
# Ela pode vir de uma pergunta mais abrangente ou j� de uma hip�tese que se queira testar.

# Os dados usados neste exemplo vieram do U.S. National Environmental Agency e envolve a medi��o de polui��o do ar.
# Em especial o Fine Particulate Matter
# https://en.wikipedia.org/wiki/Particulates
## S�o particulas microsc�picas, l�quidas ou s�lidas que ficam suspensam na atmosfera
## Elas tem efeito clim�ticos e tamb�m afetam vida humana.
## � basicamente um nome glamoroso para poeira.
## � uma preocupa��o medir estas part�culas, porque elas est�o presentes no ar que inalamos.
## Existe uma lei americana feita visando reduzir a polui��o do ar.
## Ent�o, uma das perguntas que temos interesse �: "A polui��o do ar est� menor agora do que antes?"
## Estes dados come�aram a ser medidos em 1999 e s�o medidos at� hoje.
## Ent�o, olharemos os dados de 1999 e os dados de 2012 para tentar responder uma pergunta mais espec�fica
## e poss�vel de ser respondida com estes dados:
### A medi��es de polui��o m�dia do ar est� menor em 2012 do que estava em 1999?
## Nota-se a diferen�a da segunda pergunta em rela��o a primeira.
## Ela j� define o que ser� medido e qual ser� a compara��o.
## � poss�vel que para se chegar a esta pergunta, outras tenham sido respondidas anteriormente.


#----------------------------------------#
#----------------------------------------#
# ABRINDO E EXPLORANDO OS DADOS

# Limpando o ambiente
rm(list = ls())

# Set do diret�rio
setwd("C:/Users/gabriel.fiorelli/datasciencecoursera/ClassesAndAssignments/Course04Week04/CaseStudy_AirPollution")

library(dplyr)

# Listando os arquivos
list.files("Data/")

# Ler os arquivos
pm1 <- read.table("Data/RD_501_88101_1999-0.txt", comment.char = "#", header = FALSE, sep = "|", na.strings = "")
pm2 <- read.table("Data/RD_501_88101_2012-0.txt", comment.char = "#", header = FALSE, sep = "|", na.strings = "")

# Vendo linhas e dimens�es
dim(pm1)

# Como o datasource est� sem o nome das dimens�es, pegamos a partir da primeira linha:
columnNames1 <- readLines("Data/RD_501_88101_1999-0.txt", 1)
columnNames2 <- readLines("Data/RD_501_88101_2012-0.txt", 1)
columnNames1 <- strsplit(columnNames1, "|", fixed = TRUE)
columnNames2 <- strsplit(columnNames2, "|", fixed = TRUE)
names(pm1) <- make.names(columnNames1[[1]])
names(pm2) <- make.names(columnNames2[[1]])

# Separando a vari�vel que interessa
x1 <- pm1$Sample.Value
x2 <- pm2$Sample.Value

# Verificando a classe:
class(x1)
class(x2)

# Alguns resumos:
str(x1)
str(x2)
summary(x1)
summary(x2)

# Verificando a quantidade de Missing Values
mean(is.na(x1))
mean(is.na(x2))

#----------------------------------------#
#----------------------------------------#
# MISSING DATA

# Quando encontramos casos assim, devemos nos perguntar se isto (ter NA) � algo que precisamos nos preocupar.
# Por exemplo, no caso desta an�lise. Onde queremos saber se houve mudan�a entre 1999 e 2012:
## Ter dados faltante em alguns dias, ou algumas cidades � um problema?
# Ou sendo mais espec�fico, quando j� sabemos a quantidade de Missing Values:
## Ter 11% de dados faltantes, vai fazer grande diferen�a para esta resposta?
# Apesar de normalmente serem um grande inconveniente, depender� de uma an�lise do problema para saber o quanto isto � ruim.

#----------------------------------------#
#----------------------------------------#
# COMPARANDO DADOS DE 1999 e 2012

summary(x1)
summary(x2)

# Tanto a mediana quanto a m�dia s�o menores em 2012 do que eram em 1999.
# Em compensa��o o valor MAX � quase 6x maior em 2012. � um valor encontrado em outros pa�ses.
# Mas n�o observado normalmente nos EUA. O que pode indicar um erro de medi��o.
# At� aqui podemos dizer que aparentemente houve uma queda na quantidade de part�culas PM2.5 ao longo dos anos.

mean(is.na(x1))
mean(is.na(x2))
# A quantidade de NA em 2012 tamb�m � menor. 11% em 1999 contra 5% em 2012.

boxplot(x1, x2)
# Mostra uma maior concentra��o mais pr�xima de 0 em ambos os datasets.
# No entanto, tem uma variedade muito maior em 2012.

boxplot(log10(x1), log10(x2))
# Com log10 fica um pouco mais f�cil observar as caixas do gr�fico.
# A linha da mediana reduz um pouco em 2012.
# Lembrando que, mesmo a diminui��o nesta escala sendo pouca, por estar em log, a diferen�a real � maior.
# O varia��o dos dados em 2012 tamb�m fica mais clara em base 10.


#----------------------------------------#
#----------------------------------------#
# VALORES NEGATIVOS

# Na explora��o feita at� aqui, foi poss�vel ver que em 2012 o dado m�nimo � -10.
# A medi��o destes dados � feita usando um filtro, aonde as part�culas s�o sugadas e a massa delas � medida.
# Ent�o, n�o dever�amos ter valores negativos, porque n�o podemos ter massa negativa.

negativeValues <- x2 < 0
str(negativeValues)
sum(negativeValues, na.rm = TRUE)
# S�o 26474 valores negativos.
mean(negativeValues, na.rm = TRUE)
# Cerca de 2% dos resultados.

dates <- pm2$Date
str(dates)
# As datas est�o criadas com valores inteiros.
dates <- as.Date(as.character(dates), "%Y%m%d")
str(dates)
# Agora as datas est�o convertidas para o formato correto.

hist(dates, "month")
hist(dates[negativeValues], "month")

# Para fazer uma compara��o mais direta, os dados usados ser�o da cidade de Nova Iorque.
# 1999
site1 <- unique(subset(pm1, State.Code == 36, c(County.Code, Site.ID)))
# 2012
site2 <- unique(subset(pm2, State.Code == 36, c(County.Code, Site.ID)))


# Criando vari�vel agregando os dois valores. Dicion�rio.
# A ideia � conseguir comparar os resultado nos dois anos (1999 e 2012), no mesmo local.
# S� que v�rios lugares tem os dados coletados em 2012 mas n�o tinham em 1999.
site1 <- paste(site1[,1], site1[,2], sep = ".")
site2 <- paste(site2[,1], site2[,2], sep = ".")
intersection <- intersect(site1, site2)

str(site1)
str(site2)
str(intersection)

# Pr�ximo passo � verificar quantas observa��es est�o dispon�veis em cada monitor.
# Uma nova vari�vel criada, juntando os dados de cada linha.
pm1$County.Site <- with(pm1, paste(County.Code, Site.ID, sep = "."))
pm2$County.Site <- with(pm2, paste(County.Code, Site.ID, sep = "."))

# Outro jeito de chegar ao mesmo resultado. Usando dplyr.
# pm1 <- mutate(pm1, County.Site = paste(County.Code, Site.ID, sep = "."))
# pm2 <- mutate(pm2, County.Site = paste(County.Code, Site.ID, sep = "."))


# Separando somente os dados do estado de Nova Iorque
ny1 <- subset(pm1, State.Code == 36 & County.Site %in% intersection)
ny2 <- subset(pm2, State.Code == 36 & County.Site %in% intersection)

# Outro jeito de chegar ao mesmo resultado. Usando dplyr.
# ny1 <- filter(pm1, State.Code == 36 & County.Site %in% intersection)
# ny2 <- filter(pm2, State.Code == 36 & County.Site %in% intersection)

# Contar a quantidade de eventos de cada County.Site
sapply(split(ny1, ny1$County.Site), nrow)
sapply(split(ny2, ny2$County.Site), nrow)
# O escolhido para a an�lsie foi o County = 63, monitor = 2008

# Ser� gerado um novo set de dados somente com os dados do monitor em quest�o
pm1sub <- subset(pm1, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
pm2sub <- subset(pm2, State.Code == 36 & County.Code == 63 & Site.ID == 2008)

dim(pm1sub)
dim(pm2sub)

# Agora a ideia � plotar a quantidade de pm2.5 em fun��o do tempo.
# Ver como foi a evolu��o disto ao longo do tempo.
# A primeira coisa � tirar as datas:
# 1999 Data
dates1 <- as.Date(as.character(pm1sub$Date), "%Y%m%d")
x1sub <- pm1sub$Sample.Value
# Scatterplot com estes dados:
plot(dates1, x1sub)


# 2012 Data
dates2 <- as.Date(as.character(pm2sub$Date), "%Y%m%d")
x2sub <- pm2sub$Sample.Value
# Scatterplot com estes dados:
plot(dates2, x2sub)

# Para fins de compara��o os dados dos dois anos ser�o comparados no mesmo gr�fico:
par(mfrow = c(1,2), mar = c(4,4,2,1))
plot(dates1, x1sub, pch = 20)
abline(h = median(x1sub, na.rm=T))
plot(dates2, x2sub, pch = 20)
abline(h = median(x2sub, na.rm=T))

# Como os dois gr�ficos est�o com escalas diferentes.
# A primeira impress�o � de que ao longo dos anos isto aumentou.
# Para verificar o range entre os dois conjuntos de dados:
rng <- range(x1sub, x2sub, na.rm = T)

# Refazendos os gr�ficos:
par(mfrow = c(1,2), mar = c(4,4,2,1))
plot(dates1, x1sub, pch = 20, ylim = rng)
abline(h = median(x1sub, na.rm = TRUE))
plot(dates2, x2sub, pch = 20, ylim = rng)
abline(h = median(x2sub, na.rm = TRUE))

# Ou seja, n�o s� a m�dia baixou, como tamb�m a polui��o m�xima.
# At� aqui, foi poss�vel mostrar que houve redu��o na polui��o.
# Mas seria interessante tamb�m olhar outros lugares.
# Ent�o a ideia � avaliar a evolu��o de cada estado.

# Criaremos um gr�fico que tem as m�dias de um estado para 1999
# e as m�dias para 2012.

head(pm1)
# Temos um campo que � o State.Code
# Ent�o � basicamente tirar a m�dia para cada estado para 1999

mn1 <- with(pm1, tapply(Sample.Value, State.Code, mean, na.rm = TRUE))
str(mn1)
summary(mn1)

mn2 <- with(pm2, tapply(Sample.Value, State.Code, mean, na.rm = TRUE))
str(mn2)
summary(mn2)

# Criando os dataframes
d1 <- data.frame(State = names(mn1), mean = mn1)
d2 <- data.frame(State = names(mn2), mean = mn2)

# Merge (JOIN) nos dois dataframes
mrg <- merge(d1, d2, by = "State")

# Agora temos uma estrutura de dados que mostra a m�dia de cada estado
# para cada per�odo de tempo.

par(mfrow = c(1,1))
with(mrg, plot(rep(1999, 52), mrg[,2], xlim = c(1998, 2013), ylim = c(0,21)))
with(mrg, points(rep(2012, 52), mrg[,3]))
segments(rep(1999, 52), mrg[,2], rep(2012, 52), mrg[,3])


# A grande maioria dos estados tem um diminui��o nas m�dias.
# Apenas alguns apresentam um crescimento m�dio de polui��o