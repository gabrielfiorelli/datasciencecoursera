# https://www.coursera.org/learn/exploratory-data-analysis/lecture/hVteM/air-pollution-case-study
# Air Pollution Case Study


#----------------------------------------#
#----------------------------------------#
# RESUMO

# A ideia é começar uma análise exploratória mostrando o básico.
# Isto nos mostrará como são os dados e o que poderemos fazer com eles.


#----------------------------------------#
#----------------------------------------#
# ASKING THE QUESTION

# Quando começamos a analisar os dados já temos uma ideia, mesmo que mais abrangente do que estamos buscando.
# Ela pode vir de uma pergunta mais abrangente ou já de uma hipótese que se queira testar.

# Os dados usados neste exemplo vieram do U.S. National Environmental Agency e envolve a medição de poluição do ar.
# Em especial o Fine Particulate Matter
# https://en.wikipedia.org/wiki/Particulates
## São particulas microscópicas, líquidas ou sólidas que ficam suspensam na atmosfera
## Elas tem efeito climáticos e também afetam vida humana.
## É basicamente um nome glamoroso para poeira.
## É uma preocupação medir estas partículas, porque elas estão presentes no ar que inalamos.
## Existe uma lei americana feita visando reduzir a poluição do ar.
## Então, uma das perguntas que temos interesse é: "A poluição do ar está menor agora do que antes?"
## Estes dados começaram a ser medidos em 1999 e são medidos até hoje.
## Então, olharemos os dados de 1999 e os dados de 2012 para tentar responder uma pergunta mais específica
## e possível de ser respondida com estes dados:
### A medições de poluição média do ar está menor em 2012 do que estava em 1999?
## Nota-se a diferença da segunda pergunta em relação a primeira.
## Ela já define o que será medido e qual será a comparação.
## É possível que para se chegar a esta pergunta, outras tenham sido respondidas anteriormente.


#----------------------------------------#
#----------------------------------------#
# ABRINDO E EXPLORANDO OS DADOS

# Limpando o ambiente
rm(list = ls())

# Set do diretório
setwd("C:/Users/gabriel.fiorelli/datasciencecoursera/ClassesAndAssignments/Course04Week04/CaseStudy_AirPollution")

library(dplyr)

# Listando os arquivos
list.files("Data/")

# Ler os arquivos
pm1 <- read.table("Data/RD_501_88101_1999-0.txt", comment.char = "#", header = FALSE, sep = "|", na.strings = "")
pm2 <- read.table("Data/RD_501_88101_2012-0.txt", comment.char = "#", header = FALSE, sep = "|", na.strings = "")

# Vendo linhas e dimensões
dim(pm1)

# Como o datasource está sem o nome das dimensões, pegamos a partir da primeira linha:
columnNames1 <- readLines("Data/RD_501_88101_1999-0.txt", 1)
columnNames2 <- readLines("Data/RD_501_88101_2012-0.txt", 1)
columnNames1 <- strsplit(columnNames1, "|", fixed = TRUE)
columnNames2 <- strsplit(columnNames2, "|", fixed = TRUE)
names(pm1) <- make.names(columnNames1[[1]])
names(pm2) <- make.names(columnNames2[[1]])

# Separando a variável que interessa
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

# Quando encontramos casos assim, devemos nos perguntar se isto (ter NA) é algo que precisamos nos preocupar.
# Por exemplo, no caso desta análise. Onde queremos saber se houve mudança entre 1999 e 2012:
## Ter dados faltante em alguns dias, ou algumas cidades é um problema?
# Ou sendo mais específico, quando já sabemos a quantidade de Missing Values:
## Ter 11% de dados faltantes, vai fazer grande diferença para esta resposta?
# Apesar de normalmente serem um grande inconveniente, dependerá de uma análise do problema para saber o quanto isto é ruim.

#----------------------------------------#
#----------------------------------------#
# COMPARANDO DADOS DE 1999 e 2012

summary(x1)
summary(x2)

# Tanto a mediana quanto a média são menores em 2012 do que eram em 1999.
# Em compensação o valor MAX é quase 6x maior em 2012. É um valor encontrado em outros países.
# Mas não observado normalmente nos EUA. O que pode indicar um erro de medição.
# Até aqui podemos dizer que aparentemente houve uma queda na quantidade de partículas PM2.5 ao longo dos anos.

mean(is.na(x1))
mean(is.na(x2))
# A quantidade de NA em 2012 também é menor. 11% em 1999 contra 5% em 2012.

boxplot(x1, x2)
# Mostra uma maior concentração mais próxima de 0 em ambos os datasets.
# No entanto, tem uma variedade muito maior em 2012.

boxplot(log10(x1), log10(x2))
# Com log10 fica um pouco mais fácil observar as caixas do gráfico.
# A linha da mediana reduz um pouco em 2012.
# Lembrando que, mesmo a diminuição nesta escala sendo pouca, por estar em log, a diferença real é maior.
# O variação dos dados em 2012 também fica mais clara em base 10.


#----------------------------------------#
#----------------------------------------#
# VALORES NEGATIVOS

# Na exploração feita até aqui, foi possível ver que em 2012 o dado mínimo é -10.
# A medição destes dados é feita usando um filtro, aonde as partículas são sugadas e a massa delas é medida.
# Então, não deveríamos ter valores negativos, porque não podemos ter massa negativa.

negativeValues <- x2 < 0
str(negativeValues)
sum(negativeValues, na.rm = TRUE)
# São 26474 valores negativos.
mean(negativeValues, na.rm = TRUE)
# Cerca de 2% dos resultados.

dates <- pm2$Date
str(dates)
# As datas estão criadas com valores inteiros.
dates <- as.Date(as.character(dates), "%Y%m%d")
str(dates)
# Agora as datas estão convertidas para o formato correto.

hist(dates, "month")
hist(dates[negativeValues], "month")

# Para fazer uma comparação mais direta, os dados usados serão da cidade de Nova Iorque.
# 1999
site1 <- unique(subset(pm1, State.Code == 36, c(County.Code, Site.ID)))
# 2012
site2 <- unique(subset(pm2, State.Code == 36, c(County.Code, Site.ID)))


# Criando variável agregando os dois valores. Dicionário.
# A ideia é conseguir comparar os resultado nos dois anos (1999 e 2012), no mesmo local.
# Só que vários lugares tem os dados coletados em 2012 mas não tinham em 1999.
site1 <- paste(site1[,1], site1[,2], sep = ".")
site2 <- paste(site2[,1], site2[,2], sep = ".")
intersection <- intersect(site1, site2)

str(site1)
str(site2)
str(intersection)

# Próximo passo é verificar quantas observações estão disponíveis em cada monitor.
# Uma nova variável criada, juntando os dados de cada linha.
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
# O escolhido para a análsie foi o County = 63, monitor = 2008

# Será gerado um novo set de dados somente com os dados do monitor em questão
pm1sub <- subset(pm1, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
pm2sub <- subset(pm2, State.Code == 36 & County.Code == 63 & Site.ID == 2008)

dim(pm1sub)
dim(pm2sub)

# Agora a ideia é plotar a quantidade de pm2.5 em função do tempo.
# Ver como foi a evolução disto ao longo do tempo.
# A primeira coisa é tirar as datas:
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

# Para fins de comparação os dados dos dois anos serão comparados no mesmo gráfico:
par(mfrow = c(1,2), mar = c(4,4,2,1))
plot(dates1, x1sub, pch = 20)
abline(h = median(x1sub, na.rm=T))
plot(dates2, x2sub, pch = 20)
abline(h = median(x2sub, na.rm=T))

# Como os dois gráficos estão com escalas diferentes.
# A primeira impressão é de que ao longo dos anos isto aumentou.
# Para verificar o range entre os dois conjuntos de dados:
rng <- range(x1sub, x2sub, na.rm = T)

# Refazendos os gráficos:
par(mfrow = c(1,2), mar = c(4,4,2,1))
plot(dates1, x1sub, pch = 20, ylim = rng)
abline(h = median(x1sub, na.rm = TRUE))
plot(dates2, x2sub, pch = 20, ylim = rng)
abline(h = median(x2sub, na.rm = TRUE))

# Ou seja, não só a média baixou, como também a poluição máxima.
# Até aqui, foi possível mostrar que houve redução na poluição.
# Mas seria interessante também olhar outros lugares.
# Então a ideia é avaliar a evolução de cada estado.

# Criaremos um gráfico que tem as médias de um estado para 1999
# e as médias para 2012.

head(pm1)
# Temos um campo que é o State.Code
# Então é basicamente tirar a média para cada estado para 1999

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

# Agora temos uma estrutura de dados que mostra a média de cada estado
# para cada período de tempo.

par(mfrow = c(1,1))
with(mrg, plot(rep(1999, 52), mrg[,2], xlim = c(1998, 2013), ylim = c(0,21)))
with(mrg, points(rep(2012, 52), mrg[,3]))
segments(rep(1999, 52), mrg[,2], rep(2012, 52), mrg[,3])


# A grande maioria dos estados tem um diminuição nas médias.
# Apenas alguns apresentam um crescimento médio de poluição