rm(list = ls())
setwd("C:/Users/gabriel.fiorelli/datasciencecoursera/Course04Week01 - Exploratory Data Analysis")

## BASIC PLOT SYSTEM
## CARS
##

library(datasets)
data(cars)
with(cars, plot(speed, dist))
View(cars)

## LATTICE PLOT SYSTEM

library(lattice)
state <- data.frame(state.x77, region = state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4, 1))

## GGPLOT2 SYSTEM
library(ggplot2)
data(mpg)
qplot(displ, hwy, data = mpg)
View(mpg)


## BASE GRAPH SYSTEM

library(datasets)

hist(airquality$Ozone)

with(airquality, plot(Wind, Ozone))

airquality <- transform(airquality, Month = factor(Month))
boxplot(Ozone ~ Month, airquality, xlab = "Month", ylab = "Ozone (ppb)", col = "red")
