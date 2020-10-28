#----------------------------------------------
# MDS - Prediccion
# Caso Practico 1  -  Datos NBA
#----------------------------------------------

# Cargamos las librerias necesarias

library(readr)
library(tidyverse)
library(MASS)
library(gvlma)
library(car)

# Extraemos los datos del fichero nba.csv

datosNBA <- read.csv("nba.csv")


# Lanzamos el modelo con todas las variables disponibles en el dataset

modelo1 <- lm(Salary ~ NBA_Country + NBA_DraftNumber + Age + Tm + G + MP + PER +
                TS. + X3PAr + FTr + ORB. + DRB. + TRB. + AST. + STL. + BLK. +
                TOV. + USG. + OWS + DWS + WS + WS.48 + OBPM + DBPM + BPM +
                VORP, data = datosNBA)

summary(modelo1)

# Contrastamos todas las hipOtesis del modelo mediante el test de Pena, 
# EA and Slate, EH (2006)


gvmodel <- gvlma(modelo1) 
summary(gvmodel)


# Comprobamos la posible multicolinealidad del modelo con el Factor de inflacion
# de la varianza VIF


vif(modelo1) 


# Vamos a comprobar la existencia de valores atipicos en la muestra

outlierTest(modelo1)



# Mediante el metodo de seleccion Stepwise mixto, obtenemos que habria que 
# descartar del modelo1, 17 variables para obtener la mejor prediccion posible

stepAIC(modelo1, direction = "both")

# Por tanto, el modelo resultante despues de este analisis seria el siguiente:

modelo2 <- lm(Salary ~ NBA_DraftNumber + Age + G + MP + PER +
                           X3PAr + ORB. + TRB. + USG. + WS + OBPM, 
                         data = datosNBA)
summary(modelo2)

# Comparamos este nuevo modelo con el modelo1 con un analisis ANOVA

anova(modelo2, modelo1)

# Obtenemos un p_valor alto, con lo cual el modelo2 no se puede rechazar

# A continuación, realizamos los diferentes contrastes y analisis realizados anteriormente
# para obtener mayor informacion sobre el nuevo modelo

# Validacion global

gvmodel2 <- gvlma(modelo2) 
summary(gvmodel2)

# VIF

vif(modelo2) 

# Outliers

outlierTest(modelo2)


#### Cambio en la variable NBA_DraftNumber ####
# Dentro de la variable de Draft, se pueden establecer varias categorias,
# que nos ofrecezcan algo mas de informacion
# Podemos agrupar por tramos, ya que las primeras posiciones del draft puedan
# tener una relacion con el salario parecida pero distinta de la de las
# ultimas posiciones
# La primera distincion que queria hacer es entre jugadores de primera ronda y
# los de segunda (mas los no drafteados). Esto es interesante porque a los 
# jugadores de primera ronda se les aplica el Rookie Scale en el contrato
# Esto es, que segun la posicion, el jugador tiene un salario asegurado
# durante los siguientes 2-3 annos
# Los jugadores de segunda ronda no tienen este "privilegio" y han de
# negociar las condiciones. Normalmrnte estos ultimos tendran un menor salario
# excepto algunos casos de jugadores internacionales que vienen a la liga con una
# reputacion ya labrada.
# La segunda distincion es en la primera ronda. Los 15 primeros picks son
# los mas importantes y los que mas dinero reciben en sus contratos de rookies
# Ademas, estos jugadores son los mejores prospectos, por lo que 
# probablemente tengan un mejor rendimiento en su carrera que se acabara
# traduciendo en salarios mas grandes
# Ademas, como mencionaba antes, es posible que algun jugador extranjero
# con cierto renombre prefiera ser seleccionado en segunda ronda (o no serlo)
# antes que en la parte baja de la primera, para poder negociar libremente su contrato.
# En resumen, voy a generar tres nuevas variables binarias para cada uno de
# los tres rangos descritos, a saber: 
# - Del pick 1 al 15  <-  HighFirst
# - Del pick 16 al 30  <-  LowFirst
# - Del pick 31 al 60 (mas los valores de 62, que corresponden a los no drafteados)
# La variable se llamara  <-  SecUnd

# Primero creo tres columnas identicas a la variable ya existente de NBA_DraftNumber

datosNBA <- datosNBA %>% mutate(HighFirst = NBA_DraftNumber,
                                LowFirst = NBA_DraftNumber,
                                SecUnd = NBA_DraftNumber)

# Segundo, completo esas columnas asignando 1s y 0s dependiendo del numero de draft

# Para la parte alta de la primera ronda

datosNBA$HighFirst[datosNBA$HighFirst <= 15] <- 1
datosNBA$HighFirst[16 <= datosNBA$HighFirst] <- 0

# Para la parte baja de la primera

datosNBA$LowFirst[datosNBA$LowFirst <= 15 | datosNBA$LowFirst >= 30] <- 0
datosNBA$LowFirst[datosNBA$LowFirst >= 16 & datosNBA$LowFirst <= 30] <- 1

# Para segunda ronda y undrafted

datosNBA$SecUnd[datosNBA$SecUnd <= 30] <- 0
datosNBA$SecUnd[datosNBA$SecUnd >= 31] <- 1


# Generamos el modelo con estas nuevas variables
modelo3 <- lm(Salary ~ HighFirst + LowFirst + SecUnd + Age + G + MP + PER +
                           X3PAr + ORB. + TRB. + USG. + WS + OBPM, 
                         data = datosNBA)
summary(modelo3)

# Volvemos a utilizar el procedimiento Stepwise mixto
# La conclusion es que debemos descartarnos de la variable SecUnd

stepAIC(modelo3, direction = "both")

# Creamos un modelo4 ahora sin la variable descartada por el proceso

modelo4 <- lm(Salary ~ HighFirst + LowFirst + Age + G + MP + PER +
                X3PAr + ORB. + TRB. + USG. + WS + OBPM, 
              data = datosNBA)

# Y finalmente, comparamos este nuevo modelo 4 con el modelo 2, para 
# decidir con cual nos quedariamos

anova(modelo4, modelo2)

AIC(modelo2,modelo4)

# El AIC es menor en el modelo2, por lo que nos quedamos con ese como
# mejor predictor de los salarios en la NBA
