# Estimamos el modelo con los datos de Advertising

mData=read.csv("Advertising.csv")
regres01=lm(Sales~TV+Radio+Newspaper,data=mData)
summary(regres01)


# Tiramos un qqplot porque compara nuestra muestra con la distribucion de la 
# poblacion. El equivalente de esta grafica es el t.tes. Es alternitava, se 
# representar graficamente y también numericamente

library(car)
## Loading required package: carData
qqPlot(regres01, labels=row.names(mData), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")

# Varianza Constante. Homocedasticidad
# Varianza no constante (ncvtest) (Breusch-Pagan)

ncvTest(regres01)

spreadLevelPlot(regres01) # Si las lineas no son planas, significa que estamos
# teniendo problemas de heterocedasticidad

# Estas cosas son de regresion explicativa y no de prediccion

# VALIDACION GOBAL para la regresion explicativa
# No hace falta hacer todo lo demas

library(gvlma)
gvmodel <- gvlma(regres01) 
summary(gvmodel)


# Caso Practico NBA - Semilla <- 1234
