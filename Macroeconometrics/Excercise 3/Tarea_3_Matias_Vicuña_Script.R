#----------------------------------------------#
#******* Tarea Empírica 3               *******#
#******* Macroeconometría               *******#
#******* Profesor: Mauricio Tejada      *******#
#******* Ayudante: Diego Trauranca      *******#
#******* Estudiante: Matías Vicuña      *******#
#******* Universidad Alberto Hurtado    *******#
#----------------------------------------------#

rm(list = ls())

# Seteamos las librerías a utilizar
library(dynlm)
library(tidyverse)
library(readxl)
library(stargazer)
library(dygraphs)
library(forecast)
library(AER)

# Seteamos nuestro directorio
setwd("~/OneDrive - Universidad Alberto Hurtado/UNIVERSIDAD UAH/4to Año/8vo Semestre/Macroeconometría/Tareas/Ejercicio Empírico 3")

# Generamos la base de datos
db <- read_xlsx("datos_ejercicio_empirico_3.xlsx")

# Guardamos en la memoria
attach(db)

#******* Parte 1 *******#

# Generamos la Time-Series
ipm.ts <- ts(db$IPM, start = c(2009,1), frequency = 12)

lipm.ts <- log(ipm.ts)

dif.lipm <- diff(lipm.ts, lag = 1)

# Graficamos la variable lipm y diff(lipm)
plot(lipm.ts,
  main = "Índice Producción Manufacturero Linealizado",
  ylab = "ln(IPM)",
  xlab = "Tiempo en Meses",
  col = "blue")
# Respuesta: Claro efecto de tendencia creciente.

plot(dif.lipm,
  main = "Primera Diferencia - Índice Producción Manufacturero Linealizado",
  ylab = "dif(ln(IPM))",
  xlab = "Tiempo en Meses",
  col = "red")
# Respuesta: Hay presencia de caminata aleatoria, además de varianza constante.

#******* Parte 2 *******#

# Modelo AR(1)
ar1.ts <- dynlm(dif.lipm~L(dif.lipm))

stargazer(ar1.ts, type = "text", single.row = TRUE)

# Modelo AR(2)
ar2.ts <- dynlm(dif.lipm~L(dif.lipm, 1:2))

stargazer(ar2.ts, type = "text", single.row = TRUE)

# Modelo AR(3)
ar3.ts <- dynlm(dif.lipm~L(dif.lipm, 1:3))

stargazer(ar3.ts, type = "text", single.row = TRUE)

# Modelo AR(4)
ar4.ts <- dynlm(dif.lipm~L(dif.lipm, 1:4))

stargazer(ar4.ts, type = "text", single.row = TRUE)

# Modelo AR(5)
ar5.ts <- dynlm(dif.lipm~L(dif.lipm, 1:5))

stargazer(ar5.ts, type = "text", single.row = TRUE)

# Modelo AR(6)
ar6.ts <- dynlm(dif.lipm~L(dif.lipm, 1:6))

stargazer(ar6.ts, type = "text", single.row = TRUE)

# Modelo AR(7)
ar7.ts <- dynlm(dif.lipm~L(dif.lipm, 1:7))

stargazer(ar7.ts, type = "text", single.row = TRUE)

# Modelo AR(8)
ar8.ts <- dynlm(dif.lipm~L(dif.lipm, 1:8))

stargazer(ar8.ts, type = "text", single.row = TRUE)

# Comparación de los AR con el criterio de Akaike

aic.test <- data.frame(AIC = c(extractAIC(ar1.ts),extractAIC(ar2.ts),
  extractAIC(ar3.ts),extractAIC(ar4.ts),
  extractAIC(ar5.ts),extractAIC(ar6.ts),
  extractAIC(ar7.ts),extractAIC(ar8.ts)))
aic.test

AIC <- c(aic.test[2,],aic.test[4,],aic.test[6,],aic.test[8,],aic.test[10,],aic.test[12,],aic.test[14,],aic.test[16,])

Models <- c("AR(1)","AR(2)","AR(3)","AR(4)","AR(5)","AR(6)","AR(7)","AR(8)")

AIC.db <- data.frame(cbind(Models,AIC))
AIC.db

# Segun el criterio de AIC, AR(6) es el mínimo dentro de los 8 diferentes modelo de regresión autoregresivo.

#******* Parte 3 *******#
# Ahora usamos la autoregresión con 6 rezagos para evaluar y hacer el test de Hipotesis para verficar si es que nuestro modelo autoregresivo es significante o no.

# usamos el siguiente modelo:
ar6.ts <- dynlm(dif.lipm~L(dif.lipm, 1:6))

# Ahora, hacemos nuestro test de hipótesis:
test.f.ar6.dif <- linearHypothesis(ar6.ts, c("L(dif.lipm, 1:6)5 = 0", "L(dif.lipm, 1:6)6 = 0"))

test.f.ar6.dif

# Respuesta: al realizar el test de hipotesis F, se rechaza la hipotesis nula con un 0,000009465 de Pr(>F) sobre la hipotesis alternativa, es decir, el modelo autoregresivo con 6 periodos es significante.

#******* Parte 4 *******#
# Primero definimos el modelo AR(6) Estimado
ar.6.dif.hat <- ar(dif.lipm, start = c(2009,1), order.max = 6)

# Formulamos la predicción con 5 periodos siguientes.
predict <- predict(ar.6.dif.hat, n.ahead = 5)

prediccion <- c(predict$pred[1], predict$pred[2], predict$pred[3], predict$pred[4], predict$pred[5])

fecha <- c("Aug 2022", "Sep 2022", "Oct 2022", "Nov 2022", "Dec 2022")

prediccion.list <- data.frame(cbind(fecha,prediccion))
prediccion.list

# Luego de ello, hacemos la predicción del modelo con 5 periodos de interés (Aug, Sep, Oct, Nov, Dec) del 2022.

dif.aug <- predict$pred[1] # Intercepto Agosto de la predicción
dif.aug

dif.sep <- predict$pred[2] # Intercepto Septiembre de la predicción
dif.sep

dif.oct <- predict$pred[3] # Intercepto Octubre de la predicción
dif.oct

dif.nov <- predict$pred[4] # Intercepto Noviembre de la predicción
dif.nov

dif.dic <- predict$pred[5] # Intercepto Diciembre de la predicción
dif.dic

#******* Parte 5 *******#
# Para poder realizar el modelo autoregresivo debemos de usar el modelo con la proyección de los 5 periodos faltantes del año 2022

lipm.aug.ts <- lipm.ts[163]+dif.aug 
# Empezamos desde Julio del 2022 y vamos usando nuestro coeficiente proyectado 

lipm.sep.ts <- lipm.aug.ts+dif.sep 
# Usamos el valor calculado anterior sumando el coefciente proyectado de septiembre

lipm.oct.ts <- lipm.sep.ts+dif.oct 
# al igual que la anterior, usamos el objeto calculado anterior y le agregamos el valor proyectado de octubre.

lipm.nov.ts <- lipm.oct.ts+dif.nov
# mismo procedimiento, esta vez para el valor de noviembre proyectado

lipm.dec.ts <- lipm.nov.ts+dif.dic
# Finalizamos calculando el valor estimado de noviembre con la proyección del forecast de diciembre

# Por último, hacemos el valor del índice para diciembre del 2022.
dec.ts.ipm <- exp(lipm.dec.ts)
dec.ts.ipm

#******* Parte 6 *******#

reg <- ar(dif.lipm, order.max = 8, aic = TRUE)
reg$aic

reg_final <- ar(dif.lipm, order.max = 6)
resultados <- forecast(reg_final, h = 5, level = 0.95)
resultados

# Respuesta: Se llega a los mismo resultados anteriormente conseguidos en el apartado 4, comprobando su veracidad y precisión.
