#----------------------------------------------#
#******* Ejercicio Empírico 6           *******#
#******* Macroeconometría               *******#
#******* Profesor: Mauricio Tejada      *******#
#******* Ayudante: Diego Trauranca      *******#
#******* Estudiante: Matías Vicuña      *******#
#******* Universidad Alberto Hurtado    *******#
#----------------------------------------------#

rm(list = ls())

library(dynlm)
library(tidyverse)
library(readxl)
library(stargazer)
library(dygraphs)
library(forecast)
library(AER)
library(urca)
library(vars)

# Seteamos nuestro directorio de trabajo.
setwd("~/OneDrive - Universidad Alberto Hurtado/UNIVERSIDAD UAH/4to Año/8vo Semestre/Macroeconometría/Tareas/Ejercicio Empirico 6")

# Cargamos la base de datos
db <- read_xlsx("datos_ejercicio_empirico_6.xlsx")

# Guardamos en la memoria
attach(db)

#********************** PARTE 1 **********************#

TIB1 <- db$TIB1
TIB10 <- db$TIB10

# Transformamos a serie de tiempo
TIB1.ts <- ts(TIB1,
  start = c(2002, 9),
  end = c(2021, 10),
  frequency = 12)

TIB10.ts <- ts(TIB10,
  start = c(2002, 9),
  end = c(2021, 10),
  frequency = 12)

lTIB1 <- log(TIB1.ts)
lTIB10 <- log(TIB10.ts)

db.ts <- ts.union(lTIB1, lTIB10)

# Gráfica temporal de las tasas.
ts.plot(
  db.ts,
  col = c("blue", "red"),
  lwd = 2,
  main = "Tasas Bonos del Tesoro, 2002 al 2021",
  xlab = "Tiempo (Meses)",
  ylab = "Tasa de Interés (%)"
)
legend(
  title = "Tipo de Tasa",
  x = "bottomleft",
  legend = c("TIB1", "TIB10"),
  fill = c("blue", "red"),
  col = c("blue", "red"),
  box.lwd = 1,
  bg = 0,
)

# Se comprueba que hay un correlacioón entre TIB1 y TIB10, en donde cada uno tiene una tendencia muy similar en terminos de fluctuaciónes. Sin embargo, se aprecia que TIB1 tiene una variación más alta que la de TIB10.

#********************** PARTE 2 **********************#

test.tib1.ur <-
  ur.df(
    TIB1,
    type = c("trend"),
    lags = 1,
    selectlags = c("AIC")
  )
summary(test.tib1.ur)

test2.tib1.ur <-
  ur.df(
    TIB1,
    type = c("drift"),
    lags = 1,
    selectlags = c("AIC")
  )
summary(test2.tib1.ur)

test3.tib1.ur <-
  ur.df(
    TIB1,
    type = c("none"),
    lags = 1,
    selectlags = c("AIC")
  )
summary(test3.tib1.ur)

ndiffs(log(TIB1))
# Respuesta: Se comprueba que mediante los test de raiz unitaria, el bono de tesoro TIB1 se observa que hay condiciones para la Cointegración de Orden 1.

test.tib10.ur <-
  ur.df(
    TIB10,
    type = c("trend"),
    lags = 1,
    selectlags = c("AIC")
  )
summary(test.tib10.ur)

test2.tib10.ur <-
  ur.df(
    TIB10,
    type = c("drift"),
    lags = 1,
    selectlags = c("AIC")
  )
summary(test2.tib10.ur)

test3.tib10.ur <-
  ur.df(
    TIB10,
    type = c("none"),
    lags = 1,
    selectlags = c("AIC")
  )
summary(test3.tib10.ur)

ndiffs(log(TIB10))
# Respuesta: Se comprueba que mediante los test de raiz unitaria, el bono de tesoro TIB10 se observa que hay condiciones para la Cointegración de Orden 1.

#********************** PARTE 3 **********************#

# Definimos nuestro modelo dinámico.
regresion.1 <- dynlm(lTIB10 ~ lTIB1, data = db.ts)
stargazer(regresion.1, type = "text")
summary(regresion.1)

#********************** PARTE 4 **********************#
# Guardamos los residuos del modelo
residuales <- resid(regresion.1)

# Grafico de los Residuales
plot(residuales, main = "Residuales", lwd = 1.5, col = "blue", xlab = "Tiempo (Meses)", ylab = "Residuales")
abline(h = 0, lwd = 2, col = "red")
# Tendencia del error espurea

test.res.ur <-
  ur.df(
    residuales,
    type = c("none"),
    lags = 6,
    selectlags = c("AIC")
  )
summary(test.res.ur)

ndiffs(residuales)
# Tiene un orden de Cointegración de 1, por tanto, si se confirma la relación a largo plazo de las tasas.

#********************** PARTE 5 **********************#

# Modificamos nuestro log(TIB1) y log(TIB10) como la diferencia del mismo.
diff.lTIB1 <- diff(lTIB1)
diff.lTIB10 <- diff(lTIB10)

# Modelo de Rezagos con TIB10
regresion.2 <- dynlm(diff.lTIB10 ~ L(diff.lTIB10,1:6) + L(diff.lTIB1,1:6) + residuales, data = db.ts)
summary(regresion.2)
stargazer(regresion.2, type = "text")

# Residuos del modelo con TIB10
residuales.2 <- resid(regresion.2)
plot(residuales.2, col = "blue", lwd = 1.5, main = "Residuales 6 Rezagos TIB10")
abline(h = 0, lwd = 2, col = "red")

# Test de DF
test.mod2.ur <- ur.df(residuales.2, type = c("none"), lags = 1, selectlags = c("AIC"))
summary(test.mod2.ur)

# Corroboramos
ndiffs(residuales.2)
# Conclusión: el modelo de 6 rezagos de TIB10 es de orden de cointegración 1, por tanto, habría relación al LP.

# Modelo de Rezagos con TIB1
regresion.3 <- dynlm(diff.lTIB1 ~ L(diff.lTIB10,1:6) + L(diff.lTIB1,1:6) + residuales, data = db.ts)
summary(regresion.3)
stargazer(regresion.3, type = "text")

# Residuos del modelo con TIB1
residuales.3 <- resid(regresion.3)
plot(residuales.3, col = "blue", lwd = 1.5, main = "Residuales 6 Rezagos TIB1")
abline(h = 0, lwd = 2, col = "red")

# Test de DF
test.mod3.ur <- ur.df(residuales.3, type = c("none"), lags = 1, selectlags = c("AIC"))
summary(test.mod3.ur)

# Corroboramos
ndiffs(residuales.3)
# Conclusión: el modelo de 6 rezagos de TIB1 es de orden de cointegración 0, por tanto, no habría relación al LP.

