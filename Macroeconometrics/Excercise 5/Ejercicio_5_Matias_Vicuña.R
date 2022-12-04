#----------------------------------------------#
#******* Ejercicio Empírico 5           *******#
#******* Macroeconometría               *******#
#******* Profesor: Mauricio Tejada      *******#
#******* Ayudante: Diego Trauranca      *******#
#******* Estudiante: Matías Vicuña      *******#
#******* Universidad Alberto Hurtado    *******#
#----------------------------------------------#

# Reseteamos nuestro enviroment de trabajo
rm(list = ls())

# Seteamos las librerías a utilizar
library(dynlm)
library(tidyverse)
library(readxl)
library(stargazer)
library(dygraphs)
library(forecast)
library(AER)
library(urca)
library(vars)

# Seteamos el directorio de trabajo de nuestro script
setwd("~/OneDrive - Universidad Alberto Hurtado/UNIVERSIDAD UAH/4to Año/8vo Semestre/Macroeconometría/Tareas/Ejercicio Empirico 5")

# Procedemos a cargar la data
db <- read_xls("Datos_Blanchard_Quah.xls")
db

attach(db)

# Generamos las series de tiempo
unrate <-
  ts(
    db$unrate,
    start = c(1950, 2),
    end = c(1987, 4),
    frequency = 4
  )

unrate <- window(unrate, start = c(1950,3), end = c(1987, 4))

gnp <- ts(
  db$gnp,
  start = c(1950, 2),
  end = c(1987, 4),
  frequency = 4
)

# Series de tiempo linealizadas
lgnp.t <- log(gnp)
lunrate.t <- log(unrate)

# Transformamos la data en la primera diferencia linealizada de cada una
dlgnp.t <- diff(log(gnp), 1)
dlunrate.t <- diff(log(unrate), 1)*100

# la agrupamos en una data
data <- ts.union(lunrate.t, dlgnp.t, dframe = FALSE)

# visualizamos la data linealizada
plot(
  data,
  col = "blue",
  main = "Primeras diferencias linealizadas",
  xlab = "Tiempo (Trimestral)",
  lwd = 2
)

#********************** PARTE 1 **********************#
# Verificacion de estacionariedad mediante test de raiz unitaria.

# TEST UNRATE modelo 3
test.unrate.ur <-
  ur.df(
    unrate,
    type = c("trend"),
    lags = 1,
    selectlags = c("AIC")
  )
summary(test.unrate.ur)

# TEST UNRATE modelo 2
test.unrate.ur.2 <-
  ur.df(
    unrate,
    type = c("drift"),
    lags = 1,
    selectlags = c("AIC")
  )
summary(test.unrate.ur.2)

# TEST UNRATE modelo 1
test.unrate.ur.1 <-
  ur.df(
    unrate,
    type = c("none"),
    lags = 1,
    selectlags = c("AIC")
  )
summary(test.unrate.ur.1)

ndiffs(unrate, type = c("trend"))
# Conclusiones, tenemos que nuestro test de raiz unitaria de unrate es de orden de integracion 1, por lo que si hay estacionariedad.

# TEST GNP
test.gnp.ur <-
  ur.df(
    gnp,
    type = c("trend"),
    lags = 1,
    selectlags = c("AIC")
  )
summary(test.gnp.ur)

# TEST GNP modelo 2
test.gnp.ur.2 <-
  ur.df(
    gnp,
    type = c("drift"),
    lags = 1,
    selectlags = c("AIC")
  )
summary(test.gnp.ur.2)

# TEST GNP modelo 1
test.gnp.ur.1 <-
  ur.df(
    gnp,
    type = c("none"),
    lags = 1,
    selectlags = c("AIC")
  )
summary(test.gnp.ur.1)

ndiffs(gnp, type = c("trend"))
# Conclusiones, tenemos que nuestro test de raiz unitaria de gnp es de orden de integracion 1, por lo que si hay estacionariedad.

#********************** PARTE 2 **********************#
# Estimacion de modelo VAR simplificado, evaluando estabilidad y ruido blanco
# Procedemos a usar la matriz "data" para nuestra estimacion del VAR.

# Modelo VAR reducido
VAR.1 <- VAR(y = data, p = 1)
summary(VAR.1)
coef(VAR.1)

# Estimacion del mejor numero de rezagos mediante criterio de informacion AIC
VARselect(y = data,
  lag.max = 12,
  season = 4,
  type = "const")
# Comprobamos que nuestro mejor modelo de VAR es tomando en cuenta 6 rezagos como max segun el criterio de minimizacion de AIC

VAR.1.opt <- VAR(y = data,
  p = 6 ,
  type = "const")
summary(VAR.1.opt)
plot(VAR.1.opt)

# Estabilidad del VAR (VAR Óptimo (VAR 6))
barplot(
  roots(VAR.1.opt),
  ylim = c(0, 1),
  main = "Estabilidad: Raíces de Polinomio Característico",
  names.arg = as.character(1:12)
)
abline(h = 1,
  col = "red",
  lty = 2,
  lwd = 2)
# Conclusion: al ser todos los polinomios caracteristicos del modelo menores a 1, comprobamos que nuestro modelo VAR si presenta estabilidad.

# Normalidad de los errores, pruebas de distribución
normalidad <- normality.test(VAR.1.opt)
normalidad

# H0 = Los errores distribuyen normalmente. > 0.05
# H1 = Los errores no distribuyen con normalidad. < 0.05
# Conclusión: Tenemos que la prueba Skewness con p-value de 0.885, por tanto, rechazamos hipotesis alternativa, los errores se distribuyen normal.

# Prueba de Autocorrelación Serial
test.res.serial <- serial.test(VAR.1.opt, lags.pt = 12, type = c("PT.asymptotic"))
test.res.serial

# H0 = Los residuales no estan correlacionados. > 0.05
# H1 = Los residuales si estan correlacionados. < 0.05
# Conclusión: Dado que nuestro test de autocorrelacion serial presenta un p-value de 0.08537, no rechazamos la hipotesis nula y por tanto, nuestro modelo no presenta problemas de correlacion serial entre los residuales.

# Errores del modelo VAR(4)
u <- resid(VAR.1.opt)

ts.plot(u[,1], xlab = "Trimestres", ylab = "Errores", 
  main = "Error de Unrate", col = "blue", lwd = 2)
abline(h = 0, lwd = 2.5, col = "red")

ts.plot(u[,2], xlab = "Trimestres", ylab = "Errores", 
  main = "Errores de GNP", col = "blue", lwd = 2)
abline(h = 0, lwd = 2.5, col = "red")

#********************** PARTE 3 **********************#
# Impulso = unrate; Respuesta = gnp

# Generamos la matriz con diagonal inferior izquierda.
b <- diag(1,2)
b[lower.tri(b)] <- NA

var.opt.model.b <- SVAR(VAR.1.opt, Bmat = b)

var.opt.model.b$B

irf.i <-
  irf(
    var.opt.model.b,
    impulse = "lunrate.t",
    response = "dlgnp.t",
    n.ahead = 24,
    ortho = TRUE
  )

plot(irf.i,
  main = "Impulso = Tasa de Desempleo; Respuesta = Crecimiento producto nacional bruto",
  ylab = "Log(GNP)",
  lwd = 2)

irf.ii <-
  irf(
    var.opt.model.b,
    impulse = "dlgnp.t",
    response = "lunrate.t",
    n.ahead = 24,
    ortho = TRUE
  )

plot(irf.ii,
  main = "Impulso = Crecimiento producto nacional bruto; Respuesta = Tasa de Desempleo",
  ylab = "Log(unrate)",
  lwd = 2)

#********************** PARTE 4 **********************#
# Análisis de descomposición de la varianza del error de predicción:
des.var <- fevd(var.opt.model.b, n.ahead = 12)

plot(des.var, xlab = c("Horizonte","Horizonte"), 
  ylab = c("Porcentaje","Porcentaje"), 
  legend = c("Desempleo","Crec. PIB"),
  main = c("Varianza Desempleo", "Varianza Crecimiento PIB"))

#********************** PARTE 5 **********************#

var.opt.model.BQ <- BQ(VAR.1.opt)

irf.bq <- irf(var.opt.model.BQ,
  impulse = "lunrate.t",
  response = "dlgnp.t",
  n.ahead = 24,
  ortho = TRUE
)
plot(irf.bq,
  main = "Impulso = Tasa de Desempleo; Respuesta = Crecimiento producto nacional bruto",
  ylab = "Log(GNP)",
  lwd = 2)

irf.ii.bq <-
  irf(
    var.opt.model.BQ,
    impulse = "dlgnp.t",
    response = "lunrate.t",
    n.ahead = 24,
    ortho = TRUE
  )

plot(irf.ii.bq,
  main = "Impulso = Crecimiento producto nacional bruto; Respuesta = Tasa de Desempleo",
  ylab = "Log(unrate)",
  lwd = 2)

#********************** PARTE 6 **********************#
# Análisis de descomposición de la varianza del error de predicción:
des.var.ii <- fevd(var.opt.model.BQ, n.ahead = 12)

plot(des.var.ii, xlab = c("Horizonte","Horizonte"), 
  ylab = c("Porcentaje","Porcentaje"), 
  legend = c("Desempleo","Crec. PIB"),
  main = c("Varianza Desempleo", "Varianza Crecimiento PIB"))

#********************** PARTE 7 **********************#
# Comentarios en el Documento PDF adjunto.



