#----------------------------------------------#
#******* Ejercicio Empírico 4           *******#
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
library(urca)

# Seteamos nuestro directorio
setwd("~/OneDrive - Universidad Alberto Hurtado/UNIVERSIDAD UAH/4to Año/8vo Semestre/Macroeconometría/Tareas/Ejercicio Empirico 4")

# Cargamos la base de datos
db <- read_excel("~/OneDrive - Universidad Alberto Hurtado/UNIVERSIDAD UAH/4to Año/8vo Semestre/Macroeconometría/Tareas/Ejercicio Empirico 4/datos_ejercicio_empirico_4.xlsx", 
  col_types = c("date", "numeric", "numeric", 
    "numeric", "numeric", "numeric"))
View(db)

attach(db)

#******* Pruebas de Estacionalidad (grafico) *******#

#******* Consumo *******#
c_ts <- ts(db$consumo, start = 1986, frequency = 4)
# Log(Consumo)
lg_cts <- log(c_ts)
# Primeras diferencias
dif_lg_cts <- diff(lg_cts, lag = 1)
# Tendencia
tendencia_cts <- time(lg_cts)

# graficas de acf y plot
acf(lg_cts,
  type = "correlation",
  plot = TRUE, col = "red",
  main = "ACF Ln(Consumo))")

acf(dif_lg_cts,
  type = "correlation",
  plot = TRUE,
  col = "red",
  main = "ACF Diff(Ln(Consumo))")

plot(lg_cts,
  main = "Gráfica 1",
  xlab = "Tiempo en Trimestres",
  ylab = "Ln(Consumo)",
  col = "blue")

plot(dif_lg_cts,
  main = "Gráfica 2",
  xlab = "Tiempo en Trimestres",
  ylab = "Diff(Ln(Consumo))",
  col = "red")

# Test Raiz Unitario - Consumo
test_ur1_cts <- ur.df(lg_cts, type = c("trend"), selectlags = "AIC")
summary(test_ur1_cts)

ndiffs(lg_cts, type = c("trend"))
# es de orden 1 la integración.

# test R.U. Dif(log(consumo))
test_ur2_dif_cts <- ur.df(dif_lg_cts, type = c("trend"), selectlags = "AIC")
summary(test_ur2_dif_cts)

ndiffs(dif_lg_cts, type = c("trend"))

#******* Inversión *******#
i_ts <- ts(db$inversion, start = 1986, frequency = 4)
# Log(inversión)
lg_its <- log(i_ts)
# Primeras diferencias
dif_lg_its <- diff(lg_its, lag = 1)

# graficas de acf y plot
acf(lg_its,
  type = "correlation",
  plot = TRUE, col = "red",
  main = "ACF Ln(Inverión))")

acf(dif_lg_its,
  type = "correlation",
  plot = TRUE,
  col = "red",
  main = "ACF Diff(Ln(Inversión))")

plot(lg_its,
  main = "Gráfica 1",
  xlab = "Tiempo en Trimestres",
  ylab = "Ln(Inversión)",
  col = "blue")

plot(dif_lg_its,
  main = "Gráfica 2",
  xlab = "Tiempo en Trimestres",
  ylab = "Diff(Ln(Inversión))",
  col = "red")

# Test Raiz Unitario - Inversión
test_ur1_its <- ur.df(lg_its, type = c("trend"), selectlags = "AIC")
summary(test_ur1_its)

ndiffs(lg_its, type = c("trend"))
# es de orden 1 la integración.

# test R.U. Dif(log(inversión))
test_ur2_dif_its <- ur.df(dif_lg_its, type = c("trend"), selectlags = "AIC")
summary(test_ur2_dif_its)

ndiffs(dif_lg_its, type = c("trend"))

#******* Exportaciones *******#
e_ts <- ts(db$exportaciones, start = 1986, frequency = 4)
# Log(Exportaciones)
lg_ets <- log(e_ts)
# Primeras diferencias
dif_lg_ets <- diff(lg_ets, lag = 1)

# graficas de acf y plot
acf(lg_ets,
  type = "correlation",
  plot = TRUE, col = "red",
  main = "ACF Ln(Exportaciones))")

acf(dif_lg_ets,
  type = "correlation",
  plot = TRUE,
  col = "red",
  main = "ACF Diff(Ln(Exportaciones))")

plot(lg_ets,
  main = "Gráfica 1",
  xlab = "Tiempo en Trimestres",
  ylab = "Ln(Exportaciones)",
  col = "blue")

plot(dif_lg_ets,
  main = "Gráfica 2",
  xlab = "Tiempo en Trimestres",
  ylab = "Diff(Ln(Exportaciones))",
  col = "red")

# Test Raiz Unitario - Exportaciones
test_ur1_ets <- ur.df(lg_ets, type = c("trend"), selectlags = "AIC")
summary(test_ur1_ets)

ndiffs(lg_ets, type = c("trend"))
# es de orden 1 la integración.

# test R.U. Dif(log(exportaciones))
test_ur2_dif_ets <- ur.df(dif_lg_ets, type = c("trend"), selectlags = "AIC")
summary(test_ur2_dif_ets)

ndiffs(dif_lg_ets, type = c("trend"))

#******* Importaciones *******#
im_ts <- ts(db$importanciones, start = 1986, frequency = 4)
# Log(Importaciones)
lg_imts <- log(im_ts)
# Primeras diferencias
dif_lg_imts <- diff(lg_imts, lag = 1)

# graficas de acf y plot
acf(lg_imts,
  type = "correlation",
  plot = TRUE, col = "red",
  main = "ACF Ln(Importaciones))")

acf(dif_lg_imts,
  type = "correlation",
  plot = TRUE,
  col = "red",
  main = "ACF Diff(Ln(Importaciones))")

plot(lg_imts,
  main = "Gráfica 1",
  xlab = "Tiempo en Trimestres",
  ylab = "Ln(Importaciones)",
  col = "blue")

plot(dif_lg_imts,
  main = "Gráfica 2",
  xlab = "Tiempo en Trimestres",
  ylab = "Diff(Ln(Importaciones))",
  col = "red")

# Test Raiz Unitario - Importaciones
test_ur1_imts <- ur.df(lg_imts, type = c("trend"), selectlags = "AIC")
summary(test_ur1_imts)

ndiffs(lg_imts, type = c("trend"))
# es de orden 1 la integración.

# test R.U. Dif(log(importaciones))
test_ur2_dif_imts <- ur.df(dif_lg_imts, type = c("trend"), selectlags = "AIC")
summary(test_ur2_dif_imts)

ndiffs(dif_lg_imts, type = c("trend"))

#******* PIB *******#
pib_ts <- ts(db$pib, start = 1986, frequency = 4)
# Log(Consumo)
lg_pibts <- log(pib_ts)
# Primeras diferencias
dif_lg_pibts <- diff(lg_pibts, lag = 1)

# graficas de acf y plot
acf(lg_pibts,
  type = "correlation",
  plot = TRUE, col = "red",
  main = "ACF Ln(PIB))")

acf(dif_lg_pibts,
  type = "correlation",
  plot = TRUE,
  col = "red",
  main = "ACF Diff(Ln(PIB))")

plot(lg_pibts,
  main = "Gráfica 1",
  xlab = "Tiempo en Trimestres",
  ylab = "Ln(PIB)",
  col = "blue")

plot(dif_lg_pibts,
  main = "Gráfica 2",
  xlab = "Tiempo en Trimestres",
  ylab = "Diff(Ln(PIB))",
  col = "red")

# Test Raiz Unitario - PIB
test_ur1_pibts <- ur.df(lg_pibts, type = c("trend"), selectlags = "AIC")
summary(test_ur1_pibts)

ndiffs(lg_pibts, type = c("trend"))
# es de orden 1 la integración.

# test R.U. Dif(log(PIB))
test_ur2_dif_pibts <- ur.df(dif_lg_pibts, type = c("trend"), selectlags = "AIC")
summary(test_ur2_dif_pibts)

ndiffs(dif_lg_pibts, type = c("trend"))
