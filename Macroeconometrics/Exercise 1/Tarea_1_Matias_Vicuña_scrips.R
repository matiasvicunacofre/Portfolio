#----------------------------------------------#
#******* Tarea Empírica 1               *******#
#******* Macroeconometría               *******#
#******* Profesor: Mauricio Tejada      *******#
#******* Ayudante: Diego Trauranca      *******#
#******* Estudiante: Matías Vicuña      *******#
#******* Universidad Alberto Hurtado    *******#
#----------------------------------------------#

# Librerias a utilizar
library(readxl)
library(lubridate)
library(tidyverse)
library(stargazer)
library(dygraphs)

# Llamamos la data
db <- read_xlsx("datos_ejercicio_empirico_1.xlsx")
View(db)

# guardamos en memoria la data.
attach(db)

# Trabajamos la data como data frame.
db_df <- as.data.frame(db)

# Tranformamos la variable periodo para tener el formato
db_df_2 <- db_df %>% 
  mutate(Periodo = ymd(db_df$periodo))
View(db_df_2)

# Eliminamos la variable "periodo" por temas de comodidad de trabajo de data.
db_df_2$Periodo <- NULL

# Definimos la data como time series
db_ts <- ts(data = db_df_2, start = c(1985, 1), frequency = 12)
View(db_ts)
help(ts)

# Grafica simple de ambos precios
plot(db_ts,
  xlab = "Tiempo en años",
  main = "Precios desde 1985 al 2022",
  col = "blue")

#******* Parte 1: graficos en logaritmo de cada variable. *******#
# Precio bencina Linealizado

# Copiamos la data en un nuevo objeto para bencina
db_bencina <- db_df_2

# Eliminamos la variable "petroleo"
db_bencina$petroleo <- NULL

# Creamos el logaritmo de los precios historicos.
db_bencina <- db_bencina %>% 
  mutate(Log_bencina = log(db_bencina$bencina))

# Nuevamente, eliminamos la variable "bencina", dejando solo la variable Log_bencina.
db_bencina$bencina <- NULL
db_bencina$periodo <- NULL

# Pasamos la data de data frame a matrix para uso de Time-Series.
dbts_bencina <- ts(db_bencina, start = c(1985,1), frequency = 12)

# Acotamos la serie de tiempo
dbts_bencina <- window(dbts_bencina, start = c(1990,1), end = c(2021,12))

# Creamos la grafica historica de los precios de la bencina.
plot_bencina <-
  plot(
    dbts_bencina,
    main = "Precio Histórico Linealizado Bencina (1990-2022)",
    xlab = "Tiempo en meses",
    ylab = "Logaritmo del Precio",
    col = "red"
  )

# Precio Petroleo Linealizado

# Copiamos la data en un nuevo objeto para petroleo
db_petroleo <- db_df_2

# Eliminamos la variable "bencina"
db_petroleo$bencina <- NULL
db_petroleo$periodo <- NULL

# Creamos el logaritmo de los precios historicos.
db_petroleo <- db_petroleo %>% 
  mutate(Log_petroleo = log(db_petroleo$petroleo))

# Nuevamente, eliminamos la variable "petroleo", dejando solo la variable Log_petroleo.
db_petroleo$petroleo <- NULL

# Pasamos la data de data-frame a matrix para uso de Time-Series.
dbts_petroleo <- ts(db_petroleo, start = c(1985,1), frequency = 12)

# Acotamos la serie de tiempo
dbts_petroleo <- window(dbts_petroleo, start = c(1990,1), end = c(2021,12))

# Creamos la grafica historica de los precios de la bencina.
plot_petroleo <-
  plot(
    dbts_petroleo,
    main = "Precio Histórico Linealizado Petróleo (1990-2022)",
    xlab = "Tiempo en meses",
    ylab = "Logaritmo del Precio",
    col = "blue"
  )

#******* Parte 2: gráfico de correlación *******#

# Precio del Petroleo (Linealizado)

# Transformamos la matriz a data frame
db_ts_petr <- as.data.frame(dbts_petroleo)

# Eliminamos la variable periodo
db_ts_petr$periodo <- NULL

# Transformamos la nueva data en Time-Series
db_ts_petr <- ts(db_ts_petr, start = c(1990,1), frequency = 12)

# Correlación del Precio del Petroleo Linealizado
acf(
  db_ts_petr,
  lag.max = 10,
  type = "correlation",
  plot = TRUE,
  main = "Correlación del Precio del Petróleo",
  xlab = "Rezagos en meses",
  ylab = "Correlación"
)

# Precio de la bencina (Linealizado)

# Transformamos la matriz a data frame
db_ts_benc <- as.data.frame(dbts_bencina)

# Eliminamos la variable periodo
db_ts_benc$periodo <- NULL

# Transformamos la nueva data en Time-Series
db_ts_benc <- ts(db_ts_benc, start = c(1990,1), frequency = 12)

# Correlación del Precio de la bencina Linealizado
acf(
  db_ts_benc,
  lag.max = 10,
  type = "correlation",
  plot = TRUE,
  main = "Correlación del Precio de la Bencina",
  xlab = "Rezagos en meses",
  ylab = "Correlación"
)

#******* Parte 3 - Primeras Diferencias  *******#

# Primera diferencia de log(petroleo)
dif_petroleo <- diff(db_ts_petr, lag = 1)

plot(dif_petroleo, main = "Primera diferencia de la Petroleo", xlab = "Tiempo en años", ylab = "Log(Petróleo)", col = "red")

# Primera diferencia de log(bencina)
dif_bencina <- diff(db_ts_benc, lag = 1)

plot(dif_bencina,main = "Primera diferencia de la Bencina", xlab = "Tiempo en años", ylab = "Log(Bencina)", col = "blue")

#******* Parte 4 - Autocorrelación Primeras Diferencias *******#

# ACF de la primera diferencia del petroleo
acf(
  dif_petroleo,
  lag.max = 10,
  type = "correlation",
  plot = TRUE,
  main = "Correlación Primera Diferencia del Petroleo",
  xlab = "Rezagos",
  ylab = "Correlación"
)

# ACF de la primera diferencia de la bencina
acf(
  dif_bencina,
  lag.max = 10,
  type = "correlation",
  plot = TRUE,
  main = "Correlación Primera Diferencia de la Bencina",
  xlab = "Rezagos",
  ylab = "Correlación"
)

#******* Parte 5 - Agrupamiento anual de la información *******#

# Datos Promedio Anual de la Bencina
anual_ts_bencina <- aggregate(db_ts_benc, nfrequency = 1, FUN = mean)

plot(anual_ts_bencina, main = "Promedio Anual Linealizado de la Bencina", ylab = "Log(Bencina)", xlab = "Tiempo en años", col = "blue")

# Datos Promedio Anual del Petróleo
anual_ts_petroleo <- aggregate(db_ts_petr, nfrequency = 1, FUN = mean)

plot(anual_ts_petroleo, main = "Promedio Anual Linealizado del Petróleo", ylab = "Log(Petroleo)", xlab = "Tiempo en años", col = "red")
