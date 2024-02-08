#----------------------------------------------#
#******* Tarea Empírica 2               *******#
#******* Macroeconometría               *******#
#******* Profesor: Mauricio Tejada      *******#
#******* Ayudante: Diego Trauranca      *******#
#******* Estudiante: Matías Vicuña      *******#
#******* Universidad Alberto Hurtado    *******#
#----------------------------------------------#

# Seteamos las librerías a utilizar
library(dynlm)
library(tidyverse)
library(readxl)
library(stargazer)
library(dygraphs)

# Seteamos nuestro directorio
setwd("~/OneDrive - Universidad Alberto Hurtado/UNIVERSIDAD UAH/4to Año/8vo Semestre/Macroeconometría/Tareas/Tarea 2")

# Generamos la base de datos
db <- read_xlsx("datos_ejercicio_empirico_2.xlsx")

# Guardamos en la memoria
attach(db)

# Eliminamos variable year
db$year = NULL

# Generamos la serie de tiempo con la data
db_ts <- ts(db, start = c(1981,1), frequency = 12)

#******* Parte 1 *******#

# Usamos la data y creamos una nueva data con las variables de interés.
db_1 <- db %>% 
  select(beltlaw,spdlaw)

# Transformamos a Time Series
db_1_ts <- ts(db_1, start = c(1981, 1), frequency = 12)

# Gráfica interactiva y muestral.
dygraph(db_1_ts)

plot(db_1_ts,
  main = "Efectos de Aplicación de la Ley de Cinturón y Velocidad Límite",
  xlab = "Periodo en Meses",
  col = "red")

#******* Parte 2 *******#

# Regresion del modelo de ts con ajuste de tendencia.
regress1 <- dynlm(log(totacc) ~ trend(db_ts), data = db_ts)

# Tabla de datos de la regresión
  stargazer(regress1, type = "text", no.space = TRUE, single.row = FALSE,  intercept.bottom = FALSE, 
    digits = 2, keep.stat = c("n", "rsq"))

# Grafico de los Residuos
plot(
  regress1$residuals,
  main = "Gráfico de los Residuos",
  xlab = "Tiempo (Meses)",
  ylab = "Residuos",
  col = "blue"
)

#******* Parte 3 *******#

# regresion del modelo de ts con datos regulares
regress2 <-
  dynlm(log(totacc) ~ trend(db_ts) + spdlaw + beltlaw + unem + wkends, data = db_ts)

stargazer(regress2, type = "text", no.space = TRUE, single.row = FALSE,  intercept.bottom = FALSE, 
  digits = 2, keep.stat = c("n", "rsq"))

# grafico de los residuos
plot(regress2$residuals,
  main = "Gráfico de los Residuos",
  xlab = "Tiempo (Meses)",
  ylab = "Residuos",
  col = "green"
  )

#******* Parte 4 *******#
stargazer(regress2, type = "text", no.space = TRUE, single.row = FALSE,  intercept.bottom = FALSE, 
  digits = 2, keep.stat = c("n", "rsq"))

"========================================
                 Dependent variable:    
             ---------------------------
                     log(totacc)        
----------------------------------------
Constant              10.63***          
                       (0.08)           
trend(db_ts)           0.02***          
                       (0.004)          
spdlaw                -0.06***          
                       (0.02)           
beltlaw                0.07***          
                       (0.02)           
unem                  -0.03***          
                       (0.005)          
wkends                 0.01**           
                       (0.005)          
----------------------------------------
Observations             108            
R2                      0.80            
========================================
Note:        *p<0.1; **p<0.05; ***p<0.01"

#******* Parte 5 *******#

regress3 <-
  dynlm(prcfat ~ trend(db_ts) + spdlaw + beltlaw + unem + wkends, data = db_ts)

stargazer(regress3, type = "text", no.space = TRUE, single.row = FALSE,  intercept.bottom = FALSE, 
  digits = 2, keep.stat = c("n", "rsq"))

"========================================
                 Dependent variable:    
             ---------------------------
                       prcfat           
----------------------------------------
Constant               1.03***          
                       (0.14)           
trend(db_ts)          -0.03***          
                       (0.01)           
spdlaw                 0.08**           
                       (0.03)           
beltlaw                 -0.05           
                       (0.03)           
unem                   -0.02**          
                       (0.01)           
wkends                  0.01            
                       (0.01)           
----------------------------------------
Observations             108            
R2                      0.24            
========================================
Note:        *p<0.1; **p<0.05; ***p<0.01"

plot(regress3$residuals,
  main = "Gráfico de los Residuos",
  xlab = "Tiempo (Meses)",
  ylab = "Residuos",
  col = "purple"
)

#******* Parte 6 *******#

regress4 <- dynlm(log(totacc) ~ season(db_ts) + trend(db_ts), data = db_ts)

stargazer(regress4, type = "text", no.space = TRUE, single.row = FALSE,  intercept.bottom = FALSE, 
  digits = 2, keep.stat = c("n", "rsq"))

"============================================
                     Dependent variable:    
                 ---------------------------
                         log(totacc)        
--------------------------------------------
Constant                  10.47***          
                           (0.02)           
season(db_ts)Feb           -0.04*           
                           (0.02)           
season(db_ts)Mar           0.08***          
                           (0.02)           
season(db_ts)Apr            0.02            
                           (0.02)           
season(db_ts)May            0.03            
                           (0.02)           
season(db_ts)Jun            0.02            
                           (0.02)           
season(db_ts)Jul            0.04            
                           (0.02)           
season(db_ts)Aug           0.05**           
                           (0.02)           
season(db_ts)Sep            0.04*           
                           (0.02)           
season(db_ts)Oct           0.08***          
                           (0.02)           
season(db_ts)Nov           0.07***          
                           (0.02)           
season(db_ts)Dec           0.10***          
                           (0.02)           
trend(db_ts)               0.03***          
                           (0.002)          
--------------------------------------------
Observations                 108            
R2                          0.80            
============================================
Note:            *p<0.1; **p<0.05; ***p<0.01"
