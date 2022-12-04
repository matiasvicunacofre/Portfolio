#******** Ayudantía 3 - Regresión Simple - Significancia ********#
#********     Profesor: Rodrigo Ortiz                    ********#
#********     Ayudante: Matías Vicuña                    ********#
#******** Mod Regresión Multivariada en R                ********#

# En caso de no poseer las librerías, correr este código
install.packages(c("tidyverse", "stargazer", "wooldridge", "lmtest", "car"))

# Librerías a usar
library(tidyverse)
library(stargazer)
library(wooldridge)
library(lmtest)
library(car)

# Cargamos la data "wage2" del libro de wooldridge
db <- wooldridge::wage2

# estaditica descriptiva global
summary(db)

# Visualización gráfica de los datos a usar

# método 1: histograma (paquete "base" de R)
hist(x = db$wage) # histograma de los salarios
hist(x = db$IQ) # histograma de IQ
hist(x = db$exper) # histograma de la experiencia en años

# método 2: gráfica de densidad (paquete "ggplot2" de "tidyverse")

# Densidad de IQ
plot_iq <- ggplot(db)+
  geom_density(aes(x = db$IQ),
    col = "blue",
    size = 1.5)+
  labs(x = "IQ",
    y = "Densidad",
    title = "Densidad de IQ")+
  theme_classic()
plot_iq

# Densidad de Experiencia
plot_ex <- ggplot(db)+
  geom_density(aes(x = db$exper),
    col = "blue",
    size = 1.5)+
  labs(x = "Experiencia",
    y = "Densidad",
    title = "Densidad de Experiencia")+
  theme_classic()
plot_ex

# Densidad de Salarios
plot_wg <- ggplot(db)+
  geom_density(aes(x = db$wage),
    col = "blue",
    size = 1.5)+
  labs(x = "Salarios",
    y = "Densidad",
    title = "Densidad de Salarios")+
  theme_classic()
plot_wg

#******* Iniciamos nuestra primera regresión con y = "wage" y x = "IQ", con ello buscamos explicar si los niveles de salario anual es explicado por el IQ de la persona. *******#

# Formulamos el modelo:
reg_1 <- lm(wage ~ IQ, data = db)

# Vemos el test de la normalidad de los residuos
residuos.reg1 <- reg_1$residuals
shapiro.test(residuos.reg1)

# H0: los errores son distrubuidos como una normal
# H1: los errores no son normales

# Resultado: p-value < 2.2e-16 < 1% significancia = Se rechaza H0, los errores no son normales.

# Visualizamos los resultados
summary(reg_1) # método simple

# resultados
# Call:
#   lm(formula = wage ~ IQ, data = db)

# Residuals:
#  Min     1Q Median     3Q    Max 
# -898.7 -256.5  -47.3  201.1 2072.6 

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 116.9916    85.6415   1.366    0.172    
# IQ            8.3031     0.8364   9.927   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 384.8 on 933 degrees of freedom
# Multiple R-squared:  0.09554,	Adjusted R-squared:  0.09457 
# F-statistic: 98.55 on 1 and 933 DF,  p-value: < 2.2e-16

stargazer(reg_1, type = "text") # formato tabla

# test de homocedasticidad
white.1 <- bptest(reg_1)
white.1

#H0: varianza de los errores constante = errores homocedasticos
#H1: la varianza de los errores no es constante =  errores no homocedasticos = errores heterocedasticos

# Resultado: p-value = 0.0009132 < 1%, se rechaza H0, por tanto hay heterocedasticidad.

#******* Segundo modelo *******#
reg_2 <- lm(wage ~ exper, data = db)

# Test de residuos
residuos.reg2 <- reg_2$residuals

shapiro.test(residuos.reg2)

# Resultado: p-value < 2.2e-16 < 1%, por tanto se rechaza H0: los errores no se distribuyen normalmente.

# Visualización de regresión
summary(reg_2)

# Resultado:
# Call:
#   lm(formula = wage ~ exper, data = db)

# Residuals:
#  Min      1Q  Median      3Q     Max 
# -842.43 -289.13  -52.84  201.86 2120.17 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 955.6049    37.4111  25.543   <2e-16 ***
# exper         0.2024     3.0261   0.067    0.947    
# ---
#  Signif. codes:  
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 404.6 on 933 degrees of freedom
# Multiple R-squared:  4.795e-06,	Adjusted R-squared:  -0.001067 
# F-statistic: 0.004474 on 1 and 933 DF,  p-value: 0.9467

stargazer(reg_2, type = "text")

# Test de homocedasticidad
white.2 <- bptest(reg_2)
white.2

# Resultado: p-value = 0.1836 > 5% significancia, por tanto se rechaza H1, el modelo posee homocedasticidad.