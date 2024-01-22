#******** Interpretacion Significancia Estimadores ********#
#******** Autor: Matías Vicuña                     ********#

# En caso de no poseer las librerías, correr este código
install.packages(c("tidyverse", "stargazer", "wooldridge", "lmtest", "car"))

# Librerías a usar
library(tidyverse)
library(stargazer)
library(wooldridge)
library(lmtest)
library(car)    

# Eliminamos todo el enviroment y liberamos la memoria.
rm(list = ls())

# Usaremos la data "smoke" del paquete wooldridge para realizar nuestro modelo de regresion lineal.
wooldridge::smoke
?smoke

data(smoke)

# De la base de datos, sacamos los datos de interes.
ingresos <- smoke$income
cigarros <- smoke$cigs 
educacion <- smoke$educ
cigprecio <- smoke$cigpric

# A partir de los vectores, realizamos una concatenacion de las columnas y lo transformamos a un data frame.
data <- as.data.frame(cbind(cigprecio, cigarros, educacion, ingresos))
View(data)

# Primero, vemos de manera grafica los datos a usar mediante un histograma
hist(ingresos) # ingresos anuales
hist(cigarros) # cigarros fumados al dia
hist(educacion) # años de educacion
hist(cigprecio) # Precio del cigarrillo

# Ahora, veremos el mismo nivel de densidad con el paquete ggplot de tidyverse
# Densidad ingresos
plot1 <- ggplot(data) +
  geom_density(aes(x = ingresos),
    col = "blue",
    size = 1.5) +
  labs(x = "ingresos",
    y = "Densidad",
    title = "Densidad de ingresos") +
  theme_classic()
plot1

# Densidad cigarros
plot2 <- ggplot(data) +
  geom_density(aes(x = cigarros),
    col = "blue",
    size = 1.5) +
  labs(x = "cigarros",
    y = "Densidad",
    title = "Densidad de cigarros") +
  theme_classic()
plot2

# Densidad educacion
plot3 <- ggplot(data) +
  geom_density(aes(x = educacion),
    col = "blue",
    size = 1.5) +
  labs(x = "educacion",
    y = "Densidad",
    title = "Densidad de educacion") +
  theme_classic()
plot3

# Densidad cigprecio
plot4 <- ggplot(data) +
  geom_density(aes(x = cigprecio),
    col = "blue",
    size = 1.5) +
  labs(x = "cigprecio",
    y = "Densidad",
    title = "Densidad de cigprecio") +
  theme_classic()
plot4

#******* Iniciamos nuestra primera regresión con y = "cigprecio" y x = "educacion", con ello buscamos explicar si los niveles del precio de los cigarrillos por años de eduacion. *******#

## MODELO 1 ##
reg1 <- lm(data = data, cigprecio ~ educacion)
summary(reg1)
stargazer(reg1, type = "text")

# Visualizacion grafica del modelo
plot(
  x = data$educacion,
  y = cigprecio,
  main = "Dispersión Educación-Precio del Cigarrillo",
  xlab = "Educación",
  ylab = "Precio Cigarillo"
)
abline(reg1, col = "red", lwd = 2)

## Tests aplicables al modelo ##

# Normalidad de los Residuos
residuos.reg1 <- reg1$residuals

shapiro.test(residuos.reg1)

# H0: Errores se distribuyen normalmente >= 0.05 de significancia
# H1: Errores no se distribuyen normalmente < 0.05 de significancia

# Resultado: p-value < 2.2e-16 < 1%, por tanto se rechaza H0: los errores no se distribuyen normalmente.
# p-value = 0.0000000000000022 < 0.05

# Grafica de densidad de los residuos.
plot(residuos.reg1)

ggplot(reg1)+
  geom_density(aes(x = reg1$residuals), col = "blue", size = 2)+
  theme_classic()

# Test de Homocedasticidad
white.1 <- bptest(reg1)
white.1

# H0: varianza de los errores constante = errores homocedasticos >= 0.05 de significancia
# H1: la varianza de los errores no es constante =  errores no homocedasticos = errores heterocedasticos < 0.05 de significancia

# Resultado: p-value = 0.1733 > 5%, no se rechaza H0, por tanto hay Homocedasticidad.
# p-value = 0.1733 > 0.05

## MODELO 2 ##
reg2 <- lm(data = data, cigarros ~ educacion)
summary(reg2)
stargazer(reg2, type = "text")

# Visualizacion grafica del modelo
plot(
  x = data$educacion,
  y = cigarros,
  main = "Dispersión Educación-Precio del Cigarrillo",
  xlab = "Educación",
  ylab = "Q Cigarillos"
)
abline(reg2, col = "red", lwd = 2)

# TEST
# Test de Normalidad
residuos.reg2 <- reg2$residuals
shapiro.test(residuos.reg2)

# p-value=2.2e-16 < 5% de significancia, por tanto, se rechaza H0, los errores no se distribuyen normalmente.

# Test de Homocedasticidad
white.2 <- bptest(reg2)
white.2

# conclusion: p-value = 0.4284 > 5% de significancia, por tanto, se rechaza H1, el modelo presenta Homocedasticidad.

## MODELO 3 ##
reg3 <- lm(data = data, ingresos ~ educacion)
summary(reg3)
stargazer(reg3, type = "text")

# Visualizacion grafica del modelo
plot(
  x = data$educacion,
  y = ingresos,
  main = "Dispersión Educación-Ingresos",
  xlab = "Educación",
  ylab = "Ingresos"
)
abline(reg3, col = "red", lwd = 2)

# TEST
# Test de Normalidad
residuos.reg3 <- reg3$residuals
shapiro.test(residuos.reg3)

plot(residuos.reg3)

# p-value= 2.4e-12 < 5% de significancia, por tanto, se rechaza H0, los errores no se distribuyen normalmente.

# Test de Homocedasticidad
white.3 <- bptest(reg3)
white.3

# conclusion: p-value = 0.02965 < 5% de significancia, por tanto, se rechaza H0, el modelo presenta Heterocedasticidad.
