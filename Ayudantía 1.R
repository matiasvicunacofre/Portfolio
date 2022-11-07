#******** Ayudantía 1 - Test de Hipótesis ********#
#********     Profesor: Rodrigo Ortiz     ********#
#********     Ayudante: Matías Vicuña     ********#
#******** Mod Regresión Multivariada en R ********#

# Librerías a usar
library(tidyverse)
library(stargazer)
library(wooldridge)

# En caso de no tener las librerías, correr este código
install.packages(c("tidyverse","stargazer","wooldridge"))

# Cargamos la base de datos.
url <- 'https://raw.githubusercontent.com/fhernanb/datos/master/medidas_cuerpo'
datos <- read.table(file=url, header=T)

# Extraemos la data de los hombres
hombres <- datos[datos$sexo=="Hombre", ]

# Usamos la información del peso de los hombres
peso <- as.data.frame(hombres$peso)

# tabla de estadisticas
summary(peso)

# Desviación estándar
sd(hombres$peso)

# Promedio
mean(hombres$peso)

# grafica de densidad
plot.peso <- ggplot(hombres) +
  geom_density(aes(x = hombres$peso),
    color = "blue",
    size = 2) +
  labs(
    x = "Peso (Muestra)",
    y = "Densidad",
    title = "Distribución del Peso en hombres",
    subtitle = "Muestra de N = 18"
  ) +
  theme_classic()

plot.peso

# Datos para muejeres
mujeres <- datos[datos$sexo=="Mujer", ]

peso.mujer <- as.data.frame(mujeres$peso)

summary(mujeres$peso)

# Grafica de densidad
plot2.peso <- ggplot(mujeres) +
  geom_density(aes(x = mujeres$peso),
    color = "red",
    size = 2) +
  labs(
    x = "Peso (Muestra)",
    y = "Densidad",
    title = "Distribución del Peso en Mujeres",
    subtitle = "Muestra de N = 18"
  ) +
  theme_classic()

plot2.peso

#******* Nuestra hipótesis es que tenemos un promedio de 90 (mu = 90), al 95% de confianza. *******#

# hipótesis H0 >= 90, H1 < 90.
t.test(peso, mu = 90, alternative = "less", conf.level = 0.95)

# hipótesis H0 =< 90, H1 > 90.
t.test(peso, mu = 90, alternative = "greater")

# hipótesis H0 = 90, H1 < 90 o H1 > 90.
t.test(peso, mu = 90, alternative = "two.sided")

#******* Nuestra hipótesis es que tenemos un promedio de 90 (mu = 90), al 90% de confianza. *******#

# hipótesis H0 >= 90, H1 < 90.
t.test(peso, mu = 90, alternative = "less", conf.level = 0.90)

# hipótesis H0 =< 90, H1 > 90.
t.test(peso, mu = 90, alternative = "greater", conf.level = 0.90)

# hipótesis H0 = 90, H1 < 90 o H1 > 90.
t.test(peso, mu = 90, alternative = "two.sided", conf.level = 0.90)

#******* Nuestra hipótesis es que tenemos un promedio de 82 (mu = 82), al 95% de confianza. *******#

# hipótesis H0 >= 82, H1 < 82.
t.test(peso, mu = 82, alternative = "less", conf.level = 0.95)

# hipótesis H0 =< 82, H1 > 82.
t.test(peso, mu = 82, alternative = "greater", conf.level = 0.95)

# hipótesis H0 = 82, H1 < 82 o H1 > 82.
t.test(peso, mu = 82, alternative = "two.sided", conf.level = 0.95)

#******* Ahora usamos una muestra mayor, N = 518 *******#

View(big9salary)

db <- big9salary
db <- na.omit(db)

salarios <- db$salary

ggplot(db)+
  geom_density(aes(x = db$salary),
    color = "purple",
    size = 2)+
  labs(x = "Salarios",
    y = "Frecuencia",
    title = "Distribución Salarios",
    subtitle = "N = 518")+
  theme_classic()

summary(db$salary)
sd(db$salary)

# Caso 1
#******* Nuestra hipótesis es que el salario promedio es de 95000 (mu = 95000), al 95% de confianza. *******#

# hipótesis H0 >= 95000, H1 < 95000.
t.test(salarios, mu = 95000, alternative = "less", conf.level = 0.95)

# hipótesis H0 =< 95000, H1 > 95000.
t.test(salarios, mu = 95000, alternative = "greater", conf.level = 0.95)

# hipótesis H0 = 95000, H1 < 95000 o H1 > 95000.
t.test(salarios, mu = 95000, alternative = "two.sided", conf.level = 0.95)

#Caso 2
#******* Nuestra hipótesis es que el salario promedio es de 85000 (mu = 85000), al 95% de confianza. *******#

# hipótesis H0 >= 85000, H1 < 85000.
t.test(salarios, mu = 85000, alternative = "less", conf.level = 0.95)

# hipótesis H0 =< 85000, H1 > 85000.
t.test(salarios, mu = 85000, alternative = "greater", conf.level = 0.95)

# hipótesis H0 = 85000, H1 < 85000 o H1 > 85000.
t.test(salarios, mu = 85000, alternative = "two.sided", conf.level = 0.95)
