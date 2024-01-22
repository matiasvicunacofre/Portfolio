#******** Regresión Simple ********#
#******** Autor: Matías Vicuña ********#

# En caso de no poseer las librerías, correr este código
install.packages(c("tidyverse", "stargazer", "wooldridge", "lmtest", "car"))

# Librerías a usar
library(tidyverse)
library(stargazer)
library(wooldridge)
library(lmtest)
library(car)

# Cargamos la data desde el paquete Wooldrige
data <- wooldridge::wage1

# Realizamos estadistica descriptiva de toda la muestra
summary(data)

# Visualización de las dimensiones de la data
dim(data)

# Generamos la primera regresión simple
reg1 <- lm(wage ~ educ, data = data)
summary(reg1) # 1ra opción: visualización simple
stargazer(reg1, type = "text", title = "Regresión Años de Educación") # 2da opción: Visualización stargazer

# Grafica de disperción
plot(data$educ, data$wage, 
  xlab = "Años de Educación",
  ylab = "Salarios",
  main = "Años de Educación Vs Salarios",
  col = "green")
abline(reg1, col = "red", lty = 100)


# Residuales
residuos <- reg1$residuals # Generamos el objeto de los residuos
mean(reg1$residuals) # Promedio residuos = 0

# Test de Normalidad en los errores
shapiro.test(residuos)
# H0 = Los residuos se distribuyen Normal.
# H1 = Los residuos no se distribuyen Normal.

# Resultado: p-value < 2.2e-16 < 5%, por lo tanto, se rechaza la hipótesis nula, los residuos no se distribuyen normal.

# Visualización Gráfica de los errores.
plot1 <- ggplot(reg1)+
  geom_density(aes(x = residuos),
    col = "blue")+
  labs(x = "Residuos",
    y = "Densidad",
    title = "Distribución de los Residuos")+
  theme_classic()

#******* Tests del modelo *******#

# Test de Varianza
anova(reg1)

# Test de Homocedasticidad (Test de White)
test.white <- bptest(reg1)
test.white

# H0 = Varianza de los Errores (Residuos) Constantes = Homocedastico.
# H1 = Varianza de los Errores (Residuos) No Constantes = Heterocedastico.

# Resultado: con un p-value de 9.144-05 < 5% de significancia, concluimos que en el modelo hay heterocedasticidad.

#******* Ahora, corrigiendo el modelo, usamos el ln(wage): *******#
# Modelo simple nuevo
reg2 <- lm(lwage ~ educ, data = data)
summary(reg2)
stargazer(reg2, type = "text", title = "Regresión Corregida por Heterocedasticidad")

# Análisis de Varianza
anova(reg2)

# Residuos
residuos.2 <- reg2$residuals

# test de Normalidad
shapiro.test(residuos.2)
mean(residuos.2)

# Grafica de distribución de los errores
plot2 <- ggplot(reg2)+
  geom_density(aes(x = residuos.2),
    col = "blue")+
  labs(x = "Residuos",
    y = "Densidad",
    title = "Distribución de los Residuos")+
  theme_classic()
plot2

# Test de Heterocedasticidad. 
test.white.2 <- bptest(reg2)
test.white.2
