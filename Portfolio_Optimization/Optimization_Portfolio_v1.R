#############################################################################
# Portfolio Opimization
# Financial Inferences 
# Author: Matías Vicuña Cofré
#############################################################################
#******************** Configuraciones e Inicializacion *********************#
#############################################################################
{
  # Librerías
  {
    # Lista de paquetes que quieres verificar
    paquetes <- c("tidyverse","readxl","janitor","skimr","scales","ggthemes","tseries","fPortfolio","knitr","kableExtra","gplots")
    
    # Verificar e instalar paquetes que no estén instalados
    for (paquete in paquetes) {
      if (!require(paquete, character.only = TRUE)) {
        install.packages(paquete)
      }
    }
    remove(paquetes)
    
    # Activacion Librerias
    suppressPackageStartupMessages({
      library(tidyverse)
      library(readxl)
      library(janitor)
      library(skimr)
      library(scales)
      library(ggthemes)
      library(tseries)
      library(fPortfolio)
      library(knitr)
      library(kableExtra)
    })
  }
  # Configuraciones Entorno de Trabajo
  {
    # Limpiamos el entorno de trabajo
    rm(list = ls())
    
    # Limpiamos la Memoria (RAM)
    gc(reset = TRUE)
    
    # Limpiamos la consola
    cat("\014")
    
    # De número científico a natural
    options(scipen = 999)
  }
}
# Ruta de acceso a la base (Notebook)
setwd("C:/Users/matias/Dropbox/1- Matias/Programacion/R/Portafolio_R/archives_datasets")
# Ruta de acceso a la base (Torre)
setwd("C:/Users/matei/Dropbox/1- Matias/Programacion/R/Portafolio_R/archives_datasets")

#############################################################################
#*********************************** FIN ***********************************#
#############################################################################

# Modelo de Markowitz

# Series de tiempo
Indice <- get.hist.quote(
  instrument = "^GSPC",
  start = as.Date("2010-01-04"),
  end = as.Date("2020-04-30"),
  quote = "AdjClose"
)
plot(Indice,
     col = "deepskyblue",
     xlab = "Fecha",
     ylab = "AdjClose")
title(main = "Histórico del Índice S&P 500 [2010-2020]")

summary(Indice)

MSFT <- get.hist.quote(
  instrument = "MSFT",
  start = as.Date("2010-01-04"),
  end = as.Date("2020-04-30"),
  quote = "AdjClose"
)

plot(MSFT,
     col = "deepskyblue",
     xlab = "Fecha",
     ylab = "AdjClose")
title(main = "Histórico de Microsoft Corporation [2010-2020]")

JNJ <- get.hist.quote(
  instrument = "JNJ",
  start = as.Date("2010-01-04"),
  end = as.Date("2020-04-30"),
  quote = "AdjClose"
)

plot(JNJ,
     col = "deepskyblue",
     xlab = "Fecha",
     ylab = "AdjClose")
title(main = "Histórico de Johnson & Johnson [2010-2020]")

MA <- get.hist.quote(
  instrument = "MA",
  start = as.Date("2010-01-04"),
  end = as.Date("2020-04-30"),
  quote = "AdjClose"
)

plot(MA,
     col = "deepskyblue",
     xlab = "Fecha",
     ylab = "AdjClose")
title(main = "Histórico de Mastercard Incorporated [2010-2020]")

PEP <- get.hist.quote(
  instrument = "PEP",
  start = as.Date("2010-01-04"),
  end = as.Date("2020-04-30"),
  quote = "AdjClose"
)

plot(PEP,
     col = "deepskyblue",
     xlab = "Fecha",
     ylab = "AdjClose")
title(main = "Histórico de PepsiCo Inc. [2010-2020]")

WMT <- get.hist.quote(
  instrument = "WMT",
  start = as.Date("2010-01-04"),
  end = as.Date("2020-04-30"),
  quote = "AdjClose"
)

plot(WMT,
     col = "deepskyblue",
     xlab = "Fecha",
     ylab = "AdjClose")
title(main = "Histórico de Walmart Corporate [2010-2020]")

MCD <- get.hist.quote(
  instrument = "MCD",
  start = as.Date("2010-01-04"),
  end = as.Date("2020-04-30"),
  quote = "AdjClose"
)

plot(MCD,
     col = "deepskyblue",
     xlab = "Fecha",
     ylab = "AdjClose")
title(main = "Histórico de McDonald's Corporation [2010-2020]")

CarteraInv <- merge(MSFT, JNJ, MA, PEP, WMT, MCD, all = FALSE)
names(CarteraInv)

names(CarteraInv) <- c("MSFT", "JNJ", "MA", "PEP", "WMT", "MCD")
plot(
  CarteraInv,
  main = " ",
  col = "deepskyblue",
  xlab = "Fecha"
)
title(main = "Histórico de Cartera")

RetornoIndice <- diff(log(Indice))
head(RetornoIndice, 10)

plot(
  RetornoIndice,
  main = " ",
  col = "deepskyblue",
  xlab = "Fecha",
  ylab = "Rendimientos"
)
title(main = "Rendimientos del Indice S&P 500")

Rendimientos <- diff(log(CarteraInv))
head(Rendimientos, 10)

plot(
  Rendimientos,
  main = " ",
  col = "deepskyblue",
  xlab = "Fecha"
)
title(main = "Rendimientos de la Cartera")

summary(Rendimientos)

RendimientoPromedio = c(
  mean(RetornoIndice),
  mean(Rendimientos$MSFT),
  mean(Rendimientos$JNJ),
  mean(Rendimientos$MA),
  mean(Rendimientos$PEP),
  mean(Rendimientos$WMT),
  mean(Rendimientos$MCD)
)

Volatilidad = c(
  sd(RetornoIndice),
  sd(Rendimientos$MSFT),
  sd(Rendimientos$JNJ),
  sd(Rendimientos$MA),
  sd(Rendimientos$PEP),
  sd(Rendimientos$WMT),
  sd(Rendimientos$MCD)
)

Cuadro = data.frame (rbind(RendimientoPromedio, Volatilidad))
colnames(Cuadro) <-
  c("GSPC", "MSFT", "JNJ", "MA", "PEP", "WMT", "MCD")

Cuadro * 100

var(RetornoIndice) * 100

var(Rendimientos$MSFT) * 100

var(Rendimientos$JNJ) * 100

var(Rendimientos$MA) * 100

var(Rendimientos$PEP) * 100

var(Rendimientos$WMT) * 100

var(Rendimientos$MCD) * 100

Cov <- cov(Rendimientos) * 100
Cov

corr <- cor(Rendimientos) * 100
corr

library(gplots)

generate_heat_map <- function(correlationMatrix, title)
{
  heatmap.2(
    x = correlationMatrix,
    cellnote = correlationMatrix,
    main = title,
    symm = TRUE,
    dendrogram = "none",
    Rowv = FALSE,
    trace = "none",
    density.info = "none",
    notecol = "black"
  )
}

corr1 <- round(cor(Rendimientos) * 100, 2)
generate_heat_map(corr1, "Mapa de calor: Correlaciones")

markov <- portfolioSpec()

setRiskFreeRate(markov) <- -0.001 # Tasa libre de riesgo
setNFrontierPoints(markov) <- 20 # Cantidad de carteras en frontera
constraints = "LongOnly"
Frontera <-
  portfolioFrontier(as.timeSeries(Rendimientos), spec = markov, constraints)
Frontera

frontierPlot(Frontera)
grid()
tangencyPoints(Frontera,
               pch = 19,
               col = "red",
               cex = 2)
tangencyLines(Frontera,
              col = "grey",
              pch = 19,
              cex = 2)
minvariancePoints(Frontera,
                  col = "blue",
                  pch = 19,
                  cex = 2)
monteCarloPoints(Frontera,
                 mCsteps = 2000,
                 col = "#0098D5",
                 cex = 0.001)

col <- qualiPalette(ncol(Rendimientos), "Pastel1")
weightsPlot(Frontera, col = col)

efPortfolio <-
  efficientPortfolio(as.timeSeries(Rendimientos), markov, constraints)
efPortfolio

tgPortfolio <-
  tangencyPortfolio(as.timeSeries(Rendimientos), markov, constraints)
tgPortfolio

weightsPie(efPortfolio, col = col)
mtext(
  text = "Portafolio eficiente",
  side = 3,
  line = 1.5,
  font = 2,
  cex = 0.7,
  adj = 0
)

weightsPie(tgPortfolio, col = col)
mtext(
  text = "Portafolio tangente",
  side = 3,
  line = 1.5,
  font = 2,
  cex = 0.7,
  adj = 0
)

#############################################################################

