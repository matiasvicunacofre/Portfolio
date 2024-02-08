#############################################################################
# Exploratory Data Analysis (EDA)
# Financial Inferences - S&P 500
# Author: Matías Vicuña Cofré
#############################################################################
#******************** Configuraciones e Inicializacion *********************#
#############################################################################
{
# Librerías
{
  # Lista de paquetes que quieres verificar
  paquetes <- c("tidyverse","readxl","janitor","skimr","scales","ggthemes")
  
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
#*********************************** EDA ***********************************#
#############################################################################

# Loading the dataset
db_financial <- read_csv(file = "financials_SP_500.csv")

# Describimos los sectores expuestos en el S&P 500  
unique(db_financial$Sector)

# Número de Empresas por Sector/Industria
db_financial %>%
  group_by(Sector) %>%
  tally() %>%
  ggplot() +
  geom_col(aes(x = fct_reorder(Sector,n), 
               y = n, 
               fill = Sector)) +
  geom_label(aes(x = Sector, y = n + 4, label = n),
             size = 5) +
  labs(title = "Número de Empresas por Sector",
       x = "",
       y = "Número") +
  theme_bw() +
  scale_fill_calc() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90,
                                   face = "bold",
                                   size = 12),
        axis.title.y = element_text(vjust = 3,
                                    size = 12,
                                    face = "bold"),
        plot.title = element_text(size = 20, 
                                  vjust = 1, 
                                  hjust = 0.5, 
                                  face = "bold"))
  

# Boxplot de Precios por Sector/Industria
db_financial %>%
  ggplot(aes(x = fct_reorder(Sector,Price),  y = Price, fill = Sector)) +
  geom_boxplot() +
  coord_flip() +
  theme_bw() +
  scale_fill_calc() +
  labs(title = "Distribución precios por Sector",
       x = NULL, 
       y = "Precio USD$") +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 15,
                                    vjust = -0.5, 
                                    face = "bold"),
        axis.text.y = element_text(vjust = -0.5, 
                                    face = "bold"),
        plot.title = element_text(size = 20, 
                                  vjust = 1, 
                                  hjust = 0.5, 
                                  face = "bold"))

# Precio acción empresas - Agrupado por Sectores
db_financial %>%
  ggplot(aes(x = Price, 
             fill = Sector)) +
  geom_histogram() +
  facet_wrap(vars(Sector), scales = "free") +
  theme_bw() +
  scale_fill_calc() +
  theme(axis.title.x = element_text(size = 15,
                                    face = "bold",
                                    vjust = -3),
        axis.title.y = element_text(size = 15,
                                    face = "bold",
                                    vjust = 3.5),
        plot.title = element_text(size = 20, 
                                  vjust = 2, 
                                  hjust = 0.5, 
                                  face = "bold"),
        plot.subtitle = element_text(size = 15, 
                                     vjust = 2,
                                     hjust = 0.5),
        legend.position = "none") +
  labs(x = "Cantidad",
       y = "Precio USD$",
       title = "Precio acción empresas, recuento",
       subtitle = "Por Industria/Sector")

# EBITDA Promedio por Sector
db_financial %>% 
  group_by(Sector) %>% 
  summarise(MEAN_EBITDA = mean(EBITDA)) %>%
  select(Sector, MEAN_EBITDA) %>% 
  ggplot(., aes(
                y = MEAN_EBITDA,
                x = fct_reorder(Sector, MEAN_EBITDA))) +
  geom_bar(aes(fill = Sector),
           position = "dodge",
           stat = "identity") +
  geom_label(aes(x = Sector,
                 y = MEAN_EBITDA + 1000000000, 
                 label = paste("$",
                               format(x = round(MEAN_EBITDA,0), 
                                          big.mark = "."))),
             size = 3.5) +
  labs(title = "EBITDA promedio S&P 500 por industria",
       x = "",
       fill = "Industria",
       caption = "* `B` en el eje Y correspode a Billones de Dólares (USD)") +
  scale_y_continuous("EBITDA Promedio*",
                     labels = label_number(scale_cut = cut_short_scale())) +
  scale_fill_calc() +
  theme_bw() + 
  theme(legend.position = "none",
        axis.text.x = element_text(size = 8.5, 
                                   vjust = -1, 
                                   face = "bold"),
        axis.title.y = element_text(size = 10, 
                                    vjust = 2,
                                    face = "bold"),
        plot.title = element_text(size = 20, 
                                  vjust = 1, 
                                  hjust = 0.5, 
                                  face = "bold"))

# Precio Promedio por Sector
db_financial %>% 
  group_by(Sector) %>% 
  summarise(MEAN_PRICE = mean(Price)) %>%
  select(Sector, MEAN_PRICE) %>% 
  ggplot(., aes(
    y = MEAN_PRICE,
    x = fct_reorder(Sector, MEAN_PRICE))) +
  geom_bar(aes(fill = Sector),
           position = "dodge",
           stat = "identity") +
  geom_label(aes(x = Sector,
                 y = MEAN_PRICE + 5, 
                 label = paste("$",
                               format(x = round(MEAN_PRICE,2), 
                                      big.mark = ","))),
             size = 4.5) +
  labs(title = "Precio promedio S&P 500 por industria",
       x = "",
       fill = "Industria") +
  scale_y_continuous("Precio Promedio",
                     labels = label_number(scale_cut = cut_short_scale())) +
  scale_fill_calc() +
  theme_bw() + 
  theme(legend.position = "none",
        axis.text.x = element_text(size = 9, 
                                   vjust = -1, 
                                   face = "bold"),
        axis.title.y = element_text(size = 10, 
                                    vjust = 1.5,
                                    face = "bold"),
        plot.title = element_text(size = 20, 
                                  vjust = 1, 
                                  hjust = 0.5, 
                                  face = "bold"))
