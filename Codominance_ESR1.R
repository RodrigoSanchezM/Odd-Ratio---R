#NOV 2025
#ANÁLISIS Codominancia


#carga de paquetes a utilizar
library(dplyr)
library(readxl)
library(epiR)
library(tidyverse)
library(DescTools)
library(oddsratio)

#Dirección de la carpeta de trabajo
setwd("/Users/rodrigosanchezmacedo/Documents/Proyecto CaMa")


#Creación de tabla a partir del archivo excel
tabla_completa <- read_excel("data/tablas/Tabla CaMa_R.xlsx")


#Verificar frecuencias del alelo intergenico
table(tabla_completa$`Genotipo ESR1`, tabla_completa$GRUPO)


#EXCLUSIÓN DEL GENOTIPO GG

subset <- subset(tabla_completa, `Genotipo ESR1` %in% c("AA", "AG"))

table(subset$`Genotipo ESR1`, subset_AG$GRUPO)

#Creación de la tabla 2x2

tabla_ESR1_AG <- matrix(
  c(27, 29,   # AG: pacientes, controles
    50, 40),  # AA: pacientes, controles
  nrow = 2,
  byrow = TRUE
)

#nombre de columnas
colnames(tabla_ESR1_AG) <- c("Paciente", "Control")

#nombramiento de filas
rownames(tabla_ESR1_AG) <- c("AG", "AA")

tabla_ESR1_AG


#calculo Odd Ratio

subset_AA_AG <- epi.2by2(tabla_ESR1_AG, method = "case.control",  conf.level = 0.95)
############################################################################################################

#EXCLUSIÓN DEL GENOTIPO AG

#Creación de la tabla 2x2

tabla_ESR1_GG <- matrix(
  c(15, 14,   # AG: pacientes, controles
    50, 40),  # AA: pacientes, controles
  nrow = 2,
  byrow = TRUE
)

#nombre de columnas
colnames(tabla_ESR1_GG) <- c("Paciente", "Control")

#nombramiento de filas
rownames(tabla_ESR1_GG) <- c("GG", "AA")


tabla_ESR1_GG



#calculo Odd Ratio
subset_GG <- epi.2by2(tabla_ESR1_GG, method = "case.control",  conf.level = 0.95)
