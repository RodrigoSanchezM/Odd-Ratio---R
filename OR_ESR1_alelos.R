#Script para cálculos de ORs del ALELO del SNP ESR1 en el proyecto CaMa 
#Julio 2025

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


#Verificar frecuencias del genotipo intergenico
genotipos_tabla <- table(tabla_completa$`Genotipo ESR1`, tabla_completa$GRUPO)


#Verificación del genotipo  homocigoto mutante (en este caso "GG"), y que en caso de no haber, se le asigne valor "Cero".
#Si no existe la fila "TT" en la tabla "genotipos_tabla
if (!"GG" %in% rownames(genotipos_tabla)) {
  
#Se agrega una fila con rbind, con los valores "0" tanto en el grupo "Control" como el grupo "Paciente"  
  genotipos_tabla <- rbind(genotipos_tabla, TT = c(Control = 0, Paciente = 0))
}

  

#Reordenamiento de las filas c(CC,CT,TT)
genotipos_tabla <- genotipos_tabla[c("AA", "AG", "GG"), ]


# Extraer los conteos para cada grupo
control <- genotipos_tabla[, "Control"]
paciente <- genotipos_tabla[, "Paciente"]


# Alelos para grupo Control
alelos_A_control <- 2 * control["AA"] + control["AG"]
alelos_G_control <- 2 * control["GG"] + control["AG"]



# Alelos para grupo Paciente
alelos_A_paciente <- 2 * paciente["AA"] + paciente["AG"]
alelos_G_paciente <- 2 * paciente["GG"] + paciente["AG"]



#Creación de tabla
#Para crear un dataframe en R a partir de uno o más vectores de la misma longitud, usamos la función data.frame(). Su sintaxis básica es la siguiente:  df <- data.frame(vector_1, vector_2)

tabla_alelos <- data.frame(
  Control= c(alelos_G_control,alelos_A_control),
  Paciente= c(alelos_G_paciente, alelos_A_paciente),
  row.names = c("G","A")
)

#Transformación del data frame a tabla
tabla_alelos_table <- as.table(as.matrix(tabla_alelos))

#Reordenamiento columnas

tabla_alelos_table <- tabla_alelos_table[,c("Paciente", "Control")]

################################################################################################

#Cálculo OR crudo


OR_alelos_ESR1 <- epi.2by2(tabla_alelos_table, method = "case.control",  conf.level = 0.95)

















