#Script para el cálculo del Odds Ratio entre presencia del alelo mt intergen y grupos sensibles y no sensibles a ER
#Julio 2025 

#Carga de paquetes a utilizar
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

#Homeganización de nombres (que todos los nombres en las columnas ER,PR, HER2) empiezen con mayúsculas 
#Se usa la función str_to_title
#Se tiene el problema que algunos nombres emppiezan con minúsculas y otros con mayúcula 

tabla_completa <- tabla_completa %>%
  mutate(across(c(ER,PR,HER2), str_to_title))

#Creación de nueva tabla seleccíonando del GRUPO  == " Paciente"
tabla_pacientes <- tabla_completa %>%
  filter(GRUPO == "Paciente")

##########################################################################################################################################################

#Verificación de frecuencias del alelo intergen
table(tabla_pacientes$`Genotipo ESR1`,tabla_pacientes$ER)


#Creación de una columna TRUE or FALSE, a según la muestra presente la variante (AG o GG(mt) = TRUE, CC (wt) = FALSE)

tabla_pacientes$`Presencia Alelo ESR1` <- tabla_pacientes$`Genotipo ESR1` == "AG" | tabla_pacientes$`Genotipo ESR1`== "GG"

#Creación de tabla 2x2 para el alelo intergénico
table2x2_ESR1 <- table(tabla_pacientes$`Presencia Alelo ESR1`, tabla_pacientes$ER)


#Renombramiento de las filas ("AA", "AG + GG) en reemplazo de los nombres "TRUE" y "FALSE"
rownames(table2x2_ESR1) <- c("AA", "AG + GG")


#Reordenamiento de filas para que el programa epi.2by2, coja correctamente por default cuál es el grupo 
#con la presencia del alelo mutante (grupo expuesto) y cuál es el grupo no expuesto (contiene alelo wt)
table2x2_ESR1 <- table2x2_ESR1[nrow(table2x2_ESR1):1,]

#Reordenamiento de columnas

table2x2_ESR1 <- table2x2_ESR1[,c("Positivo","Negativo")]


#Cálculo OR crudo
OR_ESR1_ER <- epi.2by2(table2x2_ESR1, method = "case.control",  conf.level = 0.95)


#Selección de columnas y formación de una nueva tabla
OR_ajustado_ESR1_ER <- tabla_pacientes %>% select("ER","Edad","Presencia Alelo ESR1")


#Reemplazo valores columna "GRUPO"
OR_ajustado_ESR1_ER <- OR_ajustado_ESR1_ER %>% 
  mutate(ER = if_else(ER == "Positivo", 1,0))


#Conversión de la variable GRUPO en una VARIABLE CATEGÓRICA (y no numérica). Los modelos de regresión logística (glm(..., family = binomial)) esperan 
#que la variable dependiente sea un factor con dos niveles (por ejemplo, "Control" vs "Cancer").

OR_ajustado_ESR1_ER$ER <- as.factor(OR_ajustado_ESR1_ER$ER)

#Establecer la variable EDAD como variable NUMÉRICA y no categórica.
OR_ajustado_ESR1_ER$Edad <- as.numeric(OR_ajustado_ESR1_ER$Edad)



#Cálculo de la regresión logística 
OR_ajustado_ESR1_ER <- glm (ER ~ `Presencia Alelo ESR1`+ Edad, data = OR_ajustado_ESR1_ER, family = binomial)



#Selección de FILAS COEF
summary_ORtab_ESR1_ER <- summary (OR_ajustado_ESR1_ER)$coef



OR_ESR1_ER <- data.frame(variable = rownames(summary_ORtab_ESR1_ER),
                    oddsratio = round(exp(summary_ORtab_ESR1_ER[,1]),3),
                    ci_low = round(exp(summary_ORtab_ESR1_ER[,1] - 1.96*summary_ORtab_ESR1_ER[,2]),3),
                    ci_high = round(exp(summary_ORtab_ESR1_ER[,1] + 1.96*summary_ORtab_ESR1_ER[,2]),3),
                    pval = scales::pvalue(summary_ORtab_ESR1_ER[,4]),
                    row.names = NULL)











































