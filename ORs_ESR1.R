#Script para cálculos de ORs del alelo INTERGÉNICO en el proyecto CaMa 
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


#Verificar frecuencias del alelo intergenico
table(tabla_completa$`Genotipo ESR1`, tabla_completa$GRUPO)


#Creación de una columna TRUE or FALSE, a según la muestra presente la variante (AG(mt) = TRUE, GG = TRUE, y CC (wt) = FALSE)
tabla_completa$`ESR1 AG+GG` <- tabla_completa$`Genotipo ESR1` == "AG" | tabla_completa$`Genotipo ESR1`== "GG"

tabla_completa$`ESR1 GG` <- tabla_completa$`Genotipo ESR1`== "GG"


#Creación de tabla 2x2 para el alelo ESR1
tabla_ESR1_a <- table(tabla_completa$`ESR1 AG+GG`, tabla_completa$GRUPO)

tabla_ESR1_b <- table(tabla_completa$`ESR1 GG`, tabla_completa$GRUPO)


#Nombramiento de filas
rownames(tabla_ESR1_a) <- c("AA", "AG + GG")
rownames(tabla_ESR1_b) <- c("AA + AG", "GG")



#reordenamiento de filas para que el programa epi.2by2, coja correctamente por default cuál es el grupo 
#con la presencia del alelo mutante (grupo expuesto) y cuál es el grupo no expuesto (contiene alelo wt)
tabla_ESR1_a <- tabla_ESR1_a[nrow(tabla_ESR1_a):1,]
tabla_ESR1_b <- tabla_ESR1_b[nrow(tabla_ESR1_b):1,]


#reordenamiento de COLUMNAS

tabla_ESR1_a <- tabla_ESR1_a[,c("Paciente","Control")]
tabla_ESR1_b <- tabla_ESR1_b[,c("Paciente","Control")]

#Cálculo OR crudo
OR_ESR1_a <- epi.2by2(tabla_ESR1_a, method = "case.control",  conf.level = 0.95)

OR_ESR1_b <- epi.2by2(tabla_ESR1_b, method = "case.control",  conf.level = 0.95)



#Selección de columnas y formación de una nueva tabla
tabla_OR_ajustado_ESR1_a <- tabla_completa %>% select("GRUPO","Edad","ESR1 AG+GG")

tabla_OR_ajustado_ESR1_b <- tabla_completa %>% select("GRUPO", "Edad", "ESR1 GG")



#Reemplazo valores columna "GRUPO"
tabla_OR_ajustado_ESR1_a <- tabla_OR_ajustado_ESR1_a %>% 
  mutate(GRUPO = if_else(GRUPO == "Paciente", 1,0))


tabla_OR_ajustado_ESR1_b <- tabla_OR_ajustado_ESR1_b %>% 
  mutate(GRUPO = if_else(GRUPO == "Paciente", 1,0))



#Conversión de la variable GRUPO en una VARIABLE CATEGÓRICA (y no numérica). Los modelos de regresión logística (glm(..., family = binomial)) esperan 
#que la variable dependiente sea un factor con dos niveles (por ejemplo, "Control" vs "Cancer").

tabla_OR_ajustado_ESR1_a$GRUPO <- as.factor(tabla_OR_ajustado_ESR1_a$GRUPO)

tabla_OR_ajustado_ESR1_b$GRUPO <- as.factor(tabla_OR_ajustado_ESR1_b$GRUPO)



#Establecer la variable EDAD como variable NUMÉRICA y no categórica.
tabla_OR_ajustado_ESR1_a$Edad <- as.numeric(tabla_OR_ajustado_ESR1_a$Edad)


#Establecer la variable EDAD como variable NUMÉRICA y no categórica.
tabla_OR_ajustado_ESR1_b$Edad <- as.numeric(tabla_OR_ajustado_ESR1_b$Edad)





#Cálculo de la regresión logística 
OR_ajustado_ESR1_a <- glm (GRUPO ~ `ESR1 AG+GG`+ Edad, data = tabla_OR_ajustado_ESR1_a, family = binomial)


OR_ajustado_ESR1_b <- glm (GRUPO ~ `ESR1 GG`+ Edad, data = tabla_OR_ajustado_ESR1_b, family = binomial)


#Selección de FILAS Coef

summary_ORtab_ESR1_a <- summary (OR_ajustado_ESR1_a)$coef

summary_ORtab_ESR1_b <- summary (OR_ajustado_ESR1_b)$coef



#Oddsratio ajustado con la variable edad teniendo como grupo expuesto personas con el genotipo "AG o GG"

data.frame(variable = rownames(summary_ORtab_ESR1_a),
           oddsratio = round(exp(summary_ORtab_ESR1_a[,1]),3),
           ci_low = round(exp(summary_ORtab_ESR1_a[,1] - 1.96*summary_ORtab_ESR1_a[,2]),3),
           ci_high = round(exp(summary_ORtab_ESR1_a[,1] + 1.96*summary_ORtab_ESR1_a[,2]),3),
           pval = scales::pvalue(summary_ORtab_ESR1_a[,4]),
           row.names = NULL)


#Oddsratio ajustado con la variable edad teniendo como grupo expuesto personas con el genotipo "GG"

data.frame(variable = rownames(summary_ORtab_ESR1_b),
           oddsratio = round(exp(summary_ORtab_ESR1_b[,1]),3),
           ci_low = round(exp(summary_ORtab_ESR1_b[,1] - 1.96*summary_ORtab_ESR1_b[,2]),3),
           ci_high = round(exp(summary_ORtab_ESR1_b[,1] + 1.96*summary_ORtab_ESR1_b[,2]),3),
           pval = scales::pvalue(summary_ORtab_ESR1_b[,4]),
           row.names = NULL)













