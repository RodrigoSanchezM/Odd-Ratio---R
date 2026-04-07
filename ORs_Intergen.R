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


#Verificar frecuencias del genotipo intergenico
table(tabla_completa$`Genotipo Intergen`, tabla_completa$GRUPO)


#Creación de una columna TRUE oe FALSE, a según la muestra presente la variante (CT(mt) = TRUE, CC (wt) = FALSE)
tabla_completa$`Presencia alelo Intergen` <- tabla_completa$`Genotipo Intergen` == "CT" | tabla_completa$`Genotipo Intergen` == "TT"


#Creación de tabla 2x2 para el alelo intergénico
tabla_intergen <- table(tabla_completa$`Presencia alelo Intergen`, tabla_completa$GRUPO)


#Renombramiento de las filas ("CC", "CT + TT) en reemplazo de los nombres "TRUE" y "FALSE"
rownames(tabla_intergen) <- c("CC", "CT + TT")

#reordenamiento de FILAS para que el programa epi.2by2, coja correctamente por default cuál es el grupo con la presencia del alelo mutante (grupo expuesto) y cuál es el grupo no expuesto (contiene alelo wt)


tabla_intergen <- tabla_intergen[nrow(tabla_intergen):1,]



#reordenamiento de COLUMNAS (primero tiene que estar los pacientes)

tabla_intergen <- tabla_intergen[, c("Paciente", "Control")]



#Cálculo OR crudo
OR_intergen_a <- epi.2by2(tabla_intergen, method = "case.control",  conf.level = 0.95)



#ANÁLISIS ODDS RATIO AJUSTADO


#Selección de columnas y formación de una nueva tabla
tabla_OR_ajustado_intergen <- tabla_completa %>% select("GRUPO","Edad","Presencia alelo Intergen")


#Reemplazo valores columna "GRUPO"
#Es necesario que que el grupo "Paciente" sea igual a 1. Ya que en el modelo de regresión logística el grupo codificado como 1 es el que presenta el evento de interés (la enfermedad). El grupo "0" representa la AUSENCIA del evento
tabla_OR_ajustado_intergen <- tabla_OR_ajustado_intergen %>% 
  mutate(GRUPO = if_else(GRUPO == "Paciente", 1,0))



#Conversión de la variable GRUPO en una VARIABLE CATEGÓRICA (y no numérica). Los modelos de regresión logística (glm(..., family = binomial)) esperan 
#que la variable dependiente sea un factor con dos niveles (por ejemplo, "Control" vs "Cancer").

tabla_OR_ajustado_intergen$GRUPO <- as.factor(tabla_OR_ajustado_intergen$GRUPO)

#Establecer la variable EDAD como variable NUMÉRICA y no categórica.
tabla_OR_ajustado_intergen$Edad <- as.numeric(tabla_OR_ajustado_intergen$Edad)


#Cálculo de la regresión logística 
OR_ajustado_intergen <- glm (GRUPO ~ `Presencia alelo Intergen`+ Edad, data = tabla_OR_ajustado_intergen, family = binomial)

#Selección de FILAS COEF

summary_ORtab_Intergen <- summary (OR_ajustado_intergen)$coef


data.frame(variable = rownames(summary_ORtab_Intergen),
           oddsratio = round(exp(summary_ORtab_Intergen[,1]),3),
           ci_low = round(exp(summary_ORtab_Intergen[,1] - 1.96*summary_ORtab_Intergen[,2]),3),
           ci_high = round(exp(summary_ORtab_Intergen[,1] + 1.96*summary_ORtab_Intergen[,2]),3),
           pval = scales::pvalue(summary_ORtab_Intergen[,4]),
           row.names = NULL)




