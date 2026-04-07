#Script para cálculos de ORs del alelo INTERGÉNICO en el proyecto CaMa 
#Dic 2025

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



#Creación de una columna TRUE oe FALSE, a según la muestra presente la variante (TT (homocigoto mt) = TRUE, CC o CT = FALSE)
tabla_completa$`Presencia alelo Intergen` <-  tabla_completa$`Genotipo Intergen` == "TT"



#Creación de tabla 2x2 para el alelo intergénico
tabla_intergen <- table(tabla_completa$`Presencia alelo Intergen`, tabla_completa$GRUPO)



#Selección de columnas GRUPO, Edad, Genotipo Intergen

tabla_codominante_OR_Ajustado <- tabla_completa %>% select("GRUPO", "Edad", "Presencia alelo Intergen", )


#Reemplazo valores columna "GRUPO"
#Es necesario que que el grupo "Paciente" sea igual a 1. Ya que en el modelo de regresión logística el grupo codificado como 1 es el que presenta el evento de interés (la enfermedad). El grupo "0" representa la AUSENCIA del evento
tabla_codominante_OR_Ajustado <- tabla_codominante_OR_Ajustado %>% 
  mutate(GRUPO = if_else(GRUPO == "Paciente", 1,0))



#Conversión de la variable GRUPO en una VARIABLE CATEGÓRICA (y no numérica). Los modelos de regresión logística (glm(..., family = binomial)) esperan 
#que la variable dependiente sea un factor con dos niveles (por ejemplo, "Control" vs "Cancer").

tabla_codominante_OR_Ajustado$GRUPO <- as.factor(tabla_codominante_OR_Ajustado$GRUPO)



#Establecer la variable EDAD como variable NUMÉRICA y no categórica.
tabla_codominante_OR_Ajustado$Edad <- as.numeric(tabla_codominante_OR_Ajustado$Edad)



#Cálculo de la regresión logística 
OR_ajustado_intergen_cod <- glm (GRUPO ~ `Presencia alelo Intergen`+ Edad, data = tabla_codominante_OR_Ajustado, family = binomial)


#Selección de FILAS COEF
summary_ORtab_Intergen <- summary (OR_ajustado_intergen_cod )$coef


data.frame(variable = rownames(summary_ORtab_Intergen),
           oddsratio = round(exp(summary_ORtab_Intergen[,1]),3),
           ci_low = round(exp(summary_ORtab_Intergen[,1] - 1.96*summary_ORtab_Intergen[,2]),3),
           ci_high = round(exp(summary_ORtab_Intergen[,1] + 1.96*summary_ORtab_Intergen[,2]),3),
           pval = scales::pvalue(summary_ORtab_Intergen[,4]),
           row.names = NULL)















