#Script para el cálculo del Odds Ratio entre presencia del alelo mt intergen y grupos sensibles y
#no sensibles a ER
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


#Verificación de frecuencias del alelo intergen

table(tabla_pacientes$`Genotipo Intergen`,tabla_pacientes$HER2)

#Creación de una columna TRUE oe FALSE, a según la muestra presente la variante (CT(mt) = TRUE, CC (wt) = FALSE)
tabla_pacientes$`Presencia Alelo Intergen` <- tabla_pacientes$`Genotipo Intergen` == "CT" | tabla_pacientes$`Genotipo Intergen` == "TT"


#Creación de tabla 2x2 para el alelo intergénico
table2x2_intergen_HER2 <- table(tabla_pacientes$`Genotipo Intergen`, tabla_pacientes$HER2)


#Renombramiento de las filas ("CC", "CT + TT) en reemplazo de los nombres "TRUE" y "FALSE"
rownames(table2x2_intergen_HER2) <- c("CC", "CT + TT")


#reordenamiento de filas para que el programa epi.2by2, coja correctamente por default cuál es el grupo 
#con la presencia del alelo mutante (grupo expuesto) y cuál es el grupo no expuesto (contiene alelo wt)
table2x2_intergen_HER2 <- table2x2_intergen_HER2[nrow(table2x2_intergen_HER2):1,]

#reordenamiento del orden de las columnas

table2x2_intergen_HER2 <- table2x2_intergen_HER2[, c("Positivo", "Negativo")]


#Cálculo OR crudo
OR_intergen_HER2 <- epi.2by2(table2x2_intergen_HER2, method = "case.control",  conf.level = 0.95)




#Selección de columnas y formación de una nueva tabla
OR_ajustado_intergen_HER2 <- tabla_pacientes %>% select("HER2","Edad","Presencia Alelo Intergen")


#Reemplazo valores columna "GRUPO"
OR_ajustado_intergen_HER2 <- OR_ajustado_intergen_HER2 %>% 
  mutate(HER2 = if_else(HER2 == "Positivo", 1,0))



#Conversión de la variable GRUPO en una VARIABLE CATEGÓRICA (y no numérica). Los modelos de regresión logística (glm(..., family = binomial)) esperan 
#que la variable dependiente sea un factor con dos niveles (por ejemplo, "Control" vs "Cancer").

OR_ajustado_intergen_HER2$HER2 <- as.factor(OR_ajustado_intergen_HER2$HER2)

#Establecer la variable EDAD como variable NUMÉRICA y no categórica.
OR_ajustado_intergen_HER2$Edad <- as.numeric(OR_ajustado_intergen_HER2$Edad)



#Cálculo de la regresión logística 
OR_ajustado_intergen_HER2 <- glm (HER2 ~ `Presencia Alelo Intergen`+ Edad, data = OR_ajustado_intergen_HER2, family = binomial)

#Selección de FILAS COEF
summary_ORtab_Intergen_HER2 <- summary (OR_ajustado_intergen_HER2)$coef



OR_HER2 <- data.frame(variable = rownames(summary_ORtab_Intergen_HER2),
                    oddsratio = round(exp(summary_ORtab_Intergen_HER2[,1]),3),
                    ci_low = round(exp(summary_ORtab_Intergen_HER2[,1] - 1.96*summary_ORtab_Intergen_HER2[,2]),3),
                    ci_high = round(exp(summary_ORtab_Intergen_HER2[,1] + 1.96*summary_ORtab_Intergen_HER2[,2]),3),
                    pval = scales::pvalue(summary_ORtab_Intergen_HER2[,4]),
                    row.names = NULL)

