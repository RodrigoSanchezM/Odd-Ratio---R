#Script para cálculos de ORs ajustado del alelo ESR1 para CODOMINANCIA 
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


#Verificar frecuencias del alelo intergenico
table(tabla_completa$`Genotipo ESR1`, tabla_completa$GRUPO)

#########################################################################################################

#Filtro para ESR1
tabla_ESR1 <- tabla_completa %>% select("GRUPO", "Genotipo ESR1", "Edad")

#########################################################################################################

#Descarte de NA y genotipo = GG

ESR1_filtrado_AG <- tabla_ESR1 %>%
  filter(!is.na(`Genotipo ESR1`), `Genotipo ESR1` != "GG")





#Creación de una columna TRUE oe FALSE, a según la muestra presente la variante (AG(mt) = TRUE, GG = TRUE, y CC (wt) = FALSE)

ESR1_filtrado_AG$`ESR1 AG` <- ESR1_filtrado_AG$`Genotipo ESR1` == "AG" 





#Reemplazo valores columna "GRUPO"
ESR1_filtrado_AG <- ESR1_filtrado_AG %>% 
  mutate(GRUPO = if_else(GRUPO == "Paciente", 1,0))



#Conversión de la variable GRUPO en una VARIABLE CATEGÓRICA (y no numérica). Los modelos de regresión logística (glm(..., family = binomial)) esperan 
#que la variable dependiente sea un factor con dos niveles (por ejemplo, "Control" vs "Cancer").

ESR1_filtrado_AG$GRUPO <- as.factor(ESR1_filtrado_AG$GRUPO)



#Establecer la variable EDAD como variable NUMÉRICA y no categórica.
ESR1_filtrado_AG$Edad <- as.numeric(ESR1_filtrado_AG$Edad)






#Cálculo de la regresión logística 
OR_ajustado_AG <- glm (GRUPO ~ `ESR1 AG`+ Edad, data = ESR1_filtrado_AG, family = binomial)


#Selección de FILAS Coef

summary_ORtab_ESR1_AG <- summary (OR_ajustado_AG)$coef


#Oddsratio ajustado con la variable edad teniendo como grupo expuesto personas con el genotipo "AG"

data.frame(variable = rownames(summary_ORtab_ESR1_AG),
           oddsratio = round(exp(summary_ORtab_ESR1_AG[,1]),3),
           ci_low = round(exp(summary_ORtab_ESR1_AG[,1] - 1.96*summary_ORtab_ESR1_AG[,2]),3),
           ci_high = round(exp(summary_ORtab_ESR1_AG[,1] + 1.96*summary_ORtab_ESR1_AG[,2]),3),
           pval = scales::pvalue(summary_ORtab_ESR1_AG[,4]),
           row.names = NULL)


########################################################################################################################



#Descarte de NA y genotipo = AG

ESR1_filtrado_GG <- tabla_ESR1 %>%
  filter(!is.na(`Genotipo ESR1`), `Genotipo ESR1` != "AG")



#Creación de una columna TRUE oe FALSE, a según la muestra presente la variante (AG(mt) = TRUE, GG = TRUE, y CC (wt) = FALSE)

ESR1_filtrado_GG$`ESR1 GG` <- ESR1_filtrado_GG$`Genotipo ESR1` == "GG" 





#Reemplazo valores columna "GRUPO"
ESR1_filtrado_GG <- ESR1_filtrado_GG %>% 
  mutate(GRUPO = if_else(GRUPO == "Paciente", 1,0))



#Conversión de la variable GRUPO en una VARIABLE CATEGÓRICA (y no numérica). Los modelos de regresión logística (glm(..., family = binomial)) esperan 
#que la variable dependiente sea un factor con dos niveles (por ejemplo, "Control" vs "Cancer").

ESR1_filtrado_GG$GRUPO <- as.factor(ESR1_filtrado_GG$GRUPO)



#Establecer la variable EDAD como variable NUMÉRICA y no categórica.
ESR1_filtrado_GG$Edad <- as.numeric(ESR1_filtrado_GG$Edad)






#Cálculo de la regresión logística 
OR_ajustado_GG <- glm (GRUPO ~ `ESR1 GG`+ Edad, data = ESR1_filtrado_GG, family = binomial)


#Selección de FILAS Coef

summary_ORtab_ESR1_GG <- summary (OR_ajustado_GG)$coef


#Oddsratio ajustado con la variable edad teniendo como grupo expuesto personas con el genotipo "AG"

data.frame(variable = rownames(summary_ORtab_ESR1_GG),
           oddsratio = round(exp(summary_ORtab_ESR1_GG[,1]),3),
           ci_low = round(exp(summary_ORtab_ESR1_GG[,1] - 1.96*summary_ORtab_ESR1_GG[,2]),3),
           ci_high = round(exp(summary_ORtab_ESR1_GG[,1] + 1.96*summary_ORtab_ESR1_GG[,2]),3),
           pval = scales::pvalue(summary_ORtab_ESR1_GG[,4]),
           row.names = NULL)















