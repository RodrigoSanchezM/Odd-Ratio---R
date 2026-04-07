#Análisis OR crudo

library(dplyr)
library(readxl)
library(epiR)
library(tidyverse)
library(DescTools)
library(oddsratio)


setwd("/Users/rodrigosanchezmacedo/Documents/Proyecto CaMa")



tabla_completa <- read_excel("data/tablas/Tabla CaMa_R.xlsx")


#verificar frecuencias

table(tabla_completa$`Genotipo ESR1`, tabla_completa$GRUPO)

table(tabla_completa$`Genotipo Intergen`, tabla_completa$GRUPO)


#Asignar columna de TRUE/FALSE para la presencia (TRUE) o ausencia del alelo mutante (FALSE)


tabla_completa$`Presencia alelo ESR1` <- tabla_completa$`Genotipo ESR1` == "AG" | tabla_completa$`Genotipo ESR1` == "GG"
tabla_completa$`Presencia alelo Intergen` <- tabla_completa$`Genotipo Intergen` == "CT" | tabla_completa$`Genotipo Intergen` == "TT"


tabla_ESR1<- table(tabla_completa$`Presencia alelo ESR1`, tabla_completa$GRUPO)

tabla_intergen <- table(tabla_completa$`Presencia alelo Intergen`, tabla_completa$GRUPO)



#Nombramiento de filas
rownames(tabla_ESR1) <- c("AA", "AG + GG")
rownames(tabla_intergen) <- c("CC", "CT + TT")


#reordenamiento de filas para que el programa epi.2by2, coja correctamente por default cuál es el grupo 
#con la presencia del alelo mutante (grupo expuesto) y cuál es el grupo no expuesto (contiene alelo wt)
tabla_ESR1 <- tabla_ESR1[nrow(tabla_ESR1):1,]
tabla_intergen <- tabla_intergen[nrow(tabla_intergen):1,]



#Cálculo OR crudo

OR_ESR1_a <- epi.2by2(tabla_ESR1, method = "case.control",  conf.level = 0.95)

OR_intergen_a <- epi.2by2(tabla_intergen, method = "case.control",  conf.level = 0.95)



#Generación de tablas para ESR1, considerando como grupo expuesto personas con alelo homociogto mutado (GG)


tabla_completa$`AA + AG o GG` <- tabla_completa$`Genotipo ESR1` == "GG"


tabla_ESR1b <- table(tabla_completa$`AA + AG o GG`, tabla_completa$GRUPO)


rownames(tabla_ESR1b) <- c("AA + AG", "GG")

#reordenamiento de filas
tabla_ESR1b <- tabla_ESR1b[nrow(tabla_ESR1):1,]


OR_ESR1_b <- epi.2by2(tabla_ESR1b, method = "case.control", conf.level = 0.95)


#Regresión logística multivariada (con edad )

#Selección de columnas

tabla_OR_ajustado_ESR1 <- tabla_completa %>% select("GRUPO","Edad","Presencia alelo ESR1")

#selección sin EDAD

tabla_OR_ajustado_intergen <- tabla_completa %>% select("GRUPO","Edad","Presencia alelo Intergen")

summary(tabla_OR_ajustado_ESR1)

summary(tabla_OR_ajustado_intergen)

#Reemplazo valores columna "GRUPO"

tabla_OR_ajustado_ESR1 <- tabla_OR_ajustado_ESR1 %>%
                mutate(GRUPO = if_else(GRUPO == "Control", 0,1))

tabla_OR_ajustado_intergen <- tabla_OR_ajustado_intergen %>% 
                mutate(GRUPO = if_else(GRUPO == "Control", 1,0))

str(tabla_OR_ajustado_ESR1)

#transformación de valores categóricos en valores factoriales

tabla_OR_ajustado_ESR1$GRUPO <- as.factor(tabla_OR_ajustado_ESR1$GRUPO)

tabla_OR_ajustado_ESR1$Edad <- as.numeric(tabla_OR_ajustado_ESR1$Edad)

tabla_OR_ajustado_intergen$GRUPO <- as.factor(tabla_OR_ajustado_intergen$GRUPO)

tabla_OR_ajustado_intergen$Edad <- as.numeric(tabla_OR_ajustado_intergen$Edad)




#OR ajustado
#Regresión logística binaria 


#ESR1 OR ajustado
OR_ajustado_ESR1 <- glm(GRUPO ~ `Presencia alelo ESR1` + Edad, data = tabla_OR_ajustado_ESR1, family = binomial)


summary(OR_ajustado_ESR1)




#Intergen OR ajustado
OR_ajustado_intergen <- glm (GRUPO ~ `Presencia alelo Intergen`+ Edad, data = tabla_OR_ajustado_intergen, family = binomial)

summary_ORtab_Intergen <- summary (OR_ajustado_intergen)$coef




data.frame(variable = rownames(summary_ORtab_Intergen),
           oddsratio = round(exp(summary_ORtab_Intergen[,1]),3),
           ci_low = round(exp(summary_ORtab_Intergen[,1] - 1.96*summary_ORtab_Intergen[,2]),3),
           ci_high = round(exp(summary_ORtab_Intergen[,1] + 1.96*summary_ORtab_Intergen[,2]),3),
           pval = scales::pvalue(summary_ORtab_Intergen[,4]),
           row.names = NULL)[-1]


summary(OR_ajustado_intergen)


#prueba cálculo OR con paquete "Oddsratio"

OR_ajustado_intergen_B <- or_glm (GRUPO ~ `Presencia alelo Intergen`+ Edad, data = tabla_OR_ajustado_intergen, family = binomial)

exp(OR_ajustado_intergen$coefficients[-1])


























                     

                     