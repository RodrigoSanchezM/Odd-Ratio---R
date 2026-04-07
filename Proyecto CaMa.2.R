library(dplyr)
library(readxl)
library(epiR)
library(tidyverse)
library(DescTools)

setwd("/Users/rodrigosanchezmacedo/Documents/Proyecto CaMa")

esr1 <- read_excel("data/tablas/CaMa.xlsx","ESR1")
esr1 <- esr1%>%select("GRUPO", "Genotipo ESR1")



#eliminación de la filas con valor "."

esr1 <- esr1 %>% 
    filter( `Genotipo ESR1` != ".")


table(esr1$`Genotipo ESR1`, esr1$GRUPO)


#asignación de TRUE/FALSE para cada fila en base a la presencia de alelo mutanrte

esr1$alelo <- esr1$`Genotipo ESR1` == "AG" | esr1$`Genotipo ESR1` == "GG"


tabla_esr1 <- table(esr1$alelo,esr1$GRUPO)

OR_ESR1 <- epi.2by2(tabla_esr1, method = "case.control", conf.level = 0.95)


#Cálculo OR para región intergénica

intergen <- read_excel("data/tablas/CaMa.xlsx", "INTERGEN")
intergen <- intergen %>%select ("GRUPO","Genotipo Intergen") 

intergen <- intergen %>%
      filter(`Genotipo Intergen` != ".")

intergen$alelo <- intergen$`Genotipo Intergen`=="CT" | intergen$`Genotipo Intergen` =="TT"

tabla_intergen <- table(intergen$alelo,intergen$GRUPO)

OR_intergen <- epi.2by2(tabla_intergen, method = "case.control", conf.level = 0.95)

