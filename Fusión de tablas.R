
library(dplyr)
library(readxl)
library(openxlsx)

setwd("/Users/rodrigosanchezmacedo/Documents/Proyecto CaMa/data/tablas")

tabla1 <- read_excel("Base de datos INEN Ca. Mama.xlsx","Base datos")

esr1 <- read_excel("CaMa.xlsx","ESR1")

datos_fincyt <- read_excel("datos faltantes fincyt.xlsx","Datos completos")

tabla_completa <- read_excel("Tabla CaMa_R.xlsx")

#homogenización del código de las muestras


tabla1 <- tabla1 %>%
  mutate(`Código Lab` = gsub("-", " ", `Código Lab`),                # Replace "-" with " "
         `Código Lab` = sprintf("INEN %03d", as.numeric(gsub("INEN ", "", `Código Lab`))))  # Add leading zeros

tabla1 <- tabla1%>% 
  select("Código Lab","Edad al DX","Estadio cancer", "HER2","ER","PR","Metástasis")


tabla1 <- tabla1 %>%
  rename(MUESTRA = `Código Lab`)





datos_fincyt <- datos_fincyt %>%
  mutate(Código = gsub("Fincyt-(\\d+)-\\d+", "FINCYT \\1", Código))

datos_fincyt <- datos_fincyt %>% 
    rename(MUESTRA = Código)



#Fusión de tablas 

merged_ESR1 <- left_join(esr1,tabla1, by = "MUESTRA")

merged_ESR1 <- left_join(merged_ESR1, datos_fincyt, by = "MUESTRA")



merged_ESR1 <- merged_ESR1 %>%
    select("MUESTRA", "GRUPO", "Edad.x", "Genotipo ESR1", "Estadio cancer.y", "ER.x", "PR.x", "HER2.x")

merged_ESR1 <- merged_ESR1 %>%
  mutate_all(~replace(., grepl("\\.", .), NA))


#Fusión de las tablas merged_ESR1 + datos_fincyt


tabla_ESR1 <- merged_ESR1 %>%
  mutate(ER.x = ifelse(is.na(ER.x), datos_fincyt$ER[match(MUESTRA, datos_fincyt$MUESTRA)], ER.x),
         `Estadio cancer.y`= ifelse(is.na(`Estadio cancer.y`), datos_fincyt$`Estadio del cáncer`[match(MUESTRA, datos_fincyt$MUESTRA)], `Estadio cancer.y`),
         PR.x = ifelse(is.na(PR.x), datos_fincyt$PR[match(MUESTRA, datos_fincyt$MUESTRA)], PR.x),
         HER2.x = ifelse(is.na(HER2.x), datos_fincyt$HER2[match(MUESTRA, datos_fincyt$MUESTRA)], HER2.x)
         )





#REEMPLAZO POR "0" Y "1" 

tabla_stata <- tabla_completa %>%
        mutate(GRUPO = ifelse(GRUPO == "Control", 0, 1))


tabla_stata <- tabla_stata %>%
        mutate(`Genotipo ESR1` = case_when(
          
                `Genotipo ESR1`== "AA" ~ 0,
                `Genotipo ESR1`== "AG" ~ 1,
                `Genotipo ESR1`== "GG" ~ 2,
          
          
          
        ))


tabla_stata <- tabla_stata %>%
  mutate(`Genotipo Intergen` = case_when(
    
    `Genotipo Intergen`== "CC" ~ 0,
    `Genotipo Intergen`== "CT" ~ 1,
    `Genotipo Intergen`== "TT" ~ 2,
    
    
    
  ))



tabla_stata <- tabla_stata %>%
      mutate( `Estadio cancer`= case_when(
        
        
        
        `Estadio cancer` == "I"~ 1,
        `Estadio cancer` == "II"~2,
        `Estadio cancer` == "III" ~ 3,
        `Estadio cancer` == "IV" ~ 4,
      ))






tabla_stata <- tabla_stata %>%
          mutate(ER = ifelse(ER == "Negativo",0,1))


tabla_stata <- tabla_stata %>%
          mutate(PR = ifelse(PR == "Negativo",0,1))


tabla_stata <- tabla_stata %>% 
            mutate(HER2 = ifelse(HER2 == "Negativo",0,1))
  







write.xlsx(tabla_ESR1, "Tabla CaMa_R.xlsx", sheetName = "ESR1")
#VALORAMOS EL TIPO DE DATOS DE NUESTRA TABLA

str(merged_ESR1)

#Adición de la columna "Genotipo Intergénica"

intergen <- read_excel("CaMa.xlsx", "INTERGEN")

result <- left_join(df1, df2, by = "id")

result <- left_join(df1, df2 %>% select(id, column_to_add), by = "id") %>%
  mutate(column_to_add = coalesce(column_to_add.x, column_to_add.y)) %>%
  select(-column_to_add.x, -column_to_add.y)


tabla_CAMA <- left_join(tabla_ESR1, intergen%>%select(MUESTRA, `Genotipo Intergen`), by = "MUESTRA" )

tabla_CAMA <- tabla_CAMA %>% 
            select(MUESTRA,GRUPO,Edad.x,`Genotipo ESR1`,`Genotipo Intergen`,`Estadio cancer.y`,ER.x,PR.x,HER2.x,)


write.xlsx(tabla_CAMA, "Tabla CaMa_R.xlsx", sheetName = "Datos Completos")

write.xlsx(tabla_stata, "Tabla stata.xlsx", sheetName = "Datos Completos")



































