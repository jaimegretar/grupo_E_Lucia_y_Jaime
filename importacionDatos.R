library(readr)
library(dplyr)
library(tidyverse)
library(rjson)
library(tidyjson)
library(rjstat)
library(stringr)

#==============================================================================
#Datos TEMPERATURA
#==============================================================================

#ARIZONA-----------------------------------------------------------------------
# Importacion .csv datos temperatura Arizona
Arizona_temp_csv <- read_delim(file="INPUT/DATA/Arizona/Temperatura/data.csv", delim = ",")
View(Arizona_temp_csv)

Arizona_temp_filtrado <- Arizona_temp_csv %>%
  filter(Date >= 198001 & Date <= 202312) %>%
  mutate(
    Year = Date %/% 100,
    Month = Date %% 100,
    Value = round((Value - 32) * 5/9, 1)
  ) %>%
  select(Year, Month, Value)

View(Arizona_temp_filtrado)


#ISLANDIA----------------------------------------------------------------------
# Importacion datos temperatura Islandia .json
Islandia_temp_json <- fromJSON(file = "INPUT/DATA/Islandia/Temperatura/temperatura_reykjavik_1980_2023.json")
Islandia_temp_json %>%
  spread_all() %>%
  View()


#==============================================================================
#Datos SUICIDIOS
#==============================================================================

#ARIZONA-----------------------------------------------------------------------

#1.Importacion suicidios Arizona del 2018 al 2023..............................
datos2 <- read.csv("INPUT/DATA/Arizona/Salud/reports-data-export.csv",
                   header = TRUE,
                   stringsAsFactors = FALSE)

View(datos2)

# Usar una expresión regular más sofisticada para separar respetando las comillas
datos_separados <- datos2 %>%
  mutate(linea = .[[1]]) %>%
  separate(linea,
           into = c("Intent", "Year", "Age_Group", "Sex", "Deaths", 
                    "Population", "Crude_Rate", "Age_Adjusted_Rate", 
                    "Years_Potential_Life_Lost"),
           sep = ',(?=(?:[^"]*"[^"]*")*[^"]*$)',  # Separar por comas fuera de comillas
           extra = "merge") %>%
  mutate(across(everything(), ~str_replace_all(., '"', ''))) %>%  # Quitar comillas
  select(-1)  # Eliminar la primera columna original

# Ver resultado
View(datos_separados)

# Limpieza de datos - convertir a numérico
arizona_csv <- datos_separados %>%
  mutate(
    Year = as.numeric(Year),
    Deaths = as.numeric(gsub("[^0-9]", "", Deaths)),
    Population = as.numeric(gsub(",", "", Population)),  # Solo quitar comas
    Crude_Rate = as.numeric(gsub("[*]", "", Crude_Rate)),
    Age_Adjusted_Rate = as.numeric(gsub("[*]", "", Age_Adjusted_Rate)),
    Years_Potential_Life_Lost = as.numeric(gsub(",", "", Years_Potential_Life_Lost))
  )

# Ver resultado
View(arizona_csv)

# Filtrar solo suicidios
arizona_suicidios <- arizona_csv %>%
  filter(Intent == "Suicide")

# Ver el resultado
View(arizona_suicidios)

#2.Importacion suicidios Arizona del 2015 al 2023...............................
# Primero leer todo como una sola columna
datos <- read_delim(
  "INPUT/DATA/Arizona/Salud/Arizona_condado_edad.csv",
  delim = "\n",  # Leer línea por línea
  col_names = FALSE,
  show_col_types = FALSE
)

View(datos)

# Separar y limpiar
Arizona <- datos %>%
  mutate(linea = .[[1]]) %>%
  separate(linea,
           into = c("County", "Sex", "Age_Group", "2015", "2016", 
                    "2017", "2018", "2019", "2020", "2021", "2022", "2023"),
           sep = ',(?=(?:[^"]*"[^"]*")*[^"]*$)',  # Separar por comas fuera de comillas
           extra = "merge") %>%
  mutate(across(everything(), ~str_replace_all(., '"', ''))) %>%  # Quitar comillas
  select(-1) %>%  # Eliminar la primera columna original
  slice(-1)  # Eliminar la fila de encabezados si quedó duplicada

# Convertir años a numérico
Arizona <- Arizona %>%
  mutate(across(`2015`:`2023`, ~as.numeric(.)))

# Ver resultado
head(Arizona)
str(Arizona)
View(Arizona)

Arizona_2015_2023 <- Arizona %>%
  mutate(across(`2015`:`2023`, ~replace_na(., 0)))
View(Arizona_2015_2023)


#ISLANDIA----------------------------------------------------------------------
# Importacion .json datos suicidios de Islandia (relacionado con salud mental)
Islandia_suicidio_json <- fromJSONstat("INPUT/DATA/Islandia/Salud/suicidios_islandia.json")
df <- as.data.frame(Islandia_suicidio_json)
View(df)
  
suicidios_Islandia <- df %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year >= 1981 & Year <= 2023)

# Contar observaciones por año
suicidios_Islandia %>%
  count(Year)

view(suicidios_Islandia)

