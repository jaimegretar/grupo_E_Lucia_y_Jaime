library(readr)
library(dplyr)
library(tidyverse)
library(rjson)
library(tidyjson)
install.packages("rjstat")
library(rjstat)
library(stringr)



# Importacion datos temperatura Arizona .json
Arizona_temp_json <- fromJSON(file = "INPUT/DATA/Arizona/Temperatura/data.json")
Arizona_temp_json %>%
  spread_all() %>%
  View()

# Importacion .csv datos temperatura Arizona
Arizona_temp_csv <- read_delim(file="INPUT/DATA/Arizona/Temperatura/data.csv", delim = ",")
View(Arizona_temp_csv)

Arizona_temp_filtrado <- Arizona_temp_csv %>%
  filter(Date >= 201801 & Date <= 202312)

View(Arizona_temp_filtrado)

# Importacion datos temperatura Arizona .json
Islandia_temp_json <- fromJSON(file = "INPUT/DATA/Islandia/Temperatura/temperatura_reykjavik_2018_2023.json")
Islandia_temp_json %>%
  spread_all() %>%
  View()

# Importacion .csv datos suicidios de Arizona (relacionado con salud mental)
Arizona_suicidioRegion_csv <- read_csv(
  file = "INPUT/DATA/Arizona/Salud/HDPulse_data_export.csv",
  col_names = c(
    "Region/Condado",
    "Codigo_FIPS",
    "Muertes_100,000",
    "CI_Inferior",
    "CI_Superior",
    "Num_Muertes_Anual",
    "Tendencia",
    "Tendencia_Anual",
    "CI_Inferior_rep",
    "CI_Superior_rep"
  ),
  skip = 1,              # Saltar la fila de encabezado defectuosa
  trim_ws = TRUE,
  show_col_types = FALSE
)

View(Arizona_suicidioRegion_csv)

#Importacion suicidios Arizona 2 del 2018 al 2023
# Leer el archivo nuevamente
datos2 <- read.csv("INPUT/DATA/Arizona/Salud/reports-data-export.csv",
                   header = TRUE,
                   stringsAsFactors = FALSE)

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
head(datos_separados)
str(datos_separados)

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

# Ver resumen
summary(arizona_csv)
head(arizona_csv)
View(arizona_csv)

# Filtrar solo suicidios
arizona_suicidios <- arizona_csv %>%
  filter(Intent == "Suicide")

# Ver el resultado
View(arizona_suicidios)

# Ver cuántos registros hay
nrow(arizona_suicidios)

# Ver resumen de los suicidios
summary(arizona_suicidios)

# Importacion .json datos suicidios de Islandia (relacionado con salud mental)
Islandia_suicidio_json <- fromJSONstat("INPUT/DATA/Islandia/Salud/suicidios_islandia.json")
df <- as.data.frame(Islandia_suicidio_json)
View(df)
str(df)

<<<<<<< HEAD
suicidios_Islandia <- df %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year >= 2018 & Year <= 2023)

# Verificar el resultado
str(suicidios_Islandia)
summary(suicidios_Islandia$Year)

# Ver años únicos
unique(suicidios_Islandia$Year)

# Contar observaciones por año
suicidios_Islandia %>%
  count(Year)
view(suicidios_Islandia)
=======
>>>>>>> c8c57a3c1d9f5f40acb7a111cdc8eca59e0a6027
