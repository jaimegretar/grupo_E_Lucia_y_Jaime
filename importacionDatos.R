library(readr)
library(dplyr)
library(tidyverse)
library(rjson)
library(tidyjson)
#install.packages("rjstat")
library(rjstat)
library(stringr)

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

# Importacion datos temperatura Arizona .json
Islandia_temp_json <- fromJSON(file = "INPUT/DATA/Islandia/Temperatura/temperatura_reykjavik_1980_2023.json")
Islandia_temp_json %>%
  spread_all() %>%
  View()

# Ver el rango de años en temperatura
range(Islandia_temp_json$año, na.rm = TRUE)

# Ver el rango de años en suicidios
range(suicidios_Islandia$Year, na.rm = TRUE)

# Ver qué años tienes después del join
range(islandia_temp_suic$Year, na.rm = TRUE)

# Importacion .csv datos suicidios de Arizona (relacionado con salud mental)
Arizona_suicidioRegion_csv <- read_csv(
  file = "INPUT/DATA/Arizona/Salud/HDPulse_data_export.csv",
  col_names = c(
    "Region",
    "Codigo_FIPS",
    "Muertes_100_000",
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
  
suicidios_Islandia <- df %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year >= 1981 & Year <= 2023)

# Verificar el resultado
str(suicidios_Islandia)
summary(suicidios_Islandia$Year)

# Ver años únicos
unique(suicidios_Islandia$Year)

# Contar observaciones por año
suicidios_Islandia %>%
  count(Year)

view(suicidios_Islandia)


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

# Opción 1: Usando read.csv (base R)
library(tidyverse)

# Primero leer todo como una sola columna
datos <- read_delim(
  "INPUT/DATA/Arizona/Salud/Arizona_condado_edad.csv",
  delim = "\n",  # Leer línea por línea
  col_names = FALSE,
  show_col_types = FALSE
)

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
