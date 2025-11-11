library(readr)
library(dplyr)
library(tidyverse)
library(rjson)
library(tidyjson)
install.packages("rjstat")
library(rjstat)



# Importacion datos temperatura Arizona .json
Arizona_temp_json <- fromJSON(file = "INPUT/DATA/Arizona/Temperatura/data.json")
Arizona_temp_json %>%
  spread_all() %>%
  View()

# Importacion datos temperatura Arizona .json
Islandia_temp_json <- fromJSON(file = "INPUT/DATA/Islandia/Temperatura/temperatura_reykjavik_2018_2023.json")
Islandia_temp_json %>%
  spread_all() %>%
  View()

# Importacion .csv datos temperatura Arizona
Arizona_temp_csv <- read_delim(file="INPUT/DATA/Arizona/Temperatura/data.csv", delim = ",")
View(Arizona_temp_csv)

# Importacion .csv datos suicidios de Arizona (relacionado con salud mental)
Arizona_suicidioRegion_csv <- read_csv(
  file = "INPUT/DATA/Arizona/Salud/HDPulse_data_export.csv",
  col_names = c(
    "Region",
    "FIPS",
    "Death_Rate",
    "CI_Lower",
    "CI_Upper",
    "Num_Deaths",
    "Trend",
    "Annual_Change",
    "CI_Lower_Change",
    "CI_Upper_Change"
  ),
  skip = 1,              # Saltar la fila de encabezado defectuosa
  trim_ws = TRUE,
  show_col_types = FALSE
)

View(Arizona_suicidioRegion_csv)

# Importacion .csv datos suicidios de Arizona (relacionado con salud mental)





# Importacion .json datos suicidios de Islandia (relacionado con salud mental)
Islandia_suicidio_json <- fromJSONstat("INPUT/DATA/Islandia/Salud/suicidios_islandia.json")
df <- as.data.frame(Islandia_suicidio_json)
View(df)
str(df)
