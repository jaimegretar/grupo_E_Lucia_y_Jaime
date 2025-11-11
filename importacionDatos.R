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
Arizona_suicidioRegion_csv <- read_delim(file="INPUT/DATA/Arizona/Salud/HDPulse_data_export.csv", delim = ",")
view(Arizona_suicidioRegion_csv)

# Importacion .csv datos suicidios de Arizona (relacionado con salud mental)
Arizona_suicidio2_csv <- read_delim(file="INPUT/DATA/Arizona/Salud/reports-data-export.csv", delim = ",")
view(Arizona_suicidio2_csv)

# Importacion .json datos suicidios de Islandia (relacionado con salud mental)
Islandia_suicidio_json <- fromJSONstat("INPUT/DATA/Islandia/Salud/suicidios_islandia.json")
df <- as.data.frame(Islandia_suicidio_json)
View(df)
str(df)
