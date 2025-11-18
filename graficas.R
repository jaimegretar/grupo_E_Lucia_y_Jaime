#Grafica de suicidio de hombres y mujeres en Islandia.
# Cargar librerías necesarias
library(tidyverse)
library(rjstat)
library(jsonlite)
install.packages("plotly")
library(plotly)
library(lubridate)
#install.packages(c("dplyr", "stringr", "ggplot2", "gganimate", "gifski"))
library(dplyr)      
library(stringr)    
library(ggplot2)    
library(gganimate)  
library(gifski)     

#Grafica suicidios en Islandia
df_sexo <- suicidios_Islandia %>%
  filter(Age == "Total", Sex != "Total")

graf_suicHF_isl <- ggplot(df_sexo, aes(x = Year, y = value, color = Sex, group = Sex)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Males" = "#3498db", "Females" = "#e74c3c"),
                     labels = c("Females" = "Mujeres", "Males" = "Hombres")) +
  labs(title = "Suicidios por Sexo en Islandia",
       x = "Año",
       y = "Número de suicidios",
       color = "Sexo") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 16),
        panel.grid.minor = element_blank(),
        legend.position = "top")

print(graf_suicHF_isl)
#------------------------------------------------------------------------------
#Grafica de temperatura de Isalandia:
df_temp <- Islandia_temp_json %>%
  spread_all() %>%
  select(fecha, temperatura_media) %>%
  mutate(
    fecha = as.Date(fecha),
    temperatura_media = as.numeric(temperatura_media),
    año = year(fecha),
    mes = month(fecha, label = TRUE, abbr = FALSE),
    mes_num = month(fecha),
    estacion = case_when(
      mes_num %in% c(12, 1, 2) ~ "Invierno",
      mes_num %in% c(3, 4, 5) ~ "Primavera",
      mes_num %in% c(6, 7, 8) ~ "Verano",
      mes_num %in% c(9, 10, 11) ~ "Otoño"
    )
  )

df_resumen <- df_temp %>%
  group_by(año, estacion) %>%
  summarise(
    temp_media = mean(temperatura_media, na.rm = TRUE),
    temp_max = max(temperatura_media, na.rm = TRUE),
    temp_min = min(temperatura_media, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(estacion = factor(estacion, levels = c("Invierno", "Primavera", "Verano", "Otoño")))

graf_temp_islandia <- ggplot(df_resumen, aes(x = as.factor(año), y = temp_media, fill = estacion)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(
    values = c("Invierno" = "#3498db", "Primavera" = "#2ecc71", 
               "Verano" = "#f39c12", "Otoño" = "#e67e22")
  ) +
  labs(
    title = "Temperatura Media por Año y Estación",
    subtitle = "Reykjavik 2018-2023",
    x = "Año",
    y = "Temperatura Media (°C)",
    fill = "Estación"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

print(graf_temp_islandia)

#------------------------------------------------------------------------------
#Grafica suicidios en Arizona por sexo
arizona_suic_sexo <- arizona_suicidios %>%
  filter(!is.na(Deaths), !is.na(Year)) %>%
  group_by(Year, Sex) %>%
  summarise(Total_Deaths = sum(Deaths, na.rm = TRUE))

# Crear gráfico
graf_suicHF_ariz <- ggplot(arizona_suic_sexo, aes(x = Year, y = Total_Deaths, color = Sex, group = Sex)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Males" = "#3498db", "Females" = "#e74c3c"),
                     labels = c("Females" = "Mujeres", "Males" = "Hombres")) +
  labs(title = "Suicidios por Sexo en Arizona",
       subtitle = "2018-2023",
       x = "Año",
       y = "Número de suicidios",
       color = "Sexo") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 16),
        panel.grid.minor = element_blank(),
        legend.position = "top")

print(graf_suicHF_ariz)


#------------------------------------------------------------------------------
#Suicidios por grupo de edad en Arizona
arizona_suic_edad <- arizona_suicidios %>%
  filter(!is.na(Deaths), !is.na(Year), Age_Group != "Total") %>%
  group_by(Year, Age_Group) %>%
  summarise(Total_Deaths = sum(Deaths, na.rm = TRUE), .groups = "drop")

graf_suic_edad_ariz <- ggplot(arizona_suic_edad, aes(x = Year, y = Total_Deaths, fill = Age_Group)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Suicidios por Grupo de Edad en Arizona",
       subtitle = "2018-2023",
       x = "Año",
       y = "Número de suicidios",
       fill = "Grupo de Edad") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 16),
        panel.grid.minor = element_blank(),
        legend.position = "right")

print(graf_suic_edad_ariz)

#------------------------------------------------------------------------------
#Grafica de suicidios por region en Arizona
arizona_region_top <- Arizona_suicidioRegion_csv %>%
  filter(!is.na(Muertes_100_000)) %>%
  arrange(desc(Muertes_100_000)) %>%
  head(15)  # Top 15 regiones

graf_region_arizona <- ggplot(arizona_region_top, aes(x = reorder(Region, Muertes_100_000), y = Muertes_100_000)) +
  geom_col(fill = "#e74c3c") +
  geom_errorbar(aes(ymin = CI_Inferior, ymax = CI_Superior), width = 0.2, alpha = 0.6) +
  coord_flip() +
  labs(title = "Tasa de Suicidios por Región en Arizona",
       subtitle = "Top 15 Regiones con Mayor Tasa",
       x = "Región",
       y = "Tasa de Mortalidad por 100,000 habitantes") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 16),
        panel.grid.minor = element_blank())

print(graf_region_arizona)

#------------------------------------------------------------------------------
# Gráfica de temperatura en Arizona
df_temp_arizona <- Arizona_temp_filtrado %>%
  mutate(
    # Crear fecha para extraer mes y estación
    fecha = as.Date(paste(Year, Month, "01", sep = "-")),
    mes = month(fecha, label = TRUE, abbr = FALSE),
    estacion = case_when(
      Month %in% c(12, 1, 2) ~ "Invierno",
      Month %in% c(3, 4, 5) ~ "Primavera",
      Month %in% c(6, 7, 8) ~ "Verano",
      Month %in% c(9, 10, 11) ~ "Otoño"
    )
  )

# Resumen por año y estación
df_resumen_arizona <- df_temp_arizona %>%
  group_by(Year, estacion) %>%
  summarise(
    temp_media = mean(Value, na.rm = TRUE),
    temp_max = max(Value, na.rm = TRUE),
    temp_min = min(Value, na.rm = TRUE)
  ) %>%
  mutate(estacion = factor(estacion, levels = c("Invierno", "Primavera", "Verano", "Otoño")))

# Gráfico de temperatura
graf_temp_arizona <- ggplot(df_resumen_arizona, aes(x = as.factor(Year), y = temp_media, fill = estacion)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(
    values = c("Invierno" = "#3498db", "Primavera" = "#2ecc71", 
               "Verano" = "#f39c12", "Otoño" = "#e67e22")
  ) +
  labs(
    title = "Temperatura Media por Año y Estación",
    subtitle = "Arizona 2018-2023",
    x = "Año",
    y = "Temperatura Media (°C)",
    fill = "Estación"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

print(graf_temp_arizona)

#------------------------------------------------------------------------------
#15-11-2025
#------------------------------------------------------------------------------
#------------------------------------------------csdoifnnsipegfnsperigfpiser g

#Usamos filter para quedarnos con los datos de suicidios entre 2018 y 2023.
arizona_edad_simpl <- arizona_suicidios %>%
  filter(
    !is.na(Deaths),
    !is.na(Year),
    Age_Group != "Total",
    Year >= 2018, Year <= 2023
  ) %>%
  
  # sacamos el primer número de la categoría de edad
  mutate(
    edad_inicio = as.numeric(str_extract(Age_Group, "\\d+")),
    #clasificamos la edad de inicio en 4 grupos.
    Grupo_edad = case_when(
      edad_inicio < 20 ~ "0-19",
      edad_inicio < 40 ~ "20-39",
      edad_inicio < 60 ~ "40-59",
      TRUE            ~ "60+"
    )
  ) %>%
  #eliminamos filas que no tengan una edad de inicio valida
  filter(!is.na(edad_inicio)) %>%
  group_by(Year, Grupo_edad) %>%
  #calculamos el total de muertes por grupo de edad y año.
  summarise(
    Total_Deaths = sum(Deaths, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Pais = "Arizona")

# Hacemos lo mismo para Islandia.
islandia_edad_simpl <- suicidios_Islandia %>%
  filter(
    Age != "Total",
    Sex == "Total",
    Year >= 1980, Year <= 2023,
    !is.na(value)
  ) %>%
  #creamos la columna de edad de inicio y grupo de edad.
  mutate(
    edad_inicio = as.numeric(str_extract(Age, "\\d+")),
    Grupo_edad = case_when(
      edad_inicio < 20 ~ "0-19",
      edad_inicio < 40 ~ "20-39",
      edad_inicio < 60 ~ "40-59",
      TRUE            ~ "60+"
    )
  ) %>%
  # eliminamos filas sin edad de inicio valida.
  filter(!is.na(edad_inicio)) %>%
  group_by(Year, Grupo_edad) %>%
  #calculamos el total de muertes por grupo de edad y año.
  summarise(
    Total_Deaths = sum(value, na.rm = TRUE),
    .groups = "drop" # el drop es para evitar mensajes de agrupamiento.
  ) %>%
  mutate(Pais = "Islandia")

# Unimos ambos data frames para la comparación
comparacion_edad_simpl <- bind_rows(arizona_edad_simpl, islandia_edad_simpl) %>%
  mutate(
    Grupo_edad = factor(Grupo_edad,
                        levels = c("0-19", "20-39", "40-59", "60+"))
  )
# Hacemos el gráfico comparativo de suicidios por grupo de edad en ambos paises.
graf_comp_edad_simpl <- ggplot(
  comparacion_edad_simpl,
  aes(x = Year, y = Total_Deaths,
      color = Grupo_edad, group = Grupo_edad)
) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~ Pais, scales = "free_y") +   # escala libre por país
  labs(
    title = "Suicidios por Grandes Grupos de Edad",
    subtitle = "Comparación Arizona vs Islandia (1980–2023)",
    x = "Año",
    y = "Número de suicidios",
    color = "Grupo de edad"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

print(graf_comp_edad_simpl) #grafico de suicidios por grupo de edad en los dos sitios

#Realizamos la animación con gganimate. Si no entiendes algo en el video explica : https://youtu.be/pnSMtc1PH_w?si=EmseR8NhhXK7Yrkm
p_anim_edad <- ggplot(
  comparacion_edad_simpl,
  aes(x = Grupo_edad,
      y = Total_Deaths,
      fill = Grupo_edad)
) +
  geom_col(width = 0.7) +
  facet_wrap(~ Pais, scales = "free_y") +   # free_y para que cada país tenga su propia escala y se vea mejor
  labs(
    title = "Suicidios por grupos de edad",
    subtitle = "Año: {closest_state}",
    x = "Grupo de edad",
    y = "Número de suicidios",
    fill = "Grupos de edad"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  ) +
  transition_states(
    Year,
    transition_length = 2,
    state_length = 1
  ) +
  ease_aes("linear")

# Generamos la animacion. Laa he bajado la velocidad que al tener solo 5 años iba muy rapido.
anim_edad <- animate(
  p_anim_edad,
  nframes = 120,
  fps = 6,
  width = 900,
  height = 500,
  renderer = gifski_renderer()
)

anim_edad
# Pa guardar la imagen quitar el hastag, pero esque se me guarda cada vez que ejecuto por eso lo comento.
# anim_save("suicidios_grupos_edad_Arizona_Islandia.gif", animation = anim_edad)






#Ahora vamos a realizar un análisis de correlación entre temperatura media anual y suicidios anuales. Para ver si realmente hay una relación entre ambas variables.
# Suicidios anuales de Arizona
arizona_suic_anual <- arizona_suicidios %>%
  filter(Year >= 2018, Year <= 2023,
         !is.na(Deaths)) %>%
  group_by(Year) %>%
  summarise(
    suicidios_totales = sum(Deaths, na.rm = TRUE),
    .groups = "drop"
  )

# Temperatura anual de Arizona
arizona_temp_anual <- Arizona_temp_filtrado %>%
  group_by(Year) %>%
  summarise(
    temp_media_anual = mean(Value, na.rm = TRUE),
    .groups = "drop"
  )

#Unimos los dos data frames.
arizona_temp_suic <- inner_join(arizona_temp_anual,
                                arizona_suic_anual,
                                by = "Year")

arizona_temp_suic

# Hacemos un scatter plot con línea de regresión.
graf_arizona_temp_suic <- ggplot(arizona_temp_suic,
                                 aes(x = temp_media_anual,
                                     y = suicidios_totales)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Arizona: Suicidios vs Temperatura media anual",
    subtitle = "2018–2023",
    x = "Temperatura media anual (°F)",
    y = "Número de suicidios"
  ) +
  theme_minimal(base_size = 12)

print(graf_arizona_temp_suic)

# Correlación simple que es el coeficiente de correlación de Pearson.
cor_arizona <- cor(arizona_temp_suic$temp_media_anual,
                   arizona_temp_suic$suicidios_totales,
                   use = "complete.obs") # complete.obs lo ponemos para evitar NAs
cor_arizona # ver el valor

#Ahora en Islandia:
islandia_suic_anual <- suicidios_Islandia %>%
  filter(Year >= 1980, Year <= 2023,
         Sex == "Total",        
         !is.na(value)) %>%
  group_by(Year) %>%
  summarise(
    suicidios_totales = sum(value, na.rm = TRUE),
    .groups = "drop"
  )

#La temp media a partir de df_temp que ya tenemos creada.
islandia_temp_anual <- df_temp %>%
  group_by(año) %>%
  summarise(
    temp_media_anual = mean(temperatura_media, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(Year = año)

#Unimos los dos data frames.
islandia_temp_suic <- inner_join(islandia_temp_anual,
                                 islandia_suic_anual,
                                 by = "Year")

islandia_temp_suic

# Hacemos un scatter plot con línea de regresión.
graf_islandia_temp_suic <- ggplot(islandia_temp_suic,
                                  aes(x = temp_media_anual,
                                      y = suicidios_totales)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Islandia: Suicidios vs Temperatura media anual",
    subtitle = "1980–2023",
    x = "Temperatura media anual (°C)",
    y = "Número de suicidios"
  ) +
  theme_minimal(base_size = 12)

print(graf_islandia_temp_suic)
# Correlación simple que es el coeficiente de correlación de Pearson.
cor_islandia <- cor(islandia_temp_suic$temp_media_anual,
                    islandia_temp_suic$suicidios_totales,
                    use = "complete.obs") # complete.obs lo ponemos para evitar NAs
cor_islandia # da un valor positivo pero muy bajo.






