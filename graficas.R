# Cargar librerías necesarias
library(tidyverse)
library(rjstat)
library(jsonlite)
library(plotly)
library(lubridate)
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
#Grafica de temperatura de Islandia:
df_temp <- Islandia_temp_json %>% 
  spread_all() %>% 
  select(fecha, temperatura_media) %>% 
  mutate(
    fecha = as.Date(fecha),
    temperatura_media = as.numeric(temperatura_media),
    año = year(fecha)
  )

# Resumen anual
df_resumen <- df_temp %>% 
  group_by(año) %>% 
  summarise(
    temp_media = mean(temperatura_media, na.rm = TRUE),
    .groups = "drop"
  )

graf_temp_islandia <- ggplot(df_resumen, aes(x = año, y = temp_media)) +
  geom_line(linewidth = 1.2, color = "#3498db") +
  geom_point(size = 3, color = "#3498db") +
  labs(
    title = "Evolución de la Temperatura Media en Islandia",
    subtitle = "Reykjavik 2018-2023",
    x = "Año",
    y = "Temperatura Media (°C)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank()
  )

print(graf_temp_islandia)

#------------------------------------------------------------------------------
#Grafica suicidios en Arizona por sexo

Arizona_l <- Arizona_2015_2023 %>%
  pivot_longer(
    cols = `2015`:`2023`,
    names_to = "Year",
    values_to = "Deaths"
  ) %>%
  mutate(
    Year = as.numeric(Year),
    Deaths = as.numeric(Deaths)
  )


arizona_suic_sexo_2015 <- Arizona_l %>%
  filter(!is.na(Deaths)) %>% 
  group_by(Year, Sex) %>%
  summarise(Total_Deaths = sum(Deaths, na.rm = TRUE))

graf_suicHF_ariz <- ggplot(arizona_suic_sexo_2015,
                                aes(x = Year, y = Total_Deaths, color = Sex, group = Sex)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("Male" = "#3498db", "Female" = "#e74c3c"),
    labels = c("Female" = "Mujeres", "Male" = "Hombres")
  ) +
  labs(
    title = "Suicidios por Sexo en Arizona",
    subtitle = "2015–2023",
    x = "Año",
    y = "Número de suicidios",
    color = "Sexo"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

print(graf_suicHF_ariz)

#------------------------------------------------------------------------------
#Grafica de suicidios por region en Arizona
top15_all_years <- Arizona_l %>%
  group_by(Year, County) %>%
  summarise(Promedio_Deaths = mean(Deaths, na.rm = TRUE), .groups = "drop") %>%
  group_by(Year) %>%
  slice_max(Promedio_Deaths, n = 15) %>%
  ungroup()

graf_region_arizona <- ggplot(top15_all_years, aes(x = reorder(County, Promedio_Deaths), y = Promedio_Deaths)) +
  geom_col(fill = "#e74c3c") +
  facet_wrap(~Year, scales = "free_y") +
  coord_flip() +
  labs(
    title = "Top 15 Condados de Arizona por Suicidios",
    x = "Condado",
    y = "Número promedio de suicidios"
  ) +
  theme_minimal()

print(graf_region_arizona)

#------------------------------------------------------------------------------
# Gráfica de temperatura en Arizona
df_temp_arizona <- Arizona_temp_filtrado %>%
  mutate(
    fecha = as.Date(paste(Year, Month, "01", sep = "-"))
  )

# Resumen anual
df_resumen_arizona <- df_temp_arizona %>%
  group_by(Year) %>%
  summarise(
    temp_media = mean(Value, na.rm = TRUE),
    .groups = "drop"
  )

# Gráfico de temperatura
graf_temp_arizona <- ggplot(df_resumen_arizona, aes(x = Year, y = temp_media)) +
  geom_line(linewidth = 1.2, color = "#f39c12") +
  geom_point(size = 3, color = "#f39c12") +
  labs(
    title = "Evolución de la Temperatura Media en Arizona",
    subtitle = "Arizona 2018-2023",
    x = "Año",
    y = "Temperatura Media (°C)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank()
  )

print(graf_temp_arizona)

#------------------------------------------------------------------------------

#Usamos filter para quedarnos con los datos de suicidios entre 2018 y 2023.
arizona_edad_simpl <- Arizona_2015_2023 %>%
  # Convertimos de formato ancho a largo
  pivot_longer(
    cols = `2015`:`2023`,
    names_to = "Year",
    values_to = "Deaths"
  ) %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(
    !is.na(Deaths),
    Age_Group != "Total",
    Year >= 2015, Year <= 2023
  ) %>%
  # Sacamos el primer número de la categoría de edad
  mutate(
    edad_inicio = as.numeric(str_extract(Age_Group, "\\d+")),
    # Clasificamos la edad de inicio en 4 grupos
    Grupo_edad = case_when(
      edad_inicio < 20 ~ "0-19",
      edad_inicio < 40 ~ "20-39",
      edad_inicio < 60 ~ "40-59",
      TRUE            ~ "60+"
    )
  ) %>%
  # Eliminamos filas que no tengan una edad de inicio válida
  filter(!is.na(edad_inicio)) %>%
  group_by(Year, Grupo_edad) %>%
  # Calculamos el total de muertes por grupo de edad y año
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
graf_comp_edad_simpl_1980_2023 <- ggplot(
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

print(graf_comp_edad_simpl_1980_2023) #grafico de suicidios por grupo de edad en los dos sitios

# Filtramos solo los años 2015–2023
comparacion_edad_simpl_2015_2023 <- comparacion_edad_simpl %>%
  filter(Year >= 2015, Year <= 2023)

# Gráfico comparativo solo 2015–2023
graf_comp_edad_simpl_2015_2023 <- ggplot(
  comparacion_edad_simpl_2015_2023,
  aes(x = Year, y = Total_Deaths,
      color = Grupo_edad, group = Grupo_edad)
) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~ Pais, scales = "free_y") +
  labs(
    title = "Suicidios por Grandes Grupos de Edad",
    subtitle = "Comparación Arizona vs Islandia (2015–2023)",
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

print(graf_comp_edad_simpl_2015_2023)



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
anim_save("suicidios_grupos_edad_Arizona_Islandia.gif", animation = anim_edad)






#Ahora vamos a realizar un análisis de correlación entre temperatura media anual y suicidios anuales. Para ver si realmente hay una relación entre ambas variables.
# Suicidios anuales de Arizona desde 2015
# --- Construir suicidios totales por año desde el CSV 2015-2023 ---
arizona_suic_anual_2015 <- Arizona_2015_2023 %>%
  pivot_longer(
    cols = `2015`:`2023`,
    names_to = "Year",
    values_to = "Deaths"
  ) %>%
  mutate(
    Year = as.numeric(Year),
    Deaths = as.numeric(Deaths)
  ) %>%
  group_by(Year) %>%          # <-- ya no agrupamos por Age_Group
  summarise(
    suicidios_totales = sum(Deaths, na.rm = TRUE),
    .groups = "drop"
  )

# --- Temperatura anual ---
arizona_temp_anual_2015 <- Arizona_temp_filtrado %>%
  group_by(Year) %>%
  summarise(
    temp_media_anual = mean(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(Year >= 2015, Year <= 2023)

# --- Unión temperatura + suicidios ---
arizona_temp_suic_2015 <- inner_join(
  arizona_temp_anual_2015,
  arizona_suic_anual_2015,
  by = "Year"
)

# --- Scatter con una sola regresión ---
graf_arizona_temp_suic_2015 <- ggplot(
  arizona_temp_suic_2015,
  aes(
    x = temp_media_anual,
    y = suicidios_totales
  )
) +
  geom_point(size = 4, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1.2) +
  labs(
    title = "Arizona: Suicidios anuales vs Temperatura media anual",
    subtitle = "Un punto por año (2015–2023)",
    x = "Temperatura media anual (°C)",
    y = "Número total de suicidios"
  ) +
  theme_minimal(base_size = 12)

print(graf_arizona_temp_suic_2015)

#Si no muestra el grafico hacer esta linea de codigo para reiniciarlo
dev.off()


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


#Scatter mejorado con grupos de edad.
# Unimos la temperatura anual con los suicidios por grupo de edad en Arizona
# Preparar los datos de Arizona desde 2015
arizona_edad_csv <- Arizona_2015_2023 %>%
  pivot_longer(
    cols = `2015`:`2023`,
    names_to = "Year",
    values_to = "Deaths"
  ) %>%
  mutate(
    Year = as.numeric(Year),
    # Sacamos el primer número de Age_Group para definir grupos
    edad_inicio = as.numeric(str_extract(Age_Group, "\\d+")),
    Grupo_edad = case_when(
      edad_inicio < 20 ~ "0-19",
      edad_inicio < 40 ~ "20-39",
      edad_inicio < 60 ~ "40-59",
      TRUE            ~ "60+"
    )
  ) %>%
  filter(!is.na(edad_inicio)) %>%
  group_by(Year, Grupo_edad) %>%
  summarise(
    Total_Deaths = sum(Deaths, na.rm = TRUE),
    .groups = "drop"
  )


# Scatter plot por grupos de edad
# Preparar los datos de Arizona desde 2015
arizona_edad_csv <- Arizona_2015_2023 %>%
  pivot_longer(
    cols = `2015`:`2023`,
    names_to = "Year",
    values_to = "Deaths"
  ) %>%
  mutate(
    Year = as.numeric(Year),
    # Sacamos el primer número de Age_Group para definir grupos
    edad_inicio = as.numeric(str_extract(Age_Group, "\\d+")),
    Grupo_edad = case_when(
      edad_inicio < 20 ~ "0-19",
      edad_inicio < 40 ~ "20-39",
      edad_inicio < 60 ~ "40-59",
      TRUE            ~ "60+"
    )
  ) %>%
  filter(!is.na(edad_inicio)) %>%
  group_by(Year, Grupo_edad) %>%
  summarise(
    Total_Deaths = sum(Deaths, na.rm = TRUE),
    .groups = "drop"
  )

# Unimos con la temperatura anual
arizona_edad_temp_csv <- arizona_edad_csv %>%
  inner_join(arizona_temp_anual, by = "Year")

colnames(arizona_edad_temp_csv)

# Scatter plot por grupos de edad
graf_arizona_temp_edad_csv <- ggplot(
  arizona_edad_temp_csv,
  aes(x = temp_media,
      y = Total_Deaths,
      color = Grupo_edad)
) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +  # recta de regresión por grupo de edad
  labs(
    title = "Arizona: Suicidios vs Temperatura media anual",
    subtitle = "Por grandes grupos de edad (2015–2023, CSV)",
    x = "Temperatura media anual (°C)",
    y = "Número de suicidios",
    color = "Grupo de edad"
  ) +
  theme_minimal(base_size = 12)

print(graf_arizona_temp_edad_csv)



# 1) Unimos suicidios por grupo de edad con la temperatura anual en Islandia
islandia_edad_temp <- islandia_edad_simpl %>%
  inner_join(islandia_temp_anual, by = "Year")
# 2) Scatter plot: suicidios contra temperatura, con una línea por grupo de edad
graf_islandia_temp_edad <- ggplot(
  islandia_edad_temp,
  aes(x = temp_media_anual,
      y = Total_Deaths,
      color = Grupo_edad)
) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE) +  
  labs(
    title = "Islandia: Suicidios vs Temperatura media anual",
    subtitle = "Por grandes grupos de edad (1980–2023)",
    x = "Temperatura media anual (°C)",
    y = "Número de suicidios",
    color = "Grupo de edad"
  ) +
  theme_minimal(base_size = 12)

print(graf_islandia_temp_edad)




















