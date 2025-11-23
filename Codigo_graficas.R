# Cargar librerías necesarias  ---------------------------------------------------------
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

#================================================================================
# TEMPERATURA
#================================================================================
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
    subtitle = "Arizona 1980-2023",
    x = "Año",
    y = "Temperatura Media (°C)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank()
  )

print(graf_temp_arizona)


# Grafica de temperatura en Islandia
# Resumen anual ---------------------------------------------------------
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
    title = "Evolución de la Temperatura Media en Reykjavik",
    subtitle = "Reykjavik 1980-2023",
    x = "Año",
    y = "Temperatura Media (°C)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    panel.grid.minor = element_blank()
  )

print(graf_temp_islandia)


#================================================================================
# SUICIDIOS
#================================================================================
# Arizona
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

# Por sexo:
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

# Por región
region_suicidios_2015 <- Arizona_l %>%
  group_by(Year, County) %>%
  summarise(Promedio_Deaths = mean(Deaths, na.rm = TRUE), .groups = "drop") %>%
  group_by(Year) %>%
  slice_max(Promedio_Deaths, n = 15) %>%
  ungroup()

graf_region_arizona <- ggplot(region_suicidios_2015, aes(x = reorder(County, Promedio_Deaths), y = Promedio_Deaths)) +
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



# Islandia
df_clean <- suicidios_Islandia

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



#================================================================================
# PIB + Temperatura
#================================================================================
#Grafico de visualizacion con PIB Arizona:
ggplot(Arizona_temp_pib_anual, aes(x = Year)) +
  geom_col(aes(y = pib_per_capita / 1000, fill = "PIB per cápita"), alpha = 0.6) +
  geom_line(aes(y = temp_media_anual / 2, color = "Temperatura media anual"), linewidth = 1.1) +
  geom_point(aes(y = temp_media_anual / 2, color = "Temperatura media anual")) +
  scale_y_continuous(
    name = "PIB per cápita (miles de €)",
    sec.axis = sec_axis(~ . * 2, name = "Temperatura media anual (°C)")
  ) +
  scale_fill_manual(name = "", values = c("PIB per cápita" = "steelblue")) +
  scale_color_manual(name = "", values = c("Temperatura media anual" = "red")) +
  labs(
    title = "Arizona: PIB per cápita y temperatura media anual",
    x = "Año"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


#Grafico de visualizacion con PIB Islandia:
ggplot(islandia_temp_pib_anual, aes(x = Year)) +
  geom_col(aes(y = pib_per_capita / 1000, fill = "PIB per cápita"), alpha = 0.6) +
  geom_line(aes(y = temp_media_anual / 2, color = "Temperatura media anual"), linewidth = 1.1) +
  geom_point(aes(y = temp_media_anual / 2, color = "Temperatura media anual")) +
  scale_y_continuous(
    name = "PIB per cápita (miles de €)",
    sec.axis = sec_axis(~ . * 2, name = "Temperatura media anual (°C)")
  ) +
  scale_fill_manual(name = "", values = c("PIB per cápita" = "steelblue")) +
  scale_color_manual(name = "", values = c("Temperatura media anual" = "red")) +
  labs(
    title = "Islandia: PIB per cápita y temperatura media anual",
    x = "Año"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")



#================================================================================
# SUICIDIOS POR GRUPOS DE EDAD
#================================================================================
# Arizona
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


# Islandia.
islandia_edad_simpl <- suicidios_Islandia %>% # Nota: Usamos 'suicidios_Islandia' del paso 1.3.2.
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


# Realizamos la ANIMACIÓN con gganimate.
# Años del dataset
years_all <- sort(unique(comparacion_edad_simpl$Year))
# Años con datos de Arizona
years_arizona <- sort(unique(
  comparacion_edad_simpl$Year[comparacion_edad_simpl$Pais == "Arizona"]
))
# Años sin datos de Arizona (pero con Islandia)
years_no_data_arizona <- setdiff(years_all, years_arizona)

niveles_edad <- levels(factor(comparacion_edad_simpl$Grupo_edad))
x_centro <- niveles_edad[ceiling(length(niveles_edad) / 2)]

max_arizona <- comparacion_edad_simpl %>%
  filter(Pais == "Arizona") %>%
  summarise(maxT = max(Total_Deaths, na.rm = TRUE)) %>%
  pull(maxT)

no_data_df <- data.frame(
  Pais  = "Arizona",
  Year  = years_no_data_arizona,
  x_lab = x_centro,
  y_pos = max_arizona / 2,
  label = "No hay datos"
)

p_anim_edad <- ggplot(
  comparacion_edad_simpl,
  aes(x = Grupo_edad,
      y = Total_Deaths,
      fill = Grupo_edad)
) +
  #Para podeer colocarlo para que se vea bien.
  geom_col(width = 0.7) +
  geom_text(
    data = no_data_df,
    aes(x = x_lab, y = y_pos, label = label),
    inherit.aes = FALSE,
    size = 8,         
    fontface = "bold",
    hjust = 0.5,
    vjust = 0.5
  ) +
  facet_wrap(~ Pais, scales = "free_y") +
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
#Para cambiar la velocidad y otros ajustes.
anim_edad <- animate(
  p_anim_edad,
  nframes = 200,   
  fps =5 ,         
  width = 900,
  height = 500,
  renderer = gifski_renderer()
)

anim_edad
#anim_save("suicidios_grupos_edad_Arizona_Islandia.gif", animation = anim_edad)


# 1980 - 2023
# Hacemos el gráfico comparativo de suicidios por grupo de edad en ambos paises desde 1980.
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



# 2015 - 2023
# Filtramos solo los años 2015–2023
comparacion_edad_simpl_2015_2023 <- comparacion_edad_simpl %>%
  filter(Year >= 2015, Year <= 2023)

# Gráfico comparativo solo 2015–2023  ---------------------------------------------------------
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




#================================================================================
# SCATER PLOT: Temperatura y Suicidios
#================================================================================
# Arizona
# Total:
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
  group_by(Year) %>%          
  summarise(
    suicidios_totales = sum(Deaths, na.rm = TRUE),
    .groups = "drop"
  )
# --- Datos anuales ya existentes ---
suicidios <- arizona_suic_anual_2015 %>% 
  filter(Year >= 2015, Year <= 2023)

temperaturas <- Arizona_temp_anual %>%
  filter(Year >= 2015, Year <= 2023)

# --- Unión ---
arizona_temp_suic_2015 <- inner_join(
  temperaturas,
  suicidios,
  by = "Year"
)

# colnames(arizona_temp_suic_2015)

# --- Scatter ---
graf_arizona_temp_suic_2015 <- ggplot(arizona_temp_suic_2015,
                                      aes(x = temp_media_anual, y = suicidios_totales)) +
  geom_point(size = 4, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1.2) +
  labs(
    title = "Arizona: Suicidios anuales vs Temperatura media anual",
    subtitle = "(2015–2023)",
    x = "Temperatura media anual (°C)",
    y = "Número total de suicidios"
  ) +
  theme_minimal(base_size = 12)

print(graf_arizona_temp_suic_2015)

# Correlación simple que es el coeficiente de correlación de Pearson.
cor_arizona <- cor(arizona_temp_suic_2015$temp_media_anual,
                   arizona_temp_suic_2015$suicidios_totales,
                   use = "complete.obs") # complete.obs lo ponemos para evitar NAs
cor_arizona # ver el valor


# Por grupos de edad:
# Unimos la temperatura anual con los suicidios por grupo de edad en Arizona
arizona_edad_temp_csv <- arizona_edad_simpl %>%
  inner_join(Arizona_temp_anual, by = "Year")

#Creamos gráfico
graf_arizona_temp_edad_csv <- ggplot(
  arizona_edad_temp_csv,
  aes(x = temp_media_anual,
      y = Total_Deaths,
      color = Grupo_edad)
) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Arizona: Suicidios vs Temperatura media anual",
    subtitle = "Por grandes grupos de edad (2015–2023, CSV)",
    x = "Temperatura media anual (°C)",
    y = "Número de suicidios",
    color = "Grupo de edad"
  ) +
  theme_minimal(base_size = 12)

print(graf_arizona_temp_edad_csv)


# Islandia
# Total:
islandia_suic_anual <- suicidios_Islandia %>%
  filter(Year >= 1980, Year <= 2023,
         Sex == "Total",        
         !is.na(value)) %>%
  group_by(Year) %>%
  summarise(
    suicidios_totales = sum(value, na.rm = TRUE),
    .groups = "drop"
  )
# --- Datos anuales ya existentes ---
suicidios_Isl <- islandia_suic_anual %>% 
  filter(Year >= 1980, Year <= 2023)

temperaturas_Isl <- islandia_temp_anual %>%
  filter(Year >= 1980, Year <= 2023)

# --- Unión ---
islandia_temp_suic <- inner_join(
  temperaturas_Isl,
  suicidios_Isl,
  by = "Year"
)

# --- Scatter ---
graf_islandia_temp_suic <- ggplot(
  islandia_temp_suic,
  aes(x = temp_media_anual, y = suicidios_totales)
) +
  geom_point(size = 3, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1.2) +
  labs(
    title = "Islandia: Suicidios vs Temperatura media anual",
    subtitle = "1980–2023",
    x = "Temperatura media anual (°C)",
    y = "Número de suicidios"
  ) +
  theme_minimal(base_size = 12)

print(graf_islandia_temp_suic)

# Coeficiente de correlación de Pearson

cor_islandia <- cor(
  islandia_temp_suic$temp_media_anual,
  islandia_temp_suic$suicidios_totales,
  use = "complete.obs"
)
cor_islandia


# Por grupos de edad:
# Unimos suicidios por grupo de edad con la temperatura anual en Islandia
islandia_edad_temp <- islandia_edad_simpl %>%
  inner_join(islandia_temp_anual, by = "Year")
# Scatter plot: suicidios contra temperatura, con una línea por grupo de edad
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
