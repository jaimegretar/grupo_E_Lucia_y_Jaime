#------------------------------------------------------------------------------
# COMPARACIÓN SUICIDIOS ARIZONA VS ISLANDIA (ajustado por población)
#------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(lubridate)
#install.packages("patchwork")
library(patchwork)

# ============================================================
# PREPARACIÓN DE DATOS ARIZONA
# ============================================================

arizona_tasas <- Arizona_2015_2023 %>%
  # Convertir de formato ancho a largo
  pivot_longer(
    cols = `2015`:`2023`,
    names_to = "Year",
    values_to = "Deaths"
  ) %>%
  mutate(Year = as.numeric(Year)) %>%
  # Sumar todas las muertes por año (todos los condados, sexos y edades)
  group_by(Year) %>%
  summarise(
    Total_Deaths = sum(Deaths, na.rm = TRUE)
  ) %>%
  mutate(
    Region = "Arizona",
    Poblacion = 7400000,  # Población aproximada
    Tasa_100k = (Total_Deaths / Poblacion) * 100000
  )

# ============================================================
# PREPARACIÓN DE DATOS ISLANDIA
# ============================================================

# Filtrar datos de Islandia (2018-2023)
suicidios_Islandia <- df %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year >= 2015 & Year <= 2023)

islandia_tasas <- suicidios_Islandia %>%
  filter(Age == "Total") %>%  # Solo totales para evitar duplicar
  group_by(Year) %>%
  summarise(
    Total_Deaths = sum(value, na.rm = TRUE)
  ) %>%
  mutate(
    Region = "Islandia",
    Poblacion = 380000,  # Población aproximada
    Tasa_100k = (Total_Deaths / Poblacion) * 100000
  )

# ============================================================
# COMBINAR DATOS
# ============================================================

comparacion_tasas <- bind_rows(
  arizona_tasas %>% select(Year, Region, Tasa_100k, Total_Deaths),
  islandia_tasas %>% select(Year, Region, Tasa_100k, Total_Deaths)
)

# ============================================================
# GRÁFICO: Tasas ajustadas por población (COMPARACIÓN JUSTA)
# ============================================================

graf_tasas_comparadas <- ggplot(comparacion_tasas, aes(x = Year, y = Tasa_100k, color = Region, group = Region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = round(Tasa_100k, 1)), vjust = -1, size = 3) +
  scale_x_continuous(breaks = 2015:2023) +
  labs(
    title = "Tasa de Suicidios por 100,000 Habitantes",
    subtitle = "Comparación ajustada por población (2015-2023)",
    x = "Año",
    y = "Tasa por 100,000 hab.",
    color = "Región"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(graf_tasas_comparadas)

# ============================================================
# TABLA RESUMEN COMPLETA
# ============================================================

tabla_resumen_completa <- bind_rows(
  arizona_tasas %>% 
    summarise(
      Region = "Arizona",
      Poblacion_Aprox = 7400000,
      Media_Anual_Muertes = round(mean(Total_Deaths)),
      Total_Periodo = sum(Total_Deaths),
      Tasa_Media_100k = round(mean(Tasa_100k), 1)
    ),
  islandia_tasas %>% 
    summarise(
      Region = "Islandia",
      Poblacion_Aprox = 380000,
      Media_Anual_Muertes = round(mean(Total_Deaths)),
      Total_Periodo = sum(Total_Deaths),
      Tasa_Media_100k = round(mean(Tasa_100k), 1)
    )
)

cat("\n=== RESUMEN COMPARATIVO 2018-2023 ===\n")
print(tabla_resumen_completa)

# ============================================================
# PREPARACIÓN DATOS TEMPERATURA ARIZONA
# ============================================================

df_temp_arizona <- Arizona_temp_filtrado %>%
  mutate(
    fecha = as.Date(paste(Year, Month, "01", sep = "-")),
    mes = month(fecha, label = TRUE, abbr = FALSE),
    estacion = case_when(
      Month %in% c(12, 1, 2) ~ "Invierno",
      Month %in% c(3, 4, 5) ~ "Primavera",
      Month %in% c(6, 7, 8) ~ "Verano",
      Month %in% c(9, 10, 11) ~ "Otoño"
    )
  )

df_resumen_arizona <- df_temp_arizona %>%
  group_by(Year, estacion) %>%
  summarise(
    temp_media = mean(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Year) %>%
  summarise(temp_media = mean(temp_media, na.rm = TRUE))

# ============================================================
# PREPARACIÓN DATOS TEMPERATURA ISLANDIA
# ============================================================

df_temp_islandia <- Islandia_temp_json %>%
  spread_all() %>%
  select(fecha, temperatura_media) %>%
  mutate(
    fecha = as.Date(fecha),
    temperatura_media = as.numeric(temperatura_media),
    año = year(fecha),
    mes_num = month(fecha),
    estacion = case_when(
      mes_num %in% c(12, 1, 2) ~ "Invierno",
      mes_num %in% c(3, 4, 5) ~ "Primavera",
      mes_num %in% c(6, 7, 8) ~ "Verano",
      mes_num %in% c(9, 10, 11) ~ "Otoño"
    )
  )

df_resumen_islandia <- df_temp_islandia %>%
  group_by(año) %>%
  summarise(
    temp_media = mean(temperatura_media, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(Year = año)

# 1. PREPARAR DATOS DE TEMPERATURA (ya lo tienes)
arizona_temp_anual <- Arizona_temp_filtrado %>%
  group_by(Year) %>%
  summarise(temp_media = mean(Value, na.rm = TRUE))

# 2. PREPARAR DATOS DEL CSV 2015-2023
# Convertir a formato largo
arizona_condado_largo <- Arizona_2015_2023 %>%
  pivot_longer(
    cols = `2015`:`2023`,
    names_to = "Year",
    values_to = "Deaths"
  ) %>%
  mutate(Year = as.numeric(Year))

# Ver cómo quedó
View(arizona_condado_largo)

# Calcular totales anuales (suma de todos los condados, sexos y grupos de edad)
arizona_suic_anual <- arizona_condado_largo %>%
  group_by(Year) %>%
  summarise(
    Total_Deaths = sum(Deaths, na.rm = TRUE)
  )

# NOTA: Este CSV no tiene población, así que necesitas:
# Opción A: Buscar datos de población de Arizona por año
# Opción B: Usar solo el número total de muertes en lugar de tasa

# 3. COMBINAR CON TEMPERATURA
arizona_temp_suic <- arizona_temp_anual %>%
  inner_join(arizona_suic_anual, by = "Year") %>%
  filter(Year >= 2015 & Year <= 2023)

View(arizona_temp_suic)

# 4. GRÁFICO (ajustado para usar Total_Deaths en lugar de Tasa_100k)
factor_escala_az <- max(arizona_temp_suic$Total_Deaths, na.rm = TRUE) / 
  max(arizona_temp_suic$temp_media, na.rm = TRUE)

graf_temp_suic_arizona <- ggplot(arizona_temp_suic, aes(x = Year)) +
  # Barras de suicidios
  geom_col(aes(y = Total_Deaths, fill = "Total de Muertes"), 
           alpha = 0.6, width = 0.7) +
  # Línea de temperatura
  geom_line(aes(y = temp_media * factor_escala_az, color = "Temperatura"), 
            linewidth = 1.5) +
  geom_point(aes(y = temp_media * factor_escala_az, color = "Temperatura"), 
             size = 4) +
  # Configuración de ejes
  scale_y_continuous(
    name = "Total de Muertes",
    sec.axis = sec_axis(~ . / factor_escala_az, name = "Temperatura Media (°C)")
  ) +
  scale_x_continuous(breaks = 2015:2023) +
  scale_fill_manual(
    values = c("Total de Muertes" = "#e74c3c")
  ) +
  scale_color_manual(
    values = c("Temperatura" = "#f39c12")
  ) +
  labs(
    title = "Evolución Temperatura vs Total de Muertes",
    subtitle = "Arizona (2015-2023)",
    x = "Año",
    fill = "",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    axis.title.y.left = element_text(color = "#e74c3c"),
    axis.text.y.left = element_text(color = "#e74c3c"),
    axis.title.y.right = element_text(color = "#f39c12"),
    axis.text.y.right = element_text(color = "#f39c12"),
    panel.grid.minor = element_blank()
  )

print(graf_temp_suic_arizona)

# --- ISLANDIA: Temperatura (línea) vs Suicidios (barras) ---
# Factor de escala para el segundo eje
# 1. PREPARAR DATOS DE TEMPERATURA DE ISLANDIA
# Convertir el JSON a dataframe y calcular temperatura media anual
# ============================================================
# GRÁFICO ISLANDIA: Temperatura vs Suicidios
# ============================================================

# 1. PREPARAR TEMPERATURA ISLANDIA (ya lo tienes hecho)
df_temp_islandia <- Islandia_temp_json %>%
  spread_all() %>%
  select(fecha, temperatura_media) %>%
  mutate(
    fecha = as.Date(fecha),
    temperatura_media = as.numeric(temperatura_media),
    año = year(fecha),
    mes_num = month(fecha),
    estacion = case_when(
      mes_num %in% c(12, 1, 2) ~ "Invierno",
      mes_num %in% c(3, 4, 5) ~ "Primavera",
      mes_num %in% c(6, 7, 8) ~ "Verano",
      mes_num %in% c(9, 10, 11) ~ "Otoño"
    )
  )

islandia_temp_anual <- df_temp_islandia %>%
  group_by(año) %>%
  summarise(
    temp_media = mean(temperatura_media, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(Year = año)

# 2. PREPARAR SUICIDIOS ISLANDIA
# Ver qué columnas tiene suicidios_Islandia
str(suicidios_Islandia)

# Preparar datos de suicidios (ajusta los nombres de columnas según lo que tengas)
islandia_suic_anual <- suicidios_Islandia %>%
  group_by(Year) %>%
  summarise(
    Total_Deaths = sum(value, na.rm = TRUE)  # Ajusta "value" al nombre de tu columna
  )

# 3. COMBINAR TEMPERATURA Y SUICIDIOS
islandia_temp_suic <- islandia_temp_anual %>%
  inner_join(islandia_suic_anual, by = "Year") %>%
  filter(Year >= 1980 & Year <= 2023)  # Desde 1980

View(islandia_temp_suic)

# 4. GRÁFICO ISLANDIA
factor_escala_isl <- max(islandia_temp_suic$Total_Deaths, na.rm = TRUE) / 
  max(islandia_temp_suic$temp_media, na.rm = TRUE)

graf_temp_suic_islandia <- ggplot(islandia_temp_suic, aes(x = Year)) +
  geom_col(aes(y = Total_Deaths, fill = "Total de Muertes"), 
           alpha = 0.6, width = 0.7) +
  geom_line(aes(y = temp_media * factor_escala_isl, color = "Temperatura"), 
            linewidth = 1.5) +
  geom_point(aes(y = temp_media * factor_escala_isl, color = "Temperatura"), 
             size = 4) +
  scale_y_continuous(
    name = "Total de Muertes",
    sec.axis = sec_axis(~ . / factor_escala_isl, name = "Temperatura Media (°C)")
  ) +
  scale_x_continuous(breaks = seq(1980, 2023, by = 5)) +  # Cambiado: desde 1980
  scale_fill_manual(
    values = c("Total de Muertes" = "#2ecc71")
  ) +
  scale_color_manual(
    values = c("Temperatura" = "#3498db")
  ) +
  labs(
    title = "Evolución Temperatura vs Total de Muertes",
    subtitle = "Islandia (1980-2023)",  # Cambiado el subtítulo
    x = "Año",
    fill = "",
    color = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    axis.title.y.left = element_text(color = "#2ecc71"),
    axis.text.y.left = element_text(color = "#2ecc71"),
    axis.title.y.right = element_text(color = "#3498db"),
    axis.text.y.right = element_text(color = "#3498db"),
    panel.grid.minor = element_blank()
  )

print(graf_temp_suic_islandia)

# --- COMPARACIÓN CONJUNTA con facetas (dos gráficos lado a lado) ---
library(patchwork)

graf_comparacion_completa <- graf_temp_suic_arizona + graf_temp_suic_islandia +
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "Comparación: Temperatura vs Tasa de Suicidios",
    subtitle = "Arizona vs Islandia (2018-2023)",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    )
  )

print(graf_comparacion_completa)


# ============================================================
# Podria estar bien enfrentar suicidios por edad utilizando arizona_suicidios y suicidios_Islandia
# Que es donde estan separados los suicidios por edad y sexo

