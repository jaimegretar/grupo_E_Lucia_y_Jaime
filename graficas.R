#Grafica de suicidio de hombres y mujeres en Islandia.
# Cargar librerías necesarias
library(tidyverse)
library(rjstat)
library(jsonlite)
install.packages("plotly")
library(plotly)
library(lubridate)

#Grafica suicidios en Islandia
df_clean <- suicidios_Islandia

df_sexo <- df_clean %>%
  filter(Age == "Total", Sex != "Total")

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

p2 <- ggplot(df_resumen, aes(x = as.factor(año), y = temp_media, fill = estacion)) +
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

print(p2)
#------------------------------------------------------------------------------
#Grafica suicidios en Arizona por sexo
arizona_suic_sexo <- arizona_suicidios %>%
  filter(!is.na(Deaths), !is.na(Year)) %>%
  group_by(Year, Sex) %>%
  summarise(Total_Deaths = sum(Deaths, na.rm = TRUE), .groups = "drop")

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
#Grafica de termperatura en Arizona
df_temp_arizona <- Arizona_temp_csv %>%
  filter(Date >= 201801 & Date <= 202312) %>%  # Filtrar primero
  mutate(
    # Convertir de Fahrenheit a Celsius
    temperatura_media = (Value - 32) * 5/9,
    # Extraer año y mes
    año = Date %/% 100,
    mes_num = Date %% 100,
    # Crear fecha
    fecha = as.Date(paste(año, mes_num, "01", sep = "-")),
    mes = month(fecha, label = TRUE, abbr = FALSE),
    # Clasificar por estación
    estacion = case_when(
      mes_num %in% c(12, 1, 2) ~ "Invierno",
      mes_num %in% c(3, 4, 5) ~ "Primavera",
      mes_num %in% c(6, 7, 8) ~ "Verano",
      mes_num %in% c(9, 10, 11) ~ "Otoño"
    )
  ) %>%
  filter(!is.na(temperatura_media))

# Resumen por año y estación
df_resumen_arizona <- df_temp_arizona %>%
  group_by(año, estacion) %>%
  summarise(
    temp_media = mean(temperatura_media, na.rm = TRUE),
    temp_max = max(temperatura_media, na.rm = TRUE),
    temp_min = min(temperatura_media, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(estacion = factor(estacion, levels = c("Invierno", "Primavera", "Verano", "Otoño")))

# Gráfico de temperatura
graf_temp_arizona <- ggplot(df_resumen_arizona, aes(x = as.factor(año), y = temp_media, fill = estacion)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(
    values = c("Invierno" = "#3498db", "Primavera" = "#2ecc71", 
               "Verano" = "#f39c12", "Otoño" = "#e67e22")
  ) +
  labs(
    title = "Temperatura Media por Año y Estación",
    subtitle = "Arizona 2018-2023",
    x = "Año",
    y = "Temperatura Media (°C)",  # Cambiado a Celsius
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
#Grafica de evolucion temporal de temperatura
df_temp_mensual <- df_temp_arizona %>%
  group_by(año, mes_num) %>%
  summarise(temp_media = mean(temperatura_media, na.rm = TRUE), .groups = "drop") %>%
  mutate(fecha = as.Date(paste(año, mes_num, "01", sep = "-")))

graf_temp_evol <- ggplot(df_temp_mensual, aes(x = fecha, y = temp_media)) +
  geom_line(color = "#3498db", size = 1) +
  geom_smooth(method = "loess", color = "#e74c3c", linetype = "dashed", se = FALSE) +
  labs(title = "Evolución de la Temperatura Media en Arizona",
       subtitle = "Datos mensuales 2018-2023",
       x = "Fecha",
       y = "Temperatura Media (°C)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 16),
        panel.grid.minor = element_blank())

print(graf_temp_evol)

