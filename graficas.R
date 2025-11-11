#Grafica de suicidio de hombres y mujeres en Islandia.
# Cargar librerías necesarias
library(tidyverse)
library(rjstat)
library(jsonlite)
library(plotly)
library(lubridate)

df_sexo <- df_clean %>%
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

