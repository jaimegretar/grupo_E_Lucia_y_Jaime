#------------------------------------------------------------------------------
#Gráfica de suicidios Arizona frente a Islandia
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

# ============================================================
# PREPARACIÓN DE DATOS ARIZONA
# ============================================================

# Filtrar datos de Arizona (2018-2023)
arizona_suicidios_filtrado <- arizona_suicidios %>%
  filter(Year >= 2018 & Year <= 2023) %>%
  group_by(Year) %>%
  summarise(
    Total_Deaths = sum(Deaths, na.rm = TRUE),
    Total_Population = mean(Population, na.rm = TRUE),
    Avg_Crude_Rate = mean(Crude_Rate, na.rm = TRUE)
  ) %>%
  mutate(Region = "Arizona")

# ============================================================
# PREPARACIÓN DE DATOS ISLANDIA
# ============================================================

# Filtrar y preparar datos de Islandia (2018-2023)
suicidios_Islandia <- df %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year >= 2018 & Year <= 2023)

# Agregar por año
islandia_suicidios_filtrado <- suicidios_Islandia %>%
  group_by(Year) %>%
  summarise(
    Total_Deaths = sum(value, na.rm = TRUE)
  ) %>%
  mutate(Region = "Islandia")

# ============================================================
# COMBINAR DATOS
# ============================================================

# Unir ambos datasets
comparacion_total <- bind_rows(
  arizona_suicidios_filtrado %>% select(Year, Total_Deaths, Region),
  islandia_suicidios_filtrado
)

# ============================================================
# VISUALIZACIÓN 1: Evolución temporal comparada
# ============================================================

grafico1 <- ggplot(comparacion_total, aes(x = Year, y = Total_Deaths, color = Region, group = Region)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(breaks = 2018:2023) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Evolución de Suicidios: Islandia vs Arizona (2018-2023)",
    subtitle = "Número total de muertes por año",
    x = "Año",
    y = "Número de Suicidios",
    color = "Región"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(grafico1)

# ============================================================
# VISUALIZACIÓN 2: Gráfico de barras comparativo
# ============================================================

grafico2 <- ggplot(comparacion_total, aes(x = as.factor(Year), y = Total_Deaths, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  geom_text(aes(label = Total_Deaths), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Comparación Anual de Suicidios",
    subtitle = "Islandia vs Arizona (2018-2023)",
    x = "Año",
    y = "Número de Suicidios",
    fill = "Región"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

print(grafico2)

# ============================================================
# VISUALIZACIÓN 3: Cambio porcentual respecto a 2018
# ============================================================

comparacion_cambio <- comparacion_total %>%
  group_by(Region) %>%
  arrange(Year) %>%
  mutate(
    Base_2018 = first(Total_Deaths),
    Cambio_Porcentual = ((Total_Deaths - Base_2018) / Base_2018) * 100
  )

grafico3 <- ggplot(comparacion_cambio, aes(x = Year, y = Cambio_Porcentual, color = Region, group = Region)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_x_continuous(breaks = 2018:2023) +
  labs(
    title = "Cambio Porcentual en Suicidios (Base 2018 = 0%)",
    subtitle = "Islandia vs Arizona",
    x = "Año",
    y = "Cambio Porcentual (%)",
    color = "Región"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(grafico3)

# ============================================================
# TABLA RESUMEN
# ============================================================

tabla_resumen <- comparacion_total %>%
  group_by(Region) %>%
  summarise(
    Media_Anual = mean(Total_Deaths, na.rm = TRUE),
    Total_Periodo = sum(Total_Deaths, na.rm = TRUE),
    Minimo = min(Total_Deaths, na.rm = TRUE),
    Maximo = max(Total_Deaths, na.rm = TRUE)
  )

print("RESUMEN ESTADÍSTICO 2018-2023:")
print(tabla_resumen)

# ============================================================
# VISUALIZACIÓN 4: Boxplot comparativo
# ============================================================

grafico4 <- ggplot(comparacion_total, aes(x = Region, y = Total_Deaths, fill = Region)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(
    title = "Distribución de Suicidios Anuales (2018-2023)",
    subtitle = "Comparación entre regiones",
    x = "Región",
    y = "Número de Suicidios"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none"
  )

print(grafico4)

#-----------------------------------------------------------
#-----------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

# ============================================================
# EXPLORACIÓN INICIAL DE DATOS
# ============================================================

cat("=== ESTRUCTURA ARIZONA ===\n")
cat("Total filas:", nrow(arizona_suicidios), "\n")
cat("Años únicos:", paste(unique(arizona_suicidios$Year), collapse=", "), "\n\n")

cat("Ejemplo de datos Arizona:\n")
print(head(arizona_suicidios %>% select(Year, Age_Group, Sex, Deaths), 10))

cat("\n=== ESTRUCTURA ISLANDIA ===\n")
cat("Total filas:", nrow(suicidios_Islandia), "\n")
cat("Años únicos:", paste(unique(suicidios_Islandia$Year), collapse=", "), "\n")
cat("Grupos de edad únicos:", paste(unique(suicidios_Islandia$Age), collapse=", "), "\n\n")

cat("Ejemplo de datos Islandia:\n")
print(head(suicidios_Islandia %>% select(Year, Sex, Age, value), 10))

# ============================================================
# PREPARACIÓN DE DATOS ARIZONA
# ============================================================

# Para Arizona: sumar Deaths por año (eliminar NA)
arizona_anual <- arizona_suicidios %>%
  filter(Year >= 2018 & Year <= 2023) %>%
  group_by(Year) %>%
  summarise(
    Total_Deaths = sum(Deaths, na.rm = TRUE),
    Num_Registros = n(),
    Registros_Con_Datos = sum(!is.na(Deaths))
  ) %>%
  mutate(Region = "Arizona")

cat("\n=== TOTALES ARIZONA POR AÑO ===\n")
print(arizona_anual)

# ============================================================
# PREPARACIÓN DE DATOS ISLANDIA
# ============================================================

# Para Islandia: solo tomar filas donde Age == "Total" para evitar duplicar
islandia_anual <- suicidios_Islandia %>%
  filter(Age == "Total") %>%  # CRÍTICO: solo totales por año
  group_by(Year) %>%
  summarise(
    Total_Deaths = sum(value, na.rm = TRUE),
    Num_Registros = n()
  ) %>%
  mutate(Region = "Islandia")

cat("\n=== TOTALES ISLANDIA POR AÑO ===\n")
print(islandia_anual)

# ============================================================
# VERIFICACIÓN DE POBLACIONES
# ============================================================

cat("\n=== CONTEXTO POBLACIONAL ===\n")
cat("Población Arizona (aprox. 2023): 7.4 millones\n")
cat("Población Islandia (aprox. 2023): 380,000\n")
cat("Ratio: Arizona es ~19.5 veces más grande\n\n")

# Calcular tasas por 100,000 habitantes
arizona_tasas <- arizona_anual %>%
  mutate(
    Poblacion = 7400000,  # Aproximado
    Tasa_100k = (Total_Deaths / Poblacion) * 100000
  )

islandia_tasas <- islandia_anual %>%
  mutate(
    Poblacion = 380000,  # Aproximado
    Tasa_100k = (Total_Deaths / Poblacion) * 100000
  )

cat("=== TASAS POR 100,000 HABITANTES ===\n")
print(arizona_tasas %>% select(Year, Total_Deaths, Tasa_100k))
print(islandia_tasas %>% select(Year, Total_Deaths, Tasa_100k))

# ============================================================
# COMBINAR DATOS
# ============================================================

comparacion_absoluta <- bind_rows(
  arizona_anual %>% select(Year, Total_Deaths, Region),
  islandia_anual %>% select(Year, Total_Deaths, Region)
)

comparacion_tasas <- bind_rows(
  arizona_tasas %>% select(Year, Region, Tasa_100k),
  islandia_tasas %>% select(Year, Region, Tasa_100k)
)

# ============================================================
# VISUALIZACIÓN 1: Números absolutos
# ============================================================

grafico1 <- ggplot(comparacion_absoluta, aes(x = Year, y = Total_Deaths, color = Region, group = Region)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = Total_Deaths), vjust = -1, size = 3) +
  scale_x_continuous(breaks = 2018:2023) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Suicidios en Números Absolutos: Islandia vs Arizona (2018-2023)",
    subtitle = "⚠️ Arizona tiene ~19.5 veces más población",
    x = "Año",
    y = "Número de Suicidios",
    color = "Región"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(grafico1)

# ============================================================
# VISUALIZACIÓN 2: Tasas ajustadas por población
# ============================================================

grafico2 <- ggplot(comparacion_tasas, aes(x = Year, y = Tasa_100k, color = Region, group = Region)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = round(Tasa_100k, 1)), vjust = -1, size = 3) +
  scale_x_continuous(breaks = 2018:2023) +
  labs(
    title = "Tasa de Suicidios por 100,000 Habitantes",
    subtitle = "Comparación ajustada por población",
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

print(grafico2)

# ============================================================
# VISUALIZACIÓN 3: Comparación lado a lado
# ============================================================

grafico3 <- ggplot(comparacion_absoluta, aes(x = as.factor(Year), y = Total_Deaths, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  geom_text(aes(label = Total_Deaths), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Comparación Anual de Suicidios",
    subtitle = "Números absolutos (poblaciones muy diferentes)",
    x = "Año",
    y = "Número de Suicidios",
    fill = "Región"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

print(grafico3)

# ============================================================
# VISUALIZACIÓN 4: Facetas para mejor comparación
# ============================================================

grafico4 <- ggplot(comparacion_tasas, aes(x = Year, y = Tasa_100k, group = 1)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(aes(label = round(Tasa_100k, 1)), vjust = -1, size = 3) +
  facet_wrap(~Region, scales = "free_y") +
  scale_x_continuous(breaks = 2018:2023) +
  labs(
    title = "Evolución de Tasas de Suicidio (2018-2023)",
    subtitle = "Por 100,000 habitantes - Escalas independientes",
    x = "Año",
    y = "Tasa por 100,000 hab."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 12)
  )

print(grafico4)

# ============================================================
# TABLA RESUMEN COMPLETA
# ============================================================

cat("\n=== RESUMEN COMPARATIVO 2018-2023 ===\n\n")

resumen_completo <- bind_rows(
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

print(resumen_completo)
