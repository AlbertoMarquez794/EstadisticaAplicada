# ==============================================================================
# Tarea 2: Análisis Wiki4HE
# ==============================================================================

# 1. Cargar e instalar librerías necesarias
paquetes <- c("readr", "dplyr", "tidyr", "ggplot2")
paquetes_faltantes <- paquetes[!(paquetes %in% installed.packages()[, "Package"])]

if (length(paquetes_faltantes) > 0) {
  install.packages(paquetes_faltantes)
}

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# ==============================================================================
# Ingesta y Preparación de Datos
# ==============================================================================
data_path <- "/home/alberto/EstadisticaAplicada/Tareas/Tarea2/data/wiki4HE.csv"

# Cargar archivo delimitado por ';' tratando '?' como NA
df_raw <- read_delim(data_path, delim = ";", na = "?", show_col_types = FALSE)

# Normalizar nombres de columnas a mayúsculas
colnames(df_raw) <- toupper(colnames(df_raw))

# Mapear la columna DOMAIN a factores descriptivos
df <- df_raw %>%
  mutate(
    DOMAIN_NAME = factor(DOMAIN, 
      levels = 1:6,
      labels = c("Artes y Humanidades", 
                 "Ciencias Naturales", 
                 "Ciencias de la Salud", 
                 "Ingeniería y Arq.", 
                 "Derecho y Política", 
                 "Ciencias Sociales")
    )
  )

# ==============================================================================
# Inciso a.) Vista de Datos Demográficos
# ==============================================================================
demograficos <- df %>%
  filter(!is.na(DOMAIN_NAME)) %>%
  group_by(DOMAIN_NAME) %>%
  summarise(
    N_Profesores = n(),
    Porcentaje = round((n() / nrow(df)) * 100, 1),
    Edad_Promedio = round(mean(AGE, na.rm = TRUE), 1),
    Exp_Promedio = round(mean(YEARSEXP, na.rm = TRUE), 1)
  ) %>%
  arrange(desc(N_Profesores))

cat("\n=================== INCISO A: DEMOGRÁFICOS POR DOMINIO ===================\n")
print(demograficos)

# ==============================================================================
# Inciso b.) Preguntas con Mayoría de Respuestas Extremas (Likert 1 o 5)
# ==============================================================================
columnas_no_likert <- c("AGE", "GENDER", "DOMAIN", "DOMAIN_NAME", "PHD", 
                         "YEARSEXP", "UNIVERSITY", "UOC_POSITION", 
                         "OTHER_POSITION", "OTHERSTATUS", "USERIGHTS")

preguntas_likert <- setdiff(colnames(df), columnas_no_likert)

respuestas_extremas <- df %>%
  select(all_of(preguntas_likert)) %>%
  pivot_longer(cols = everything(), names_to = "Pregunta", values_to = "Valor") %>%
  filter(!is.na(Valor)) %>%
  group_by(Pregunta) %>%
  summarise(
    Total_Respuestas = n(),
    Pct_Extremo_1 = round(sum(Valor == 1) / n() * 100, 1),
    Pct_Extremo_5 = round(sum(Valor == 5) / n() * 100, 1),
    Pct_Total_Extremos = round(sum(Valor %in% c(1, 5)) / n() * 100, 1)
  ) %>%
  arrange(desc(Pct_Total_Extremos))

cat("\n=================== INCISO B: PREGUNTAS CON MÁS RESPUESTAS EXTREMAS ===================\n")
print(head(respuestas_extremas, 10))

# ==============================================================================
# Inciso c.) Comparación por Dominio: Consulta (EXP1) vs. Cita (VIS3)
# ==============================================================================
comparacion_dominio <- df %>%
  filter(!is.na(DOMAIN_NAME)) %>%
  group_by(DOMAIN_NAME) %>%
  summarise(
    Consulta_Area = round(mean(EXP1, na.rm = TRUE), 2),
    Cita_Articulos = round(mean(VIS3, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(Consulta_Area))

cat("\n=================== INCISO C: CONSULTA VS CITA POR DOMINIO ===================\n")
print(comparacion_dominio)

# ==============================================================================
# Visualización Gráfica (ggplot2)
# ==============================================================================

# Gráfica Inciso c: Comparación entre Consulta (EXP1) y Cita (VIS3)
df_plot_c <- comparacion_dominio %>%
  pivot_longer(cols = c(Consulta_Area, Cita_Articulos), 
               names_to = "Tipo_Uso", 
               values_to = "Promedio")

g1 <- ggplot(df_plot_c, aes(x = Promedio, y = reorder(DOMAIN_NAME, Promedio), fill = Tipo_Uso)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(
    values = c("Consulta_Area" = "#2b5c8f", "Cita_Articulos" = "#d95f02"),
    labels = c("Consulta en su área (EXP1)", "Cita en artículos (VIS3)")
  ) +
  coord_cartesian(xlim = c(1, 5)) +
  labs(
    title = "Consulta de Wikipedia vs. Cita en Artículos Académicos",
    subtitle = "Comparación de medias en escala Likert (1: Nunca a 5: Siempre)",
    x = "Promedio Likert",
    y = "Dominio de Expertise",
    fill = "Actividad"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

# Gráfica Inciso b: Top 10 preguntas con respuestas extremas
g2 <- ggplot(head(respuestas_extremas, 10), 
             aes(x = Pct_Total_Extremos, y = reorder(Pregunta, Pct_Total_Extremos))) +
  geom_col(fill = "#b2182b", width = 0.6) +
  geom_text(aes(label = paste0(Pct_Total_Extremos, "%")), hjust = -0.1, size = 3.5) +
  coord_cartesian(xlim = c(0, 100)) +
  labs(
    title = "Top 10 Preguntas con Mayor Porcentaje de Respuestas Extremas",
    subtitle = "Porcentaje acumulado de respuestas en las opciones 1 o 5",
    x = "% de Respuestas Extremas (1 o 5)",
    y = "Ítem de la Encuesta"
  ) +
  theme_minimal(base_size = 12)

# Desplegar gráficos
print(g1)
print(g2)