library(resampledata)
library(dplyr)
library(ggplot2)

# 1. Cargar el dataset
data("ChiMarathonMen")

# 2. Filtrar los datos por división
datos_filtrados <- ChiMarathonMen %>%
  filter(Division %in% c("25-29", "35-39"))

# 3. Generar la gráfica de la ECDF
ggplot(datos_filtrados, aes(x = FinishMin, color = Division)) +
  stat_ecdf(geom = "step", linewidth = 1) +
  geom_vline(xintercept = 160, linetype = "dashed", color = "red") +
  labs(
    title = "ECDF de Tiempos del Maratón de Chicago 2015",
    x = "Tiempo (minutos)",
    y = "Proporción acumulada F_n(x)",
    color = "División"
  ) +
  theme_minimal()
