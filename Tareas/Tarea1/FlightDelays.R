# Tabla de contingencia
tabla_dia_retraso <- table(FlightDelays$Day, FlightDelays$Delayed30)
tabla_dia_retraso

# Proporción de retrasos (al menos 30 min) por día
prop.table(tabla_dia_retraso, margin = 1)

ggplot(FlightDelays, aes(x = Delayed30, y = FlightLength, fill = Delayed30)) +
  geom_boxplot() +
  labs(title = "Longitud del Vuelo según Retraso de al menos 30 minutos",
       x = "¿Retrasado al menos 30 minutos? (Delayed30)",
       y = "Longitud del Vuelo en minutos (FlightLength)") +
  theme_minimal() +
  theme(legend.position = "none")