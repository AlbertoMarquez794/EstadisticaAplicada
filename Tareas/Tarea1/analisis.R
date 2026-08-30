library(resampledata3)
data(GSS2018)

# --- Inciso A: Tabla y gráfica de la pena de muerte ---
tabla_cappun <- table(GSS2018$DeathPenalty)
print(tabla_cappun)

barplot(
  tabla_cappun,
  main = "Opiniones sobre la Pena de Muerte",
  xlab = "Respuesta",
  ylab = "Frecuencia",
  col  = "skyblue"
)

# --- Inciso B: Tabla de la variable Courts incluyendo valores faltantes ---
tabla_courts <- table(GSS2018$Courts, exclude = NULL)
print(tabla_courts)

# --- Inciso C: Tabla de contingencia entre las dos variables ---
tabla_contingencia <- table(GSS2018$Courts, GSS2018$DeathPenalty)
print(tabla_contingencia)
