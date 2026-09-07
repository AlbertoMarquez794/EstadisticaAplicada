library(readr)
df_raw <- read_csv("/home/alberto/EstadisticaAplicada/Tareas/Tarea2/data/Airbnb_Open_Data.csv")
df_clean <- unique(df_raw)

if("price" %in% colnames(df_clean)) {
  df_clean$price <- as.numeric(gsub("[$,]", "", df_clean$price))
}

summary(df_clean)


hist(log10(df_clean$price + 1), 
     breaks = 50, 
     col = "steelblue", 
     main = "Distribución del Precio (Escala Logarítmica)",
     xlab = "log10(Precio)")

summary(df_clean$price)


names(df_clean)

colnames(df_clean) <- gsub(" ", "_", tolower(trimws(colnames(df_clean))))

# 1. Conteo por tipo de cuarto
table(df_clean$room_type)

# 2. Porcentaje por tipo de cuarto
prop.table(table(df_clean$room_type)) * 100

# 3. Comparación de precios (promedio y mediana) por tipo de cuarto
aggregate(price ~ room_type, data = df_clean, FUN = function(x) c(Media = mean(x, na.rm = TRUE), Mediana = median(x, na.rm = TRUE)))

# 4. Gráfico boxplot
boxplot(price ~ room_type, data = df_clean, log = "y", col = "lightgray",
        main = "Precio por Tipo de Cuarto", xlab = "Tipo de Cuarto", ylab = "Precio (Escala Log)")


        # 1. Crear el boxplot base
boxplot(price ~ room_type, data = df_clean, log = "y", col = "lightgray",
        main = "Precio por Tipo de Cuarto", xlab = "Tipo de Cuarto", ylab = "Precio (Escala Log)")

tapply(df_clean$price, df_clean$room_type, function(x) {
  quantile(x, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
})

# 1. Generar el boxplot base
boxplot(price ~ room_type, data = df_clean, log = "y", col = "lightgray",
        main = "Precio por Tipo de Cuarto", xlab = "Tipo de Cuarto", ylab = "Precio (Escala Log)")

# 2. Calcular P25, P50 y P75
percentiles_cuartiles <- c(0.25, 0.50, 0.75)
lista_cuartiles <- tapply(df_clean$price, df_clean$room_type, function(x) {
  quantile(x, probs = percentiles_cuartiles, na.rm = TRUE)
})

# 3. Dibujar las líneas sobre las cajas
for (i in seq_along(lista_cuartiles)) {
  vals <- lista_cuartiles[[i]]
  
  # P25 (azul)
  segments(x0 = i - 0.3, y0 = vals[1], x1 = i + 0.3, y1 = vals[1], col = "blue", lwd = 2.5)
  # P50 / Mediana (rojo)
  segments(x0 = i - 0.3, y0 = vals[2], x1 = i + 0.3, y1 = vals[2], col = "red", lwd = 3)
  # P75 (verde)
  segments(x0 = i - 0.3, y0 = vals[3], x1 = i + 0.3, y1 = vals[3], col = "darkgreen", lwd = 2.5)
}

# 4. Leyenda
legend("topright", legend = c("P25 (Q1)", "P50 (Mediana)", "P75 (Q3)"),
       col = c("blue", "red", "darkgreen"), lwd = 2.5, bty = "n")


# 1. Obtener el top 10 de vecindarios (ordenados de menor a mayor frecuencia 
#    para que al hacer el gráfico horizontal el #1 quede arriba)
top_vecindarios <- tail(sort(table(df_clean$neighbourhood)), 10)

# 2. Ampliar el margen izquierdo (mar = c(abajo, izquierda, arriba, derecha))
#    Aumentamos el segundo valor a 12 o 14 para dar espacio a los nombres largos
par(mar = c(5, 13, 4, 2) + 0.1)

# 3. Generar el gráfico de barras horizontal
bp <- barplot(top_vecindarios, 
              horiz = TRUE, 
              las = 1,                 # Mantiene los nombres en horizontal
              col = "#2a9d8f",         # Color verde azulado limpio
              border = "white",        # Bordes blancos entre barras
              main = "Top 10 Vecindarios con Mayor Número de Listados",
              xlab = "Número de Propiedades",
              cex.names = 0.85,        # Tamaño de fuente de los vecindarios
              cex.axis = 0.85,         # Tamaño de fuente de la escala
              xlim = c(0, max(top_vecindarios) * 1.15)) # Espacio extra a la derecha para texto

# 4. (Opcional) Agregar las cifras exactas al final de cada barra
text(x = top_vecindarios + (max(top_vecindarios) * 0.02), 
     y = bp, 
     labels = format(top_vecindarios, big.mark = ","), 
     pos = 4, 
     cex = 0.8, 
     col = "gray20")

# 5. Restaurar los márgenes por defecto
par(mar = c(5, 4, 4, 2) + 0.1)


plot(df_clean$long, df_clean$lat, 
     col = as.factor(df_clean$room_type), 
     pch = 19, cex = 0.3, 
     main = "Distribución Geográfica de Propiedades",
     xlab = "Longitud", ylab = "Latitud")
legend("topleft", legend = levels(as.factor(df_clean$room_type)), 
       col = 1:length(levels(as.factor(df_clean$room_type))), pch = 19)