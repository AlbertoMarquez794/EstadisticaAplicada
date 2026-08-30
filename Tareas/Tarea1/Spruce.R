plot(Spruce$Di.change, Spruce$Ht.change,
     main = "Relación entre Cambio en Altura y Cambio en Diámetro",
     xlab = "Cambio en Diámetro (Di.change)",
     ylab = "Cambio en Altura (Ht.change)",
     pch = 19, col = "darkblue")

# Línea de tendencia lineal
abline(lm(Ht.change ~ Di.change, data = Spruce), col = "red", lwd = 2)
