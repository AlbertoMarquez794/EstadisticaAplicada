library(resampledata)
data(Verizon)

# Definir la semilla para reproducibilidad
set.seed(42)

# Datos por grupo
time_clec <- Verizon$Time[Verizon$Group == "CLEC"]
time_ilec <- Verizon$Time[Verizon$Group == "ILEC"]

# ---------------------------------------------------------
# 1. ESTADÍSTICOS OBSERVADOS
# ---------------------------------------------------------
# A) Estándar: Diferencia de Medias
obs_diff_mean <- mean(time_clec) - mean(time_ilec)

# B) Robusto: Diferencia de Medias Recortadas (Trimming 10%)
obs_diff_trim <- mean(time_clec, trim = 0.1) - mean(time_ilec, trim = 0.1)

# C) Robusto: Diferencia de Medianas
obs_diff_median <- median(time_clec) - median(time_ilec)


# ---------------------------------------------------------
# 2. SIMULACIÓN DE PERMUTACIONES (k = 100,000)
# ---------------------------------------------------------
k <- 100000
n_total <- nrow(Verizon)
n_clec <- length(time_clec)

perm_mean <- numeric(k)
perm_trim <- numeric(k)
perm_median <- numeric(k)

all_times <- Verizon$Time

for (i in 1:k) {
  clec_idx <- sample(1:n_total, size = n_clec, replace = FALSE)
  
  sample_clec <- all_times[clec_idx]
  sample_ilec <- all_times[-clec_idx]
  
  # Estadísticos en la permutación
  perm_mean[i] <- mean(sample_clec) - mean(sample_ilec)
  perm_trim[i] <- mean(sample_clec, trim = 0.1) - mean(sample_ilec, trim = 0.1)
  perm_median[i] <- median(sample_clec) - median(sample_ilec)
}


# ---------------------------------------------------------
# 3. P-VALORES EMPÍRICOS (Unilaterales: P(Perm >= Observado))
# ---------------------------------------------------------
p_val_mean <- mean(perm_mean >= obs_diff_mean)
p_val_trim <- mean(perm_trim >= obs_diff_trim)
p_val_median <- mean(perm_median >= obs_diff_median)

# Resultados
cat("--- PRUEBA ESTÁNDAR (MEDIAS) ---\n")
cat("Diferencia observada:", round(obs_diff_mean, 4), "horas\n")
cat("P-valor por permutación:", p_val_mean, "\n\n")

cat("--- PRUEBA ROBUSTA (MEDIAS RECORTADAS AL 10%) ---\n")
cat("Diferencia observada recortada:", round(obs_diff_trim, 4), "horas\n")
cat("P-valor por permutación (robusto):", p_val_trim, "\n\n")

cat("--- PRUEBA ROBUSTA (MEDIANAS) ---\n")
cat("Diferencia observada de medianas:", round(obs_diff_median, 4), "horas\n")
cat("P-valor por permutación (medianas):", p_val_median, "\n")