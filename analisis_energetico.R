
# Paso 1: Definir vectores
energia <- c(rep("Renovable", 10), rep("No Renovable", 10))  # 10 valores de cada tipo de energía
consumo <- c(10, 12, NA, 14, 13, NA, 15, 16, 18, 20, 8, 9, NA, 11, 13, 14, 10, 12, 15, 17)  # Consumo en kWh
costo_kwh <- c(rep(0.15, 10), rep(0.20, 10))  # Costo por kWh para cada tipo de energía

# Paso 2: Limpieza de datos
for(i in 1:length(energia)) {
  if(is.na(consumo[i])) {
    if(energia[i] == "Renovable") {
      consumo[i] <- median(consumo[energia == "Renovable"], na.rm = TRUE)
    } else {
      consumo[i] <- median(consumo[energia == "No Renovable"], na.rm = TRUE)
    }
  }
}

# Paso 3: Crear el dataframe
df_consumo <- data.frame(
  energia = energia,
  consumo = consumo,
  costo_kwh = costo_kwh
)

# Paso 4: Cálculos
df_consumo$costo_total <- df_consumo$consumo * df_consumo$costo_kwh

# Total de consumo y costo por cada tipo de energía
total_consumo <- tapply(df_consumo$consumo, df_consumo$energia, sum)
total_costo <- tapply(df_consumo$costo_total, df_consumo$energia, sum)

# Media de consumo por tipo de energía
media_consumo <- tapply(df_consumo$consumo, df_consumo$energia, mean)

# Simulación de aumento del 10% en el costo
df_consumo$ganancia <- df_consumo$costo_total * 1.1

# Paso 5: Resumen
df_consumo_ordenado <- df_consumo[order(df_consumo$costo_total, decreasing = TRUE), ]

# Resumen de consumo y costo por tipo de energía
resumen_consumo <- data.frame(
  Total_Consumo = total_consumo,
  Total_Costo = total_costo
)

# Top 3 con el mayor costo_total
top_3_costos <- head(df_consumo_ordenado, 3)

# Lista resumen_energia
resumen_energia <- list(
  df_consumo_ordenado = df_consumo_ordenado,
  total_consumo = total_consumo,
  total_costo = total_costo,
  top_3_costos = top_3_costos
)

# Mostrar resumen_energia
print(resumen_energia)
