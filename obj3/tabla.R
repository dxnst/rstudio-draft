# Script corregido para generar tabla resumen de meta-análisis
# Fecha: 2023-05-10 20:04:19
# Usuario: dxnst

library(meta)
library(dplyr)
library(knitr)
library(kableExtra)
library(readr)

# Función para formatear valores p de manera adecuada
format_pvalue <- function(p) {
  if (is.na(p)) return("N/A")
  if (p < 0.0001) return("<0.0001")
  if (p < 0.001) return(sprintf("%.4f", p))
  if (p < 0.01) return(sprintf("%.3f", p))
  if (p < 0.05) return(sprintf("%.3f", p))
  return(sprintf("%.3f", p))
}

# Cargar datos
cat("Cargando datos...\n")
imc_data <- read_csv("obj3/objetivo3/imc.csv")
homa_data <- read_csv("obj3/objetivo3/homa-ir.csv")
glucosa_data <- read_csv("obj3/objetivo3/glucosa-ayunas.csv")
insulina_data <- read_csv("obj3/objetivo3/insulina-ayunas.csv")
ciclo_data <- read_csv("obj3/objetivo3/regularizacion-ciclo-menstrual.csv")

# Verificar que los datos se cargaron correctamente
cat(sprintf("IMC: %d estudios, %d participantes\n", 
            nrow(imc_data), 
            sum(imc_data$Intervention_N + imc_data$Control_N)))
cat(sprintf("HOMA-IR: %d estudios, %d participantes\n", 
            nrow(homa_data), 
            sum(homa_data$Intervention_N + homa_data$Control_N)))
cat(sprintf("Glucosa: %d estudios, %d participantes\n", 
            nrow(glucosa_data), 
            sum(glucosa_data$Intervention_N + glucosa_data$Control_N)))
cat(sprintf("Insulina: %d estudios, %d participantes\n", 
            nrow(insulina_data), 
            sum(insulina_data$Intervention_N + insulina_data$Control_N)))
cat(sprintf("Regularización: %d estudios, %d participantes\n", 
            nrow(ciclo_data), 
            sum(ciclo_data$Intervention_Total + ciclo_data$Control_Total)))

# Realizar meta-análisis
cat("Realizando meta-análisis...\n")

# Meta-análisis para IMC
meta_imc <- metacont(
  n.e = Intervention_N,
  mean.e = Intervention_Mean,
  sd.e = Intervention_SD,
  n.c = Control_N,
  mean.c = Control_Mean,
  sd.c = Control_SD,
  studlab = Study,
  data = imc_data,
  sm = "MD",
  method.tau = "REML",
  hakn = TRUE,
  title = "Meta-análisis de IMC"
)

# Meta-análisis para HOMA-IR
meta_homa <- metacont(
  n.e = Intervention_N,
  mean.e = Intervention_Mean,
  sd.e = Intervention_SD,
  n.c = Control_N,
  mean.c = Control_Mean,
  sd.c = Control_SD,
  studlab = Study,
  data = homa_data,
  sm = "MD",
  method.tau = "REML",
  hakn = TRUE,
  title = "Meta-análisis de HOMA-IR"
)

# Meta-análisis para Glucosa en Ayunas
meta_glucosa <- metacont(
  n.e = Intervention_N,
  mean.e = Intervention_Mean,
  sd.e = Intervention_SD,
  n.c = Control_N,
  mean.c = Control_Mean,
  sd.c = Control_SD,
  studlab = Study,
  data = glucosa_data,
  sm = "MD",
  method.tau = "REML",
  hakn = TRUE,
  title = "Meta-análisis de Glucosa en Ayunas"
)

# Meta-análisis para Insulina en Ayunas
meta_insulina <- metacont(
  n.e = Intervention_N,
  mean.e = Intervention_Mean,
  sd.e = Intervention_SD,
  n.c = Control_N,
  mean.c = Control_Mean,
  sd.c = Control_SD,
  studlab = Study,
  data = insulina_data,
  sm = "MD",
  method.tau = "REML",
  hakn = TRUE,
  title = "Meta-análisis de Insulina en Ayunas"
)

# Meta-análisis para Regularización del Ciclo Menstrual (OR)
meta_ciclo_or <- metabin(
  event.e = Intervention_Events,
  n.e = Intervention_Total,
  event.c = Control_Events,
  n.c = Control_Total,
  studlab = Study,
  data = ciclo_data,
  sm = "OR",
  method.tau = "REML",
  hakn = TRUE,
  title = "Meta-análisis de Regularización del Ciclo Menstrual"
)

# Revisar resultados
cat("Verificando resultados del meta-análisis...\n")
cat(sprintf("IMC - pval: %f, I2: %f%%\n", meta_imc$pval.random, meta_imc$I2*100))
cat(sprintf("HOMA-IR - pval: %f, I2: %f%%\n", meta_homa$pval.random, meta_homa$I2*100))
cat(sprintf("Glucosa - pval: %f, I2: %f%%\n", meta_glucosa$pval.random, meta_glucosa$I2*100))
cat(sprintf("Insulina - pval: %f, I2: %f%%\n", meta_insulina$pval.random, meta_insulina$I2*100))
cat(sprintf("Regularización - pval: %f, I2: %f%%\n", meta_ciclo_or$pval.random, meta_ciclo_or$I2*100))

# Cálculo de medias ponderadas para cada grupo
# Función para calcular media ponderada
weighted_mean <- function(means, weights) {
  if (length(means) != length(weights) || length(means) == 0) return(NA)
  sum(means * weights) / sum(weights)
}

# IMC - Medias ponderadas
imc_int_mean <- weighted_mean(imc_data$Intervention_Mean, imc_data$Intervention_N)
imc_ctrl_mean <- weighted_mean(imc_data$Control_Mean, imc_data$Control_N)

# HOMA-IR - Medias ponderadas
homa_int_mean <- weighted_mean(homa_data$Intervention_Mean, homa_data$Intervention_N)
homa_ctrl_mean <- weighted_mean(homa_data$Control_Mean, homa_data$Control_N)

# Glucosa - Medias ponderadas
glucosa_int_mean <- weighted_mean(glucosa_data$Intervention_Mean, glucosa_data$Intervention_N)
glucosa_ctrl_mean <- weighted_mean(glucosa_data$Control_Mean, glucosa_data$Control_N)

# Insulina - Medias ponderadas
insulina_int_mean <- weighted_mean(insulina_data$Intervention_Mean, insulina_data$Intervention_N)
insulina_ctrl_mean <- weighted_mean(insulina_data$Control_Mean, insulina_data$Control_N)

# Regularización Menstrual - Proporciones
ciclo_int_prop <- sum(ciclo_data$Intervention_Events) / sum(ciclo_data$Intervention_Total)
ciclo_ctrl_prop <- sum(ciclo_data$Control_Events) / sum(ciclo_data$Control_Total)

# Función para determinar heterogeneidad
get_heterogeneidad <- function(i2) {
  if (is.na(i2)) return("N/A")
  if (i2 < 25) return("Baja")
  if (i2 < 50) return("Moderada")
  return("Alta")
}

# Función para determinar significancia
get_significancia <- function(pvalue) {
  if (is.na(pvalue)) return("N/A")
  if (pvalue < 0.05) return("Sí")
  return("No")
}

# Crear tabla resumen
resultados_tabla <- data.frame(
  Variable = c(
    "IMC (kg/m²)", 
    "HOMA-IR", 
    "Glucosa en ayunas (mg/dL)", 
    "Insulina en ayunas (µIU/mL)",
    "Regularización del ciclo menstrual (OR)"
  ),
  
  Estudios = c(
    nrow(imc_data),
    nrow(homa_data),
    nrow(glucosa_data),
    nrow(insulina_data),
    nrow(ciclo_data)
  ),
  
  Participantes = c(
    sum(imc_data$Intervention_N) + sum(imc_data$Control_N),
    sum(homa_data$Intervention_N) + sum(homa_data$Control_N),
    sum(glucosa_data$Intervention_N) + sum(glucosa_data$Control_N),
    sum(insulina_data$Intervention_N) + sum(insulina_data$Control_N),
    sum(ciclo_data$Intervention_Total) + sum(ciclo_data$Control_Total)
  ),
  
  Media_Combinacion = c(
    sprintf("%.2f", imc_int_mean),
    sprintf("%.2f", homa_int_mean),
    sprintf("%.2f", glucosa_int_mean),
    sprintf("%.2f", insulina_int_mean),
    sprintf("%.1f%%", ciclo_int_prop * 100)  # Porcentaje para datos binarios
  ),
  
  Media_Monoterapia = c(
    sprintf("%.2f", imc_ctrl_mean),
    sprintf("%.2f", homa_ctrl_mean),
    sprintf("%.2f", glucosa_ctrl_mean),
    sprintf("%.2f", insulina_ctrl_mean),
    sprintf("%.1f%%", ciclo_ctrl_prop * 100)  # Porcentaje para datos binarios
  ),
  
  Diferencia_Medias = c(
    sprintf("%.2f", imc_int_mean - imc_ctrl_mean),
    sprintf("%.2f", homa_int_mean - homa_ctrl_mean),
    sprintf("%.2f", glucosa_int_mean - glucosa_ctrl_mean),
    sprintf("%.2f", insulina_int_mean - insulina_ctrl_mean),
    sprintf("%.1f%%", (ciclo_int_prop - ciclo_ctrl_prop) * 100)  # Diferencia en porcentaje
  ),
  
  EstimadorEfecto = c(
    sprintf("%.2f", meta_imc$TE.random),
    sprintf("%.2f", meta_homa$TE.random),
    sprintf("%.2f", meta_glucosa$TE.random),
    sprintf("%.2f", meta_insulina$TE.random),
    sprintf("%.2f", meta_ciclo_or$TE.random)
  ),
  
  IC95 = c(
    sprintf("[%.2f, %.2f]", meta_imc$lower.random, meta_imc$upper.random),
    sprintf("[%.2f, %.2f]", meta_homa$lower.random, meta_homa$upper.random),
    sprintf("[%.2f, %.2f]", meta_glucosa$lower.random, meta_glucosa$upper.random),
    sprintf("[%.2f, %.2f]", meta_insulina$lower.random, meta_insulina$upper.random),
    sprintf("[%.2f, %.2f]", meta_ciclo_or$lower.random, meta_ciclo_or$upper.random)
  ),
  
  # Usar formato mejorado para valores p
  Valor_p = c(
    format_pvalue(meta_imc$pval.random),
    format_pvalue(meta_homa$pval.random),
    format_pvalue(meta_glucosa$pval.random),
    format_pvalue(meta_insulina$pval.random),
    format_pvalue(meta_ciclo_or$pval.random)
  ),
  
  Heterogeneidad_I2 = c(
    sprintf("%.1f%%", meta_imc$I2*100),
    sprintf("%.1f%%", meta_homa$I2*100),
    sprintf("%.1f%%", meta_glucosa$I2*100),
    sprintf("%.1f%%", meta_insulina$I2*100),
    sprintf("%.1f%%", meta_ciclo_or$I2*100)
  ),
  
  InterpretacionHeterogeneidad = c(
    get_heterogeneidad(meta_imc$I2*100),
    get_heterogeneidad(meta_homa$I2*100),
    get_heterogeneidad(meta_glucosa$I2*100),
    get_heterogeneidad(meta_insulina$I2*100),
    get_heterogeneidad(meta_ciclo_or$I2*100)
  ),
  
  Significativo = c(
    get_significancia(meta_imc$pval.random),
    get_significancia(meta_homa$pval.random),
    get_significancia(meta_glucosa$pval.random),
    get_significancia(meta_insulina$pval.random),
    get_significancia(meta_ciclo_or$pval.random)
  ),
  
  Interpretacion = c(
    ifelse(meta_imc$pval.random < 0.05, 
           ifelse(meta_imc$TE.random > 0, 
                  "Favorece Metformina sola", 
                  "Favorece terapia combinada"),
           "Sin diferencia significativa"),
    
    ifelse(meta_homa$pval.random < 0.05, 
           ifelse(meta_homa$TE.random > 0, 
                  "Favorece Metformina sola", 
                  "Favorece terapia combinada"),
           "Sin diferencia significativa"),
    
    ifelse(meta_glucosa$pval.random < 0.05, 
           ifelse(meta_glucosa$TE.random > 0, 
                  "Favorece Metformina sola", 
                  "Favorece terapia combinada"),
           "Sin diferencia significativa"),
    
    ifelse(meta_insulina$pval.random < 0.05, 
           ifelse(meta_insulina$TE.random > 0, 
                  "Favorece Metformina sola", 
                  "Favorece terapia combinada"),
           "Sin diferencia significativa"),
    
    ifelse(meta_ciclo_or$pval.random < 0.05, 
           ifelse(meta_ciclo_or$TE.random > 1, 
                  "Favorece terapia combinada", 
                  "Favorece Metformina sola"),
           "Sin diferencia significativa")
  )
)

# Mostrar tabla formateada con kableExtra
tabla_formateada <- kable(resultados_tabla, 
                          col.names = c("Variable", 
                                        "N° Estudios", 
                                        "N° Participantes", 
                                        "Media Comb.",
                                        "Media Mono.",
                                        "Diferencia",
                                        "Estimador", 
                                        "IC 95%", 
                                        "Valor p", 
                                        "I²", 
                                        "Heterogeneidad",
                                        "Estad. Significativo",
                                        "Interpretación"),
                          format = "html",
                          caption = "Tabla Resumen de Meta-Análisis: Metformina + Inositol vs. Metformina") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = TRUE) %>%
  row_spec(0, bold = TRUE, color = "white", background = "#2C3E50") %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(4:6, background = "#F8F9F9") %>%
  column_spec(9, bold = TRUE) %>% # Resaltar valor p
  row_spec(which(resultados_tabla$Significativo == "Sí"), background = "#D4F1F9") %>%
  add_header_above(c(" " = 3, "Valores por grupo" = 3, "Efecto Meta-Analítico" = 3, "Análisis de Heterogeneidad" = 2, "Significancia" = 2))

# Guardar tabla como HTML
writeLines(as.character(tabla_formateada), "resultados_meta_analisis_corregido.html")

# Mostrar mensaje de completado
cat("Tabla resumen de meta-análisis corregida generada y guardada\n")
cat("Fecha y hora de generación: 2023-05-10 20:04:19\n")
cat("Usuario: dxnst\n")