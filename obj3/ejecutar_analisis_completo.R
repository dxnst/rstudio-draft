# Script integrado para ejecutar todos los análisis con los cambios solicitados

# 1. Instalar y cargar paquetes
if (!require("meta")) install.packages("meta")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("readr")) install.packages("readr")
if (!require("viridis")) install.packages("viridis")
if (!require("RColorBrewer")) install.packages("RColorBrewer")

library(meta)
library(dplyr)
library(ggplot2)
library(readr)
library(viridis)
library(RColorBrewer)

cat("Paquetes cargados correctamente.\n")

# 2. Cargar datos
cat("Cargando datos...\n")

# IMC
imc_data <- read_csv("objetivo3/imc.csv")
cat("Datos de IMC cargados: ", nrow(imc_data), " estudios\n")

# HOMA-IR
homa_data <- read_csv("objetivo3/homa-ir.csv")
cat("Datos de HOMA-IR cargados: ", nrow(homa_data), " estudios\n")

# Glucosa en Ayunas
glucosa_data <- read_csv("objetivo3/glucosa-ayunas.csv")
cat("Datos de Glucosa en Ayunas cargados: ", nrow(glucosa_data), " estudios\n")

# Insulina en Ayunas
insulina_data <- read_csv("objetivo3/insulina-ayunas.csv")
cat("Datos de Insulina en Ayunas cargados: ", nrow(insulina_data), " estudios\n")

# Regularización del Ciclo Menstrual
ciclo_data <- read_csv("objetivo3/regularizacion-ciclo-menstrual.csv")
cat("Datos de Regularización del Ciclo Menstrual cargados: ", nrow(ciclo_data), " estudios\n")

# 3. Realizar todos los meta-análisis
cat("\nRealizando meta-análisis...\n")

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
  title = "Meta-análisis de IMC: Metformina + Inositol vs. Metformina"
)
cat("Meta-análisis de IMC completado\n")

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
  title = "Meta-análisis de HOMA-IR: Metformina + Inositol vs. Metformina"
)
cat("Meta-análisis de HOMA-IR completado\n")

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
  title = "Meta-análisis de Glucosa en Ayunas: Metformina + Inositol vs. Metformina"
)
cat("Meta-análisis de Glucosa en Ayunas completado\n")

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
  title = "Meta-análisis de Insulina en Ayunas: Metformina + Inositol vs. Metformina"
)
cat("Meta-análisis de Insulina en Ayunas completado\n")

# Meta-análisis para Regularización del Ciclo Menstrual (RR)
meta_ciclo_rr <- metabin(
  event.e = Intervention_Events,
  n.e = Intervention_Total,
  event.c = Control_Events,
  n.c = Control_Total,
  studlab = Study,
  data = ciclo_data,
  sm = "RR",
  method.tau = "REML",
  hakn = TRUE,
  title = "Meta-análisis de Regularización del Ciclo Menstrual: Metformina + Inositol vs. Metformina"
)
cat("Meta-análisis de Regularización del Ciclo Menstrual (RR) completado\n")

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
  title = "Meta-análisis de Regularización del Ciclo Menstrual: Metformina + Inositol vs. Metformina"
)
cat("Meta-análisis de Regularización del Ciclo Menstrual (OR) completado\n")

# 4. Crear resumen de resultados
cat("\nCreando resumen de resultados...\n")

# Preparar dataframe para el plot combinado
resultados_combinados <- data.frame(
  Outcome = c("IMC", "HOMA-IR", "Glucosa Ayunas", "Insulina Ayunas", "Regularización Menstrual (RR)"),
  Efecto = c(
    meta_imc$TE.random,
    meta_homa$TE.random,
    meta_glucosa$TE.random,
    meta_insulina$TE.random,
    meta_ciclo_rr$TE.random
  ),
  Lower = c(
    meta_imc$lower.random,
    meta_homa$lower.random,
    meta_glucosa$lower.random,
    meta_insulina$lower.random,
    meta_ciclo_rr$lower.random
  ),
  Upper = c(
    meta_imc$upper.random,
    meta_homa$upper.random,
    meta_glucosa$upper.random,
    meta_insulina$upper.random,
    meta_ciclo_rr$upper.random
  ),
  Medida = c("MD", "MD", "MD", "MD", "RR"),
  Pvalue = c(
    meta_imc$pval.random,
    meta_homa$pval.random,
    meta_glucosa$pval.random,
    meta_insulina$pval.random,
    meta_ciclo_rr$pval.random
  ),
  I2 = c(
    meta_imc$I2,
    meta_homa$I2,
    meta_glucosa$I2,
    meta_insulina$I2,
    meta_ciclo_rr$I2
  )
)

# Traducir nombres para el gráfico
resultados_combinados$Outcome_es <- c(
  "IMC", 
  "HOMA-IR", 
  "Glucosa en Ayunas", 
  "Insulina en Ayunas", 
  "Regularización Menstrual (RR)"
)

# Agregar columna con formato de efectos
resultados_combinados$Estimador <- ifelse(
  resultados_combinados$Medida == "RR",
  sprintf("%.2f [%.2f, %.2f]", 
          resultados_combinados$Efecto, 
          resultados_combinados$Lower, 
          resultados_combinados$Upper),
  sprintf("%.2f [%.2f, %.2f]", 
          resultados_combinados$Efecto, 
          resultados_combinados$Lower, 
          resultados_combinados$Upper)
)

# Agregar columna de significancia
resultados_combinados$Significativo <- ifelse(resultados_combinados$Pvalue < 0.05, "Sí", "No")

# Agregar interpretación de heterogeneidad
resultados_combinados$Heterogeneidad <- ifelse(
  resultados_combinados$I2 < 0.25, "Baja",
  ifelse(resultados_combinados$I2 < 0.50, "Moderada", "Alta")
)

# Traducir medidas
resultados_combinados$Medida_es <- ifelse(
  resultados_combinados$Medida == "MD", 
  "Diferencia de Medias", 
  "Riesgo Relativo"
)

# Mostrar tabla de resultados
print(resultados_combinados[, c("Outcome_es", "Estimador", "Significativo", "Pvalue", "Heterogeneidad")])

# 5. Crear forest plot combinado
cat("\nCreando forest plot combinado...\n")

# Definir paletas de colores
pal_significancia <- c("Sí" = "#3498DB", "No" = "#E74C3C")

# Crear forest plot personalizado con ggplot2
p <- ggplot(resultados_combinados, aes(y = reorder(Outcome_es, Efecto))) +
  # Línea vertical en el valor nulo
  geom_vline(aes(xintercept = ifelse(Medida == "RR", 1, 0)), 
             linetype = "dashed", color = "#7F8C8D", size = 0.5) +
  # Estimador puntual e intervalo
  geom_point(aes(x = Efecto, size = 1/I2, color = Significativo), alpha = 0.8) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper, color = Significativo), height = 0.2, size = 1) +
  # Texto con estimador
  geom_text(aes(x = ifelse(Medida == "RR", max(Upper)*1.2, max(Upper)*1.2), 
                label = Estimador), hjust = 0, size = 3.5, family = "sans") +
  # Colores y leyendas
  scale_color_manual(values = pal_significancia, name = "Estadísticamente\nsignificativo") +
  scale_size_continuous(name = "Precisión\n(1/Heterogeneidad)", range = c(3, 6)) +
  # Separar por tipo de medida
  facet_wrap(~Medida_es, scales = "free_x") +
  # Títulos y etiquetas
  labs(title = "Resumen de Meta-análisis: Metformina + Inositol vs. Metformina",
       subtitle = "Comparación de diferentes dosis combinadas vs. monoterapia",
       x = "Tamaño del Efecto",
       y = "",
       caption = "Los efectos a la derecha del eje '0' (MD) o '1' (RR) favorecen la combinación de Metformina + Inositol.\nSe eliminó la columna de pesos para mejorar la visualización.") +
  # Tema personalizado
  theme_minimal(base_family = "sans") +
  theme(
    text = element_text(family = "sans", color = "#2C3E50"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "#7F8C8D"),
    plot.caption = element_text(size = 8, color = "#7F8C8D", hjust = 0),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10, color = "#2C3E50"),
    axis.text.y = element_text(face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    panel.grid.major = element_line(color = "#ECF0F1", size = 0.2),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", color = "#2C3E50", size = 10),
    strip.background = element_rect(fill = "#ECF0F1", color = NA)
  )

# Mostrar gráfico
print(p)

# 6. Generar todos los PDFs
cat("\nGenerando PDFs de Forest Plots con detalles de intervención...\n")
source("generar_pdf_forest_plots_personalizado.R")

# 7. Guardar todos los resultados
cat("\nGuardando todos los resultados...\n")
ggsave("meta_analisis_resumen.pdf", p, width = 10, height = 6)
ggsave("meta_analisis_resumen.png", p, width = 10, height = 6, dpi = 300)
write.csv(resultados_combinados, "meta_analisis_resumen.csv", row.names = FALSE)

# 8. Mensaje final
cat("\n\n==============================================================\n")
cat("ANÁLISIS COMPLETADO: Todos los resultados han sido guardados\n")
cat("==============================================================\n\n")

cat("Los forest plots ahora incluyen el tipo de intervención para cada estudio\n")
cat("y se ha eliminado la columna de peso de efecto aleatorio.\n\n")

cat("Archivos generados:\n")
cat("- forest_plot_*.pdf: Forest plots detallados para cada resultado\n")
cat("- funnel_plot_*.pdf: Funnel plots para evaluar sesgo de publicación\n")
cat("- meta_analisis_resumen.pdf/png: Visualización gráfica combinada\n")
cat("- meta_analisis_resumen.csv: Tabla con los resultados numéricos\n\n")