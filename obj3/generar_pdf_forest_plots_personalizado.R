# Este script genera forest plots personalizados sin columna de peso y con descripción de intervenciones
# Usar este script después de ejecutar los análisis individuales

library(meta)
library(grDevices)
library(dplyr)

# Función para generar los forest plots personalizados en PDF
generar_forest_plot_pdf <- function(meta_obj, datos_origen, nombre_archivo, titulo, 
                                    label_x, label_izq, label_der, 
                                    xlim_val = NULL, es_binario = FALSE) {
  
  # Abrir el dispositivo PDF
  pdf(file = nombre_archivo, width = 11, height = 7)
  
  # Preparar los límites del eje x si no se proporcionan
  if (is.null(xlim_val)) {
    if (es_binario) {
      xlim_val <- c(0.5, 3)
    } else {
      xlim_val <- c(-5, 5)
    }
  }
  
  # Crear etiquetas personalizadas para estudios con tipo de intervención
  etiquetas_estudios <- paste0(datos_origen$Study, " (", datos_origen$Intervention_Group, ")")
  
  # Definir columnas que queremos mostrar (excluyendo pesos)
  if (es_binario) {
    # Para datos binarios (RR o OR)
    leftcols <- c("studlab", "event.e", "n.e", "event.c", "n.c")
  } else {
    # Para datos continuos (MD)
    leftcols <- c("studlab", "n.e", "mean.e", "sd.e", "n.c", "mean.c", "sd.c")
  }
  
  # Generar el forest plot personalizado
  forest(meta_obj, 
         sortvar = TE,
         studlab = etiquetas_estudios,  # Usar etiquetas personalizadas con tipo de intervención
         leftcols = leftcols,           # Solo mostrar estas columnas a la izquierda
         leftlabs = ifelse(es_binario,
                           c("Estudio (Intervención)", "Eventos", "Total", "Eventos", "Total"),
                           c("Estudio (Intervención)", "N", "Media", "DE", "N", "Media", "DE")),
         label.left = label_izq,
         label.right = label_der,
         prediction = TRUE,
         xlim = xlim_val,
         xlab = label_x,
         smlab = ifelse(es_binario, "Riesgo Relativo [IC 95%]", "Diferencia de medias [IC 95%]"),
         weight.study = "random",      # Usado para cálculos pero no se mostrará
         print.weight = FALSE,         # No mostrar columna de peso
         col.diamond = "#3498DB",        
         col.predict = "#E74C3C",        
         col.square = "#2C3E50",         
         col.study = "black",            
         col.by = "#2ECC71",             
         col.label.right = "#9B59B6",    
         col.label.left = "#9B59B6",     
         text.random = "Modelo de efectos aleatorios",
         text.fixed = "Modelo de efectos fijos",
         text.predict = "Intervalo de predicción",
         fontsize = 10)
  
  # Cerrar el dispositivo PDF
  dev.off()
  
  # Confirmar generación
  cat(paste("Forest plot generado:", nombre_archivo, "\n"))
}

# Función para generar funnel plots y guardarlos en PDF
generar_funnel_plot_pdf <- function(meta_obj, nombre_archivo, titulo, label_x, es_binario = FALSE) {
  
  # Abrir el dispositivo PDF
  pdf(file = nombre_archivo, width = 8, height = 7)
  
  # Generar el funnel plot
  funnel(meta_obj, 
         xlab = label_x,
         main = titulo, 
         studlab = TRUE,
         col.fixed = "#3498DB",
         col.random = "#E74C3C",
         col.predict = "#2ECC71")
  
  # Cerrar el dispositivo PDF
  dev.off()
  
  # Confirmar generación
  cat(paste("Funnel plot generado:", nombre_archivo, "\n"))
}

# ============= GENERAR FOREST PLOTS PARA CADA ANÁLISIS =================

# IMC
if (exists("meta_imc") && exists("imc_data")) {
  generar_forest_plot_pdf(
    meta_imc, 
    imc_data,
    "forest_plot_imc.pdf", 
    "Forest Plot para IMC", 
    "Diferencia de medias en IMC", 
    "Favorece combinación", 
    "Favorece monoterapia", 
    c(-5, 5)
  )
  
  generar_funnel_plot_pdf(
    meta_imc,
    "funnel_plot_imc.pdf",
    "Gráfico de embudo para IMC",
    "Diferencia de medias"
  )
}

# HOMA-IR
if (exists("meta_homa") && exists("homa_data")) {
  generar_forest_plot_pdf(
    meta_homa, 
    homa_data,
    "forest_plot_homa.pdf", 
    "Forest Plot para HOMA-IR", 
    "Diferencia de medias en HOMA-IR", 
    "Favorece combinación", 
    "Favorece monoterapia", 
    c(-1, 1)
  )
  
  generar_funnel_plot_pdf(
    meta_homa,
    "funnel_plot_homa.pdf",
    "Gráfico de embudo para HOMA-IR",
    "Diferencia de medias"
  )
}

# Glucosa en Ayunas
if (exists("meta_glucosa") && exists("glucosa_data")) {
  generar_forest_plot_pdf(
    meta_glucosa, 
    glucosa_data,
    "forest_plot_glucosa.pdf", 
    "Forest Plot para Glucosa en Ayunas", 
    "Diferencia de medias en Glucosa en Ayunas (mg/dL)", 
    "Favorece combinación", 
    "Favorece monoterapia", 
    c(-5, 5)
  )
  
  generar_funnel_plot_pdf(
    meta_glucosa,
    "funnel_plot_glucosa.pdf",
    "Gráfico de embudo para Glucosa en Ayunas",
    "Diferencia de medias"
  )
}

# Insulina en Ayunas
if (exists("meta_insulina") && exists("insulina_data")) {
  generar_forest_plot_pdf(
    meta_insulina, 
    insulina_data,
    "forest_plot_insulina.pdf", 
    "Forest Plot para Insulina en Ayunas", 
    "Diferencia de medias en Insulina en Ayunas (µIU/mL)", 
    "Favorece combinación", 
    "Favorece monoterapia", 
    c(-3, 3)
  )
  
  generar_funnel_plot_pdf(
    meta_insulina,
    "funnel_plot_insulina.pdf",
    "Gráfico de embudo para Insulina en Ayunas",
    "Diferencia de medias"
  )
}

# Regularización del Ciclo Menstrual - RR
if (exists("meta_ciclo_rr") && exists("ciclo_data")) {
  generar_forest_plot_pdf(
    meta_ciclo_rr, 
    ciclo_data,
    "forest_plot_ciclo_rr.pdf", 
    "Forest Plot para Regularización Menstrual (RR)", 
    "Riesgo Relativo de Regularización Menstrual", 
    "Favorece monoterapia", 
    "Favorece combinación", 
    c(0.5, 3),
    TRUE
  )
  
  generar_funnel_plot_pdf(
    meta_ciclo_rr,
    "funnel_plot_ciclo_rr.pdf",
    "Gráfico de embudo para Regularización Menstrual (RR)",
    "Riesgo Relativo",
    TRUE
  )
}

# Regularización del Ciclo Menstrual - OR
if (exists("meta_ciclo_or") && exists("ciclo_data")) {
  generar_forest_plot_pdf(
    meta_ciclo_or, 
    ciclo_data,
    "forest_plot_ciclo_or.pdf", 
    "Forest Plot para Regularización Menstrual (OR)", 
    "Odds Ratio de Regularización Menstrual", 
    "Favorece monoterapia", 
    "Favorece combinación", 
    c(0.5, 10),
    TRUE
  )
  
  generar_funnel_plot_pdf(
    meta_ciclo_or,
    "funnel_plot_ciclo_or.pdf",
    "Gráfico de embudo para Regularización Menstrual (OR)",
    "Odds Ratio",
    TRUE
  )
}

# Gráfico resumen combinado
if (exists("p") && requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot2::ggsave("meta_analisis_resumen.pdf", p, width = 10, height = 6)
  cat("Forest plot combinado generado: meta_analisis_resumen.pdf\n")
}