# Instalación y carga de paquetes necesarios
if (!require("meta")) install.packages("meta")
if (!require("metafor")) install.packages("metafor")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("readr")) install.packages("readr")

# Configurar para no usar X11
options(bitmapType = "cairo")

# Cargar librerías
library(meta)
library(metafor)
library(dplyr)
library(ggplot2)
library(readr)

# Función para leer los datos
leer_datos <- function(archivo) {
  datos <- read.csv(archivo)
  return(datos)
}

# Función para realizar metaanálisis para un parámetro específico
analizar_parametro <- function(archivo) {
  # Leer datos
  datos <- leer_datos(archivo)
  
  # Extraer nombre del parámetro del nombre del archivo
  nombre_parametro <- gsub("\\.csv$", "", basename(archivo))
  nombre_parametro <- gsub("-", " ", nombre_parametro)
  
  # Realizar metaanálisis
  resultado_meta <- metacont(
    n.e = n_intervencion,
    mean.e = media_intervencion,
    sd.e = de_intervencion,
    n.c = n_control,
    mean.c = media_control,
    sd.c = de_control,
    studlab = nombre,
    data = datos,
    sm = "SMD",  # Diferencia de medias estandarizada
    fixed = FALSE,
    random = TRUE,
    method.tau = "REML",
    hakn = TRUE
  )
  
  # Guardar forest plot directamente a un archivo PDF con colores mejorados
  pdf(paste0("forest_plot_", nombre_parametro, ".pdf"), width = 10, height = 8)
  forest(
    resultado_meta,
    sortvar = TE,
    prediction = TRUE,
    label.left = "Favorece control",
    label.right = "Favorece inositol",
    leftlabs = c("Estudio", "n_i", "n_c", "DME", "IC 95%", "Peso"),
    print.tau2 = TRUE,
    print.I2 = TRUE,
    col.diamond = "darkblue",
    col.diamond.lines = "blue",
    col.predict = "red",
    col.predict.lines = "darkred",
    col.square = "darkgreen",
    col.study = "black",
    main = paste("Gráfico de bosque:", nombre_parametro),
    text.predict = "Intervalo de predicción",
    text.random = "Modelo de efectos aleatorios",
    text.w.random = "Peso relativo"
  )
  dev.off()
  
  # Realizar análisis por subgrupos según el tipo de intervención
  datos$tipo_intervencion <- case_when(
    grepl("myo-Inositol \\+ 300 mg D-chiro-Inositol", datos$intervencion) ~ "MI+DCI (300mg)",
    grepl("myo-Inositol \\+ 27.6 mg D-chiro-Inositol", datos$intervencion) ~ "MI+DCI (27.6mg)",
    grepl("4 g myo-Inositol c/24h", datos$intervencion) ~ "MI solo",
    grepl("formulacion", datos$intervencion) ~ paste0("Ratio MI:DCI ", gsub(".*formulacion (.*) myo-Inositol.*", "\\1", datos$intervencion)),
    TRUE ~ "Otras formulaciones"
  )
  
  # Solo realizar análisis por subgrupos si hay suficientes estudios
  if(length(unique(datos$tipo_intervencion)) > 1) {
    subgrupos_meta <- metacont(
      n.e = n_intervencion,
      mean.e = media_intervencion,
      sd.e = de_intervencion,
      n.c = n_control,
      mean.c = media_control,
      sd.c = de_control,
      studlab = nombre,
      data = datos,
      byvar = tipo_intervencion,
      sm = "SMD",
      fixed = FALSE,
      random = TRUE,
      method.tau = "REML",
      hakn = TRUE
    )
    
    # Guardar forest plot de subgrupos con colores mejorados
    pdf(paste0("subgrupos_", nombre_parametro, ".pdf"), width = 10, height = 10)
    forest(
      subgrupos_meta,
      sortvar = TE,
      prediction = FALSE,
      label.left = "Favorece control",
      label.right = "Favorece inositol",
      leftlabs = c("Intervención/Estudio", "n_i", "n_c", "DME", "IC 95%", "Peso"),
      print.tau2 = TRUE,
      print.I2 = TRUE,
      col.diamond = "darkblue",
      col.diamond.lines = "blue",
      col.square = "darkgreen",
      col.by = "darkred",
      col.study = "black",
      main = paste("Comparación de Intervenciones:", nombre_parametro),
      text.random = "Modelo de efectos aleatorios",
      text.w.random = "Peso relativo"
    )
    dev.off()
  }
  
  # Retornar los resultados para análisis posterior
  return(list(meta = resultado_meta, datos = datos))
}

# Análisis completo de todos los parámetros
analisis_completo <- function() {
  # Lista de archivos CSV
  archivos_csv <- c(
    "volumen-ovarico.csv",
    "homa-ir.csv",
    "glucosa-en-ayunas.csv",
    "ciclos-menstruales.csv",
    "insulina-en-ayunas.csv",
    "indice-masa-corporal.csv"
  )
  
  # Lista para almacenar resultados
  resultados <- list()
  
  # Analizar cada parámetro
  for (archivo in archivos_csv) {
    nombre_param <- gsub("\\.csv$", "", basename(archivo))
    resultados[[nombre_param]] <- analizar_parametro(archivo)
  }
  
  # Crear tabla resumen de resultados
  tabla_resumen <- data.frame(
    parametro = character(),
    n_estudios = integer(),
    efecto = numeric(),
    ic_inferior = numeric(),
    ic_superior = numeric(),
    p_valor = numeric(),
    i2 = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (nombre_param in names(resultados)) {
    meta_resultado <- resultados[[nombre_param]]$meta
    nombre_formato <- gsub("-", " ", nombre_param)
    
    tabla_resumen <- rbind(
      tabla_resumen,
      data.frame(
        parametro = nombre_formato,
        n_estudios = meta_resultado$k,
        efecto = meta_resultado$TE.random,
        ic_inferior = meta_resultado$lower.random,
        ic_upper = meta_resultado$upper.random,
        p_valor = meta_resultado$pval.random,
        i2 = meta_resultado$I2,
        stringsAsFactors = FALSE
      )
    )
  }
  
  # Ordenar por tamaño del efecto
  tabla_resumen <- tabla_resumen[order(abs(tabla_resumen$efecto), decreasing = TRUE), ]
  
  # Guardar tabla resumen como CSV
  write.csv(tabla_resumen, "resumen_metanalisis.csv", row.names = FALSE)
  
  # Crear un gráfico resumen con ggplot2 y guardarlo directamente
  p <- ggplot(tabla_resumen, 
              aes(x = reorder(parametro, efecto), y = efecto, 
                  ymin = ic_inferior, ymax = ic_superior)) +
    geom_pointrange(color = "darkblue", size = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    coord_flip() +
    labs(
      title = "Efecto del Inositol en Parámetros de SOPQ",
      subtitle = "Diferencia de Medias Estandarizada (DME) con IC 95%",
      x = "Parámetro Clínico",
      y = "Tamaño del Efecto (DME)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text.y = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95")
    )
  
  ggsave("grafico_resumen.png", p, width = 10, height = 6, dpi = 300)
  
  return(resultados)
}

# Análisis específico para HOMA-IR (el parámetro con más estudios)
analizar_homa_ir_detalle <- function() {
  # Leer datos de HOMA-IR
  datos_homa <- leer_datos("homa-ir.csv")
  
  # Clasificar las intervenciones y extraer ratio MI:DCI
  datos_homa <- datos_homa %>%
    mutate(
      tipo_intervencion = case_when(
        grepl("myo-Inositol \\+ 300 mg D-chiro-Inositol", intervencion) ~ "MI+DCI (300mg)",
        grepl("myo-Inositol \\+ 27.6 mg D-chiro-Inositol", intervencion) ~ "MI+DCI (27.6mg)",
        grepl("4 g myo-Inositol c/24h", intervencion) ~ "MI solo",
        grepl("formulacion 0:1", intervencion) ~ "DCI solo",
        grepl("formulacion", intervencion) ~ paste0("Ratio ", gsub(".*formulacion (.*) myo-Inositol.*", "\\1", intervencion)),
        TRUE ~ "Otras"
      ),
      ratio_num = case_when(
        grepl("formulacion 0:1", intervencion) ~ 0,
        grepl("formulacion 1:3.5", intervencion) ~ 1/3.5,
        grepl("formulacion 2.5:1", intervencion) ~ 2.5,
        grepl("formulacion 5:1", intervencion) ~ 5,
        grepl("formulacion 20:1", intervencion) ~ 20,
        grepl("formulacion 40:1", intervencion) ~ 40,
        grepl("formulacion 80:1", intervencion) ~ 80,
        grepl("myo-Inositol \\+ 300 mg D-chiro-Inositol", intervencion) ~ 1100/300,
        grepl("myo-Inositol \\+ 27.6 mg D-chiro-Inositol", intervencion) ~ 1100/27.6,
        grepl("4 g myo-Inositol c/24h", intervencion) ~ Inf,
        TRUE ~ NA_real_
      )
    )
  
  # Metaanálisis para HOMA-IR por tipo de intervención
  homa_meta <- metacont(
    n.e = n_intervencion,
    mean.e = media_intervencion,
    sd.e = de_intervencion,
    n.c = n_control,
    mean.c = media_control,
    sd.c = de_control,
    studlab = nombre,
    data = datos_homa,
    sm = "SMD",
    byvar = tipo_intervencion,
    fixed = FALSE,
    random = TRUE,
    method.tau = "REML",
    hakn = TRUE
  )
  
  # Guardar forest plot para HOMA-IR con colores mejorados
  pdf("homa_ir_detalle.pdf", width = 12, height = 10)
  forest(
    homa_meta,
    sortvar = TE,
    prediction = TRUE,
    label.left = "Favorece control",
    label.right = "Favorece inositol",
    leftlabs = c("Intervención/Estudio", "n_i", "n_c", "DME", "IC 95%", "Peso"),
    print.tau2 = TRUE,
    print.I2 = TRUE,
    col.diamond = "darkblue",
    col.diamond.lines = "blue",
    col.predict = "red",
    col.predict.lines = "darkred",
    col.square = "darkgreen",
    col.by = "purple",
    col.study = "black",
    main = "Efecto del Inositol en HOMA-IR por tipo de intervención",
    text.predict = "Intervalo de predicción",
    text.random = "Modelo de efectos aleatorios",
    text.w.random = "Peso relativo"
  )
  dev.off()
  
  # Calcular tamaño del efecto para cada estudio
  datos_homa$TE <- (datos_homa$media_intervencion - datos_homa$media_control) / 
    sqrt((datos_homa$de_intervencion^2 + datos_homa$de_control^2) / 2)
  datos_homa$seTE <- sqrt(1/datos_homa$n_intervencion + 1/datos_homa$n_control)
  
  # Crear gráfico de la relación entre ratio MI:DCI y efecto en HOMA-IR
  datos_ratio <- datos_homa %>% filter(!is.na(ratio_num) & ratio_num < Inf)
  
  if(nrow(datos_ratio) > 0) {
    p <- ggplot(datos_ratio, aes(x = ratio_num, y = TE, size = 1/seTE)) +
      geom_point(alpha = 0.7, color = "darkblue") +
      geom_smooth(method = "loess", se = TRUE, color = "red", fill = "pink") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
      scale_x_log10() +
      labs(
        title = "Relación entre Ratio MI:DCI y Efecto en HOMA-IR",
        x = "Ratio MI:DCI (escala logarítmica)",
        y = "Tamaño del Efecto (DME)",
        size = "Precisión"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = "bottom"
      )
    
    ggsave("homa_ir_ratio.png", p, width = 10, height = 6, dpi = 300)
  }
  
  # Metaregresión para ratio
  if(nrow(datos_ratio) > 3) {
    meta_regresion <- rma(yi = TE, sei = seTE, data = datos_ratio, mods = ~ ratio_num)
    
    # Guardar resultados de metaregresión
    sink("meta_regresion_homa_ir.txt")
    print(meta_regresion)
    sink()
  }
  
  return(homa_meta)
}

# Ejecutar el análisis completo
resultados_completos <- analisis_completo()

# Realizar análisis detallado de HOMA-IR
homa_ir_detalle <- analizar_homa_ir_detalle()

# Mostrar resultados
print("Análisis completado. Los resultados se han guardado como archivos PDF y CSV.")
cat("\nResumen de hallazgos por parámetro:\n")
results_summary <- read.csv("resumen_metanalisis.csv")
print(results_summary)