# Cargar datos de HOMA-IR
homa_data <- read_csv("objetivo3/homa-ir.csv")

# Ver estructura del dataset
str(homa_data)
summary(homa_data)

# Realizar meta-análisis para HOMA-IR
meta_homa <- metacont(
  n.e = Intervention_N,
  mean.e = Intervention_Mean,
  sd.e = Intervention_SD,
  n.c = Control_N,
  mean.c = Control_Mean,
  sd.c = Control_SD,
  studlab = Study,
  data = homa_data,
  sm = "MD",  # Diferencia de medias
  method.tau = "REML",
  hakn = TRUE,
  title = "Meta-análisis de HOMA-IR: Metformina + Inositol vs. Metformina"
)

# Volver a generar el gráfico para mostrar en pantalla
forest(meta_homa,
       sortvar = TE,
       label.left = "Favorece combinación",
       label.right = "Favorece monoterapia",
       prediction = TRUE,
       xlim = c(-1, 1),
       xlab = "Diferencia de medias en HOMA-IR",
       smlab = "Diferencia de medias [IC 95%]",
       weight.study = "tamaño", 
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
       text.w.fixed = "Peso (efectos fijos)",
       text.w.random = "Peso (efectos aleatorios)",
       fontsize = 10)

# Evaluar heterogeneidad
print(paste("I²:", round(meta_homa$I2*100, 1), "%"))
print(paste("Estadístico Q:", round(meta_homa$Q, 2), "p-valor:", round(meta_homa$pval.Q, 4)))

# Análisis de sensibilidad
metainf(meta_homa)

# Configurar parámetros para exportación del funnel plot en PDF
pdf("funnel_plot_homa.pdf", width = 8, height = 7)

# Evaluar sesgo de publicación con tema profesional
funnel(meta_homa, 
       xlab = "Diferencia de medias",
       main = "Gráfico de embudo para HOMA-IR",
       studlab = TRUE,
       col.fixed = "#3498DB",
       col.random = "#E74C3C",
       col.predict = "#2ECC71")

# Cerrar el dispositivo PDF
dev.off()

# Volver a generar el gráfico para mostrar en pantalla
funnel(meta_homa, 
       xlab = "Diferencia de medias",
       main = "Gráfico de embudo para HOMA-IR",
       studlab = TRUE,
       col.fixed = "#3498DB",
       col.random = "#E74C3C",
       col.predict = "#2ECC71")