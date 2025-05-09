# Cargar datos de Glucosa en Ayunas
glucosa_data <- read_csv("objetivo3/glucosa-ayunas.csv")

# Ver estructura del dataset
str(glucosa_data)
summary(glucosa_data)

# Realizar meta-análisis para Glucosa en Ayunas
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

# Mostrar resultados del meta-análisis
print(meta_glucosa)

# Volver a generar el gráfico para mostrar en pantalla
forest(meta_glucosa,
       sortvar = TE,
       label.left = "Favorece combinación",
       label.right = "Favorece monoterapia",
       prediction = TRUE,
       xlim = c(-5, 5),
       xlab = "Diferencia de medias en Glucosa en Ayunas (mg/dL)",
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
print(paste("I²:", round(meta_glucosa$I2*100, 1), "%"))
print(paste("Estadístico Q:", round(meta_glucosa$Q, 2), "p-valor:", round(meta_glucosa$pval.Q, 4)))

# Análisis de sensibilidad
metainf(meta_glucosa)

# Configurar parámetros para exportación del funnel plot en PDF
pdf("funnel_plot_glucosa.pdf", width = 8, height = 7)

# Evaluar sesgo de publicación con tema profesional
funnel(meta_glucosa, 
       xlab = "Diferencia de medias",
       main = "Gráfico de embudo para Glucosa en Ayunas",
       studlab = TRUE,
       col.fixed = "#3498DB",
       col.random = "#E74C3C",
       col.predict = "#2ECC71")

# Cerrar el dispositivo PDF
dev.off()

# Volver a generar el gráfico para mostrar en pantalla
funnel(meta_glucosa, 
       xlab = "Diferencia de medias",
       main = "Gráfico de embudo para Glucosa en Ayunas",
       studlab = TRUE,
       col.fixed = "#3498DB",
       col.random = "#E74C3C",
       col.predict = "#2ECC71")