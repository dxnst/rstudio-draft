# Cargar datos de Regularización del Ciclo Menstrual
ciclo_data <- read_csv("objetivo3/regularizacion-ciclo-menstrual.csv")

# Ver estructura del dataset
str(ciclo_data)
summary(ciclo_data)

# Realizar meta-análisis para regularización del ciclo menstrual (datos binarios)
meta_ciclo_rr <- metabin(
  event.e = Intervention_Events,
  n.e = Intervention_Total,
  event.c = Control_Events,
  n.c = Control_Total,
  studlab = Study,
  data = ciclo_data,
  sm = "RR",  # Riesgo relativo
  method.tau = "REML",
  hakn = TRUE,
  title = "Meta-análisis de Regularización del Ciclo Menstrual: Metformina + Inositol vs. Metformina"
)

# Mostrar resultados del meta-análisis (RR)
print(meta_ciclo_rr)

# Volver a generar el gráfico para mostrar en pantalla
forest(meta_ciclo_rr,
       sortvar = TE,
       label.left = "Favorece monoterapia",
       label.right = "Favorece combinación",
       prediction = TRUE,
       xlim = c(0.5, 3),
       xlab = "Riesgo Relativo de Regularización Menstrual",
       smlab = "Riesgo Relativo [IC 95%]",
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

# Repetir análisis con Odds Ratio
meta_ciclo_or <- metabin(
  event.e = Intervention_Events,
  n.e = Intervention_Total,
  event.c = Control_Events,
  n.c = Control_Total,
  studlab = Study,
  data = ciclo_data,
  sm = "OR",  # Odds ratio
  method.tau = "REML",
  hakn = TRUE,
  title = "Meta-análisis de Regularización del Ciclo Menstrual: Metformina + Inositol vs. Metformina"
)

# Mostrar resultados del meta-análisis (OR)
print(meta_ciclo_or)

# Configurar parámetros para exportación en PDF - Odds Ratio
pdf("forest_plot_ciclo_or.pdf", width = 10, height = 7)

# Generar forest plot para odds ratio con tema profesional en español
forest(meta_ciclo_or,
       sortvar = TE,
       label.left = "Favorece monoterapia",
       label.right = "Favorece combinación",
       prediction = TRUE,
       xlim = c(0.5, 10),
       xlab = "Odds Ratio de Regularización Menstrual",
       smlab = "Odds Ratio [IC 95%]",
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

# Cerrar el dispositivo PDF
dev.off()

# Volver a generar el gráfico para mostrar en pantalla
forest(meta_ciclo_or,
       sortvar = TE,
       label.left = "Favorece monoterapia",
       label.right = "Favorece combinación",
       prediction = TRUE,
       xlim = c(0.5, 10),
       xlab = "Odds Ratio de Regularización Menstrual",
       smlab = "Odds Ratio [IC 95%]",
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
print(paste("I² (RR):", round(meta_ciclo_rr$I2*100, 1), "%"))
print(paste("Estadístico Q (RR):", round(meta_ciclo_rr$Q, 2), "p-valor:", round(meta_ciclo_rr$pval.Q, 4)))

# Análisis de sensibilidad
metainf(meta_ciclo_rr)

# Configurar parámetros para exportación del funnel plot en PDF
pdf("funnel_plot_ciclo.pdf", width = 8, height = 7)

# Evaluar sesgo de publicación con tema profesional
funnel(meta_ciclo_rr, 
       xlab = "Riesgo Relativo",
       main = "Gráfico de embudo para Regularización Menstrual",
       studlab = TRUE,
       col.fixed = "#3498DB",
       col.random = "#E74C3C",
       col.predict = "#2ECC71")

# Cerrar el dispositivo PDF
dev.off()

# Volver a generar el gráfico para mostrar en pantalla
funnel(meta_ciclo_rr, 
       xlab = "Riesgo Relativo",
       main = "Gráfico de embudo para Regularización Menstrual",
       studlab = TRUE,
       col.fixed = "#3498DB",
       col.random = "#E74C3C",
       col.predict = "#2ECC71")