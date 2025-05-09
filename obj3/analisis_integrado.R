# Crear una función para generar un forest plot personalizado que muestre los resultados de todos los outcomes
library(gridExtra)
library(grid)

# Este código crea un resumen visual de todos los meta-análisis realizados
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

# Traducir nombres de resultados para el gráfico
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

# Traducir nombres de medidas para el gráfico
resultados_combinados$Medida_es <- ifelse(
  resultados_combinados$Medida == "MD", 
  "Diferencia de Medias", 
  "Riesgo Relativo"
)

# Mostrar tabla de resultados
print(resultados_combinados[, c("Outcome_es", "Estimador", "Significativo", "Pvalue", "Heterogeneidad")])

# Definir una paleta profesional para el gráfico combinado
# Usando paletas inspiradas en publicaciones científicas
# Paleta para significativo/no significativo
pal_significancia <- c("Sí" = "#3498DB", "No" = "#E74C3C")
# Paleta para heterogeneidad
pal_heterogeneidad <- c("Baja" = "#2ECC71", "Moderada" = "#F1C40F", "Alta" = "#9B59B6")

# Crear forest plot personalizado con ggplot2 y tema profesional en español
p <- ggplot(resultados_combinados, aes(y = reorder(Outcome_es, Efecto))) +
  # Línea vertical en el valor nulo (0 para MD, 1 para RR)
  geom_vline(aes(xintercept = ifelse(Medida == "RR", 1, 0)), 
             linetype = "dashed", color = "#7F8C8D", size = 0.5) +
  # Estimador puntual y intervalo de confianza
  geom_point(aes(x = Efecto, size = 1/I2, color = Significativo), alpha = 0.8) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper, color = Significativo), height = 0.2, size = 1) +
  # Agregar texto con el estimador
  geom_text(aes(x = ifelse(Medida == "RR", max(Upper)*1.2, max(Upper)*1.2), 
                label = Estimador), hjust = 0, size = 3.5, family = "sans") +
  # Asignar colores profesionales
  scale_color_manual(values = pal_significancia, name = "Estadísticamente\nsignificativo") +
  scale_size_continuous(name = "Precisión\n(1/Heterogeneidad)", range = c(3, 6)) +
  # Facet según tipo de medida (RR vs MD)
  facet_wrap(~Medida_es, scales = "free_x") +
  # Título y etiquetas en español
  labs(title = "Resumen de Meta-análisis: Metformina + Inositol vs. Metformina",
       subtitle = "MD = Diferencia de Medias, RR = Riesgo Relativo",
       x = "Tamaño del Efecto",
       y = "",
       caption = "Los efectos a la derecha del eje '0' (MD) o '1' (RR) favorecen la combinación de Metformina + Inositol") +
  # Tema personalizado profesional
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

# Mostrar gráfico combinado
print(p)

# Guardar gráfico combinado en PDF
ggsave("meta_analisis_resumen.pdf", p, width = 10, height = 6)
# Guardar en formato PNG alta resolución
ggsave("meta_analisis_resumen.png", p, width = 10, height = 6, dpi = 300)
# Guardar tabla de resultados
write.csv(resultados_combinados, "meta_analisis_resumen.csv", row.names = FALSE)