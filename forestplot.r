# Cargar las librerías necesarias
library(metafor)
library(ggplot2)
library(dplyr)
library(forestplot)
library(RColorBrewer)

# Crear el dataframe con los nuevos datos proporcionados
datos_meta <- data.frame(
  estudio = c("Shokrpour, 2019", "Troisi, 2019", "Nordio, 2019", 
              "Nordio, 2021", "Genazzani, 2019"),
  n_total = c(53, 30, 16, 40, 48),
  g = c(-0.261803, 0.093378, -2.099209, -4.800405, -1.602928),
  se = c(0.275990, 0.365362, 0.638264, 0.634987, 0.333559),
  ci_lower = c(-0.802744, -0.622730, -3.350207, -6.044980, -2.256703),
  ci_upper = c(0.279137, 0.809487, -0.848211, -3.555829, -0.949153),
  interpretacion = c("Pequeño", "Sin efecto", "Grande", "Grande", "Grande")
)

# Realizar un modelo de efectos aleatorios
modelo_meta <- rma(yi = g, sei = se, data = datos_meta)

# Extraer el resultado global
efecto_global <- data.frame(
  estudio = "EFECTO GLOBAL",
  g = modelo_meta$beta,
  se = modelo_meta$se,
  ci_lower = modelo_meta$ci.lb,
  ci_upper = modelo_meta$ci.ub,
  n_total = sum(datos_meta$n_total),
  interpretacion = ifelse(abs(modelo_meta$beta) < 0.2, "Sin efecto",
                          ifelse(abs(modelo_meta$beta) < 0.5, "Pequeño",
                                 ifelse(abs(modelo_meta$beta) < 0.8, "Mediano", "Grande")))
)

# Combinar los datos originales con el efecto global
datos_completos <- rbind(datos_meta, efecto_global)

# Ordenar los estudios para que el efecto global aparezca al final
datos_plot <- datos_completos %>%
  mutate(orden = ifelse(estudio == "EFECTO GLOBAL", 1, 0)) %>%
  arrange(orden, g) %>%
  mutate(estudio = factor(estudio, levels = estudio))

# Paleta de colores optimizada para impresión en papel
paleta_elegante <- c("#006D2C", "#31A354", "#74C476", "#C7E9C0")
names(paleta_elegante) <- c("Grande", "Mediano", "Pequeño", "Sin efecto")

# Crear el Forest Plot con ggplot2 usando colores elegantes y letra más grande
p <- ggplot(datos_plot, aes(y = estudio, x = g)) +
  # Fondo temático elegante
  theme_minimal(base_size = 16) +
  # Añadir una línea vertical en cero
  geom_vline(xintercept = 0, linetype = "longdash", color = "gray50", linewidth = 0.8) +
  # Líneas de referencia para interpretar el tamaño del efecto (más sutiles)
  geom_vline(xintercept = c(-0.8, -0.5, -0.2, 0.2, 0.5, 0.8), 
             linetype = "dotted", color = "gray80", alpha = 0.4) +
  # Añadir los intervalos de confianza
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper, color = interpretacion), 
                 height = 0.3, linewidth = 1.0, alpha = 0.9) +
  # Añadir los puntos para el tamaño del efecto
  geom_point(aes(size = n_total, color = interpretacion), shape = 16, alpha = 0.9) +
  # Resaltar el efecto global
  geom_point(data = filter(datos_plot, estudio == "EFECTO GLOBAL"), 
             aes(x = g), shape = 18, size = 8, color = "#006D2C") +
  # Aplicar la paleta de colores optimizada para impresión
  scale_color_manual(values = paleta_elegante) +
  # Etiquetas y tema con el nuevo título
  labs(
    title = "Eficacia de inositol en reducción de índice de resistencia a insulina en SOP",
    subtitle = "Tamaños de efecto g de Hedges (valores negativos favorecen al tratamiento con inositol)",
    x = "Tamaño del efecto (g de Hedges)",
    y = NULL,
    caption = "Nota: El tamaño de los cuadrados es proporcional al tamaño de la muestra"
  ) +
  theme_classic(base_size = 16) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray30"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(face = "bold", size = 14),
    legend.text = element_text(size = 12),
    legend.background = element_rect(fill = "white", color = NA),
    axis.text.y = element_text(size = 14, face = ifelse(datos_plot$estudio == "EFECTO GLOBAL", "bold", "plain")),
    axis.text.x = element_text(size = 13),
    axis.title.x = element_text(size = 15, face = "bold", margin = margin(t = 15)),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(25, 25, 25, 25),
    plot.caption = element_text(size = 12, face = "italic")
  ) +
  geom_text(aes(x = ci_upper + 0.3, 
                label = sprintf("g = %.2f [%.2f, %.2f]", g, ci_lower, ci_upper),
                fontface = ifelse(estudio == "EFECTO GLOBAL", "bold", "plain")),
            hjust = 0, size = 4.5) +
  guides(
    size = guide_legend(title = "Tamaño de muestra", override.aes = list(color = "#006D2C")),
    color = guide_legend(title = "Interpretación del efecto", override.aes = list(size = 5))
  ) +
  scale_x_continuous(limits = c(min(datos_plot$ci_lower) - 0.5, 
                                max(datos_plot$ci_upper) + 3),
                     breaks = seq(-6, 2, 1))

# Generar el gráfico de alta calidad
print(p)

# Guardar el gráfico en alta resolución
ggsave("forest_plot_inositol_SOP.pdf", p, width = 14, height = 8, dpi = 300)
ggsave("forest_plot_inositol_SOP.png", p, width = 14, height = 8, dpi = 300)

# Mostrar los resultados estadísticos del meta-análisis
print(modelo_meta)

# Crear una tabla de resultados del meta-análisis con formato más profesional
resultados_meta <- data.frame(
  Estadístico = c("Efecto global (g)", "Error estándar", "IC 95% inferior", "IC 95% superior", 
                  "Z", "p-valor", "I² (heterogeneidad)", "Q (heterogeneidad)", "p (heterogeneidad)"),
  Valor = c(sprintf("%.3f", modelo_meta$beta), 
            sprintf("%.3f", modelo_meta$se),
            sprintf("%.3f", modelo_meta$ci.lb),
            sprintf("%.3f", modelo_meta$ci.ub),
            sprintf("%.3f", modelo_meta$zval),
            sprintf("%.3f", modelo_meta$pval),
            sprintf("%.1f%%", modelo_meta$I2),
            sprintf("%.3f", modelo_meta$QE),
            sprintf("%.3f", modelo_meta$QEp))
)

# Imprimir la tabla de resultados
print(resultados_meta)
