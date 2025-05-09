# Este script modifica el gráfico de resumen para incluir las intervenciones
# Solo ejecutar si ya se ha generado el análisis integrado

if (exists("resultados_combinados") && exists("p")) {
  # Crear nuevo gráfico con título actualizado
  p_actualizado <- p +
    labs(
      title = "Resumen de Meta-análisis: Metformina + Inositol vs. Metformina",
      subtitle = "Comparación de diferentes dosis combinadas vs. monoterapia",
      caption = "Los efectos a la derecha del eje '0' (MD) o '1' (RR) favorecen la combinación de Metformina + Inositol.\nSe eliminó la columna de pesos para mejorar la visualización."
    )
  
  # Guardar nuevo gráfico
  ggplot2::ggsave("meta_analisis_resumen_actualizado.pdf", p_actualizado, width = 10, height = 6)
  ggplot2::ggsave("meta_analisis_resumen_actualizado.png", p_actualizado, width = 10, height = 6, dpi = 300)
  
  # Mostrar gráfico actualizado
  print(p_actualizado)
  
  cat("Gráfico resumen actualizado generado\n")
}