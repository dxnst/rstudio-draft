# Instalación de paquetes necesarios
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

# Definir una paleta de colores profesional
paleta_profesional <- c("#2C3E50", "#E74C3C", "#3498DB", "#2ECC71", "#F1C40F", "#9B59B6")

# Configuración para forest plots más profesionales
par(mar = c(4, 4, 2, 2), bg = "white")

