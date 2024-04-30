source("~/Documents/GitHub/svfpackage/R/svf_functions.R")

# Usar datos de prueba
data <- read.table("~/Documents/GitHub/svfpackage/data/datos.txt", header = TRUE, sep = ";")

# Definición de inputs, outputs y otros parámetros
inputs <- c("x1", "x2")
outputs <- c("y1")
d <- 2
C <- 1
eps <- 0
method <- 'SSVF'

# Crear y mostrar el objeto SVF
svf <- create_SVF(method, inputs, outputs, data, C, eps, d)
print(svf)
