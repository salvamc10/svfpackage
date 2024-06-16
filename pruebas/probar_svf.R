source("R/svf.R")

# Usar datos de prueba
data <- read.table("data/datos2.txt", header = TRUE, sep = ";")

# Definir listas de inputs, outputs y la cantidad de particiones
inputs <- c("x1", "x2")
outputs <- c("y1", "y2")
C <- 1
eps <- 0
d <- 2
method <- 'SVF'

# Crear una instancia de la clase SVF
svf <- SVF(method, inputs, outputs, data, C, eps, d)

# Imprimir la instancia
print(svf)
