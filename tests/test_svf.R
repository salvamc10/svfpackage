library(svfpackage)

# Usar datos de prueba
data(datos, package = "svfpackage")

# Definir listas de inputs, outputs y la cantidad de particiones
inputs <- c("x1", "x2")
outputs <- c("y1")
C <- 1
eps <- 0
d <- 2
method <- 'SVF'

# Crear una instancia de la clase SVF
svf <- SVF(method, inputs, outputs, datos, C, eps, d)

# Imprimir la instancia
print(svf)
