library(svfpackage)
library(Rcplex)

# Usar datos de prueba
data(datos, package = "svfpackage")

# Definición de inputs, outputs y otros parámetros
inputs <- c("x1", "x2")
outputs <- c("y1")
d <- 2
C <- 1
eps <- 0
method <- 'SSVF'

# Crear y mostrar el objeto SVF
ssvf <- SSVF(method, inputs, outputs, datos, C, eps, d)
print(ssvf)

trained_svf <- train.SSVF(ssvf)

# Resolver el modelo y mostrar resultados
solution_svf <- solve(trained_svf)
