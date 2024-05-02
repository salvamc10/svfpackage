source("~/Documents/GitHub/svfpackage/R/svf_functions.R")

library(Rcplex)

# Usar datos de prueba
data <- read.table("~/Documents/GitHub/svfpackage/data/datos3.txt", header = TRUE, sep = ";")

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

trained_svf <- train.SSVF(svf)

# Resolver el modelo y mostrar resultados
solution_svf <- solve(trained_svf)
