source("~/Documents/GitHub/svfpackage/R/ssvf.R")

library(Rcplex)

# Usar datos de prueba
data <- data.frame(x1 = c(1, 2, 3, 4), x2 = c(5, 6, 7, 8), y1 = c(9, 1, 2, 3))

# Definición de inputs, outputs y otros parámetros
inputs <- c("x1", "x2")
outputs <- c("y1")
d <- 2
C <- 1
eps <- 0
method <- 'SSVF'

# Crear y mostrar el objeto SVF
ssvf <- SSVF(method, inputs, outputs, data, C, eps, d)

trained_svf <- train.SSVF(ssvf)

print(trained_svf)
