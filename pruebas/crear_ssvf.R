source("~/Documents/GitHub/svfpackage/R/svf_solution.R")
source("~/Documents/GitHub/svfpackage/R/svf_functions.R")
source("~/Documents/GitHub/svfpackage/R/grid.R")
source("~/Documents/GitHub/svfpackage/R/svfgrid.R")
source("~/Documents/GitHub/svfpackage/R/svf.R")
source("~/Documents/GitHub/svfpackage/R/ssvf.R")

# Definir los datos
data <- data.frame(
  x1 = c(1, 2, 3, 4),
  y1 = c(1, 3, 2, 4)
)

# Definir listas de inputs, outputs y la cantidad de particiones
inputs <- c("x1")
outputs <- c("y1")
d <- 2

# Parámetros iniciales del modelo SVF
C <- 1
eps <- 0
method <- 'SSVF'

# Instanciar y entrenar el modelo SVF
svf_instance <- create_SVF(method, inputs, outputs, data, C, eps, d)

# Entrenar el modelo
svf_instance <- train.SSVF(svf_instance)

# Exportar el modelo como cadena de texto
print(svf_instance$model)

# Resolver el modelo
solution <- solve(svf_instance)
print(solution)
