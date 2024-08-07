source("R/svf_solution.R")
source("R/grid.R")
source("R/svfgrid.R")
source("R/svf.R")
source("R/ssvf.R")

# Definir los datos
data <- data.frame(
  x1 = c(5, 2, 3, 4),
  y1 = c(5, 3, 2, 4)
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
svf_instance <- SSVF(method, inputs, outputs, data, C, eps, d)

# Entrenar el modelo
svf_instance <- train.SSVF(svf_instance)

# Exportar el modelo como cadena de texto
print(svf_instance$model)

# Resolver el modelo
solution <- solve(svf_instance)

# Imprimir las soluciones de w
print(solution$w)

# Imprimir las soluciones de xi
print(solution$xi)
