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
d <- 3

# Parámetros iniciales del modelo SVF
C <- 1
eps <- 0
method <- 'SSVF'

# Instanciar y entrenar el modelo SVF
svf_instance <- create_SVF(method, inputs, outputs, data, C, eps, d)
svf_instance <- train.SSVF(svf_instance)

# Resolver el modelo y mostrar las soluciones
svf_solution <- solve(svf_instance)

# Imprimir las soluciones de w
print(svf_solution$w)
print(svf_solution$xi)

# Probar la función get_estimation
estimations <- list(
  get_estimation.SVF(svf_instance, c(1)),
  get_estimation.SVF(svf_instance, c(3)),
  get_estimation.SVF(svf_instance, c(1)),
  get_estimation.SVF(svf_instance, c(7)),
  get_estimation.SVF(svf_instance, c(2)),
  get_estimation.SVF(svf_instance, c(3))
)

# Imprimir las estimaciones
for (i in seq_along(estimations)) {
  cat(sprintf("Estimación %d: %s\n", i + 1, estimations[[i]]))
}

# Usar la función para graficar la frontera de producción
plot_svf_frontier(svf_instance)
