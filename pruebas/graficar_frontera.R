source("R/svf_solution.R")
source("R/svf_functions.R")
source("R/grid.R")
source("R/svfgrid.R")
source("R/svf.R")
source("R/ssvf.R")

# Definir los datos
data <- X63_B1_40

# Definir listas de inputs, outputs y la cantidad de particiones
inputs <- c("x1")
outputs <- c("y")
d <- 39

# Parámetros iniciales del modelo SVF
C <- 1
eps <- 0.05
method <- 'SSVF'

# Instanciar y entrenar el modelo SVF
svf_instance <- create_SVF(method, inputs, outputs, data, C, eps, d)
svf_instance <- train.SSVF(svf_instance)

# Resolver el modelo y mostrar las soluciones
svf_solution <- solve(svf_instance)

# Imprimir las soluciones de w
print(svf_solution$w)
print(svf_solution$xi)

# Usar la función para graficar la frontera de producción
plot_frontier(svf_instance)
