# Definir los datos
data <- data.frame(
  x1 = c(1, 2, 3, 4),
  x2 = c(1, 3, 2, 4),
  y1 = c(1, 3, 2, 4)
)

# Definir listas de inputs, outputs y la cantidad de particiones
inputs <- c("x1", "x2")
outputs <- c("y1")
d <- 2

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

# Probar la función get_estimation
estimations <- list(
  get_estimation.SVF(svf_instance, c(1, 2)),
  get_estimation.SVF(svf_instance, c(3, 4)),
  get_estimation.SVF(svf_instance, c(1, 7)),
  get_estimation.SVF(svf_instance, c(7, 1)),
  get_estimation.SVF(svf_instance, c(2, 4)),
  get_estimation.SVF(svf_instance, c(7, 1))
)

# Imprimir las estimaciones
for (i in seq_along(estimations)) {
  cat(sprintf("Estimación %d: %s\n", i + 1, estimations[[i]]))
}
