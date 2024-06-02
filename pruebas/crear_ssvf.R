library(svfpackage)

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

# Entrenar el modelo
svf_instance <- train.SSVF(svf_instance)

# Exportar el modelo como cadena de texto
print(svf_instance$model)

# Resolver el modelo
solution <- solve(svf_instance)
print(solution)
