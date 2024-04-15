# Cargar el paquete lpSolve
library(lpSolve)

# Definir los coeficientes de la función objetivo
objective.coef <- c(3, 2)

# Definir las restricciones del modelo
# Matriz con los coeficientes de las restricciones
const.mat <- matrix(c(2, 1,   # Coeficientes de la primera restricción
                      2, 3,   # Coeficientes de la segunda restricción
                      3, 1),  # Coeficientes de la tercera restricción
                    nrow = 3, byrow = TRUE)

# Lado derecho de las restricciones
const.rhs <- c(18, 42, 24)

# Direcciones de las restricciones (<=)
const.dir <- c("<=", "<=", "<=")

# Definir los límites de las variables (no negativas)
var.limits <- c(0, Inf)

# Crear el modelo en lpSolve
lp.model <- lp(direction = "max",
               objective.in = objective.coef,
               const.mat = const.mat,
               const.dir = const.dir,
               const.rhs = const.rhs,
               all.int = FALSE,
               all.bin = FALSE)

# Verificar el resultado
if(lp.model$status == 0) {
  print("Solución óptima encontrada:")
  print(paste("Valor objetivo:", lp.model$objval))
  print(paste("Valores de las variables:", toString(lp.model$solution)))
} else {
  print("No se encontró solución.")
}
