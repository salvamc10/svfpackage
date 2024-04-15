# Cargar las bibliotecas necesarias
library(ROI)
library(ROI.plugin.glpk)

# Definir las coeficientes de la función objetivo
obj <- c(3, 2)

# Matriz de restricciones
mat <- matrix(c(2, 1, 2, 3, 3, 1), nrow = 3, byrow = TRUE)

# Lado derecho de las restricciones
rhs <- c(18, 42, 24)

# Tipo de restricciones
dir <- c("<=", "<=", "<=")

# Crear el modelo de optimizacióna
lp <- ROI::OP(objective = obj,
              constraints = L_constraint(L = mat, dir = dir, rhs = rhs),
              types = c("C", "C"),
              maximum = TRUE)

# Resolver el modelo usando el solver GLPK
solution <- ROI::ROI_solve(lp, solver = "glpk")

# Imprimir la estructura del objeto de solución
print(solution$solution)
print(solution$objval)

