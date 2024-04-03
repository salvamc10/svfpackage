source("~/svfpackage/R/grid.R")

# Usar el dataset de ejemplo
data <- read.table("~/svfpackage/data/datos.txt", header = TRUE, sep = ";")

# Definir listas de inputs, outputs y la cantidad de particiones
inputs <- c("x1", "x2")
outputs <- c("y1")
d <- 2

# Inicializar la cuadrícula
grid_instance <- initialize_GRID(data, inputs, outputs, d)

# Evaluar una observación menor que el valor del nodo del grid
result <- transformation(2, 5)
cat("Resultado de la transformación:", result, "\n")
# Output: -1

# Evaluar una observación igual al valor del nodo del grid
result <- transformation(3, 3)
cat("Resultado de la transformación:", result, "\n")
# Output: 0

# Evaluar una observación mayor que el valor del nodo del grid
result <- transformation(7, 4)
cat("Resultado de la transformación:", result, "\n")
# Output: 1

# Ejemplo de uso de la función search_dmu
grid_instance$knot_list <- list(list(1, 2.5, 4), list(1, 2, 3))
dmu <- list(2.5, 2)
position <- search_dmu(grid_instance, dmu)
position_string <- paste(position, collapse = ", ")
cat("Posición en el grid:", position_string, "\n")
