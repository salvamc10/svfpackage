source("~/svfpackage/R/grid.R")

# Crear el dataset de ejemplo
data <- data.frame(x1 = c(1, 2, 3, 4), x2 = c(1, 3, 1, 2), y1 = c(2, 4, 3, 5))

# Definir listas de inputs, outputs y la cantidad de particiones
inputs <- c("x1", "x2")
outputs <- c("y1")
d <- 2

# Inicializar la cuadrícula
grid_instance <- initialize_GRID(data, inputs, outputs, d)

# Ejemplo de uso de la función search_dmu
grid_instance$knot_list <- list(list(1, 2, 3), list(4, 5, 6))
dmu <- list(2, 6)
position <- search_dmu(grid_instance, dmu)
position_string <- paste(position, collapse = ", ")
cat("Posición en el grid:", position_string, "\n")
