source("~/Documents/GitHub/svfpackage/R/grid.R")

# Crear el dataset de ejemplo
data <- data.frame(x1 = c(1, 2, 3, 4), x2 = c(5, 6, 7, 8), y1 = c(9, 1, 2, 3))

# Definir listas de inputs, outputs y la cantidad de particiones
inputs <- c("x1", "x2")
outputs <- c("y1")
d <- 2

grid_instance <- GRID(data, inputs, outputs, d)

grid_instance$knot_list <- list(list(1, 2.5, 4), list(1, 2, 3))
dmu <- c(2, 3)
position <- search_dmu.GRID(grid_instance, dmu)

print(paste("Posición en el grid: (", paste(position, collapse = ", "), ")", sep = ""))
