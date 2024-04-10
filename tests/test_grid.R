source("~/Documents/GitHub/svfpackage/R/grid.R")

# Crear una instancia de GRID y usar sus métodos
data <- data.frame(x1 = c(1, 2, 3, 4), x2 = c(1, 3, 1, 2), y1 = c(2, 4, 3, 5))
inputs <- c("x1", "x2")
outputs <- c("y1")
d <- 2

grid_instance <- GRID(data, inputs, outputs, d)

# Evaluar una observación igual al valor del nodo del grid
result <- transformation(3, 2)
cat("Resultado de la transformación:", result, "\n")

grid_instance$knot_list <- list(list(1, 2.5, 4), list(1, 2, 3))
dmu <- c(2.5, 1)
position <- search_dmu.GRID(grid_instance, dmu)

print(paste("Posición en el grid: (", paste(position, collapse = ", "), ")", sep = ""))

