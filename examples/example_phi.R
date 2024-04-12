source("~/Documents/GitHub/svfpackage/R/svfgrid.R")

# Crear un conjunto de datos de ejemplo
data <- data.frame(x1 = c(1, 2, 3, 4), x2 = c(5, 6, 7, 8), y1 = c(9, 1, 2, 3))

# Definir listas de inputs, outputs y la cantidad de particiones
inputs <- c("x1", "x2")
outputs <- c("y1")
d <- 2

# Crear la instancia de la clase SVFGrid y llamar al método create_grid
grid_obj <- SVFGrid(data, inputs, outputs, d)
grid_obj <- create_grid.SVFGrid(grid_obj)

# Ejemplo del calculo de phi para una celda dada
cell <- c(0, 0)
phi_list <- calculate_dmu_phi(grid_obj, cell)
print("Vector phi para la celda dada:")
print(phi_list)
