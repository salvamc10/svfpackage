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

# Imprimimos la knot_list
cat("knot_list:\n")
for (i in seq_along(grid_obj$knot_list)) {
  cat(paste0("Dimensión ", i, " (", names(grid_obj$knot_list)[i], "): "),
      paste(sprintf("%.1f", grid_obj$knot_list[[i]]), collapse = ", "), "\n")
}

# Imprimimos el knot_index
cat("\nknot_index:\n")
for (i in seq_along(grid_obj$knot_index)) {
  cat(paste0("Dimensión ", i, " (", names(grid_obj$knot_index)[i], "): "),
      paste(grid_obj$knot_index[[i]], collapse = ", "), "\n")
}

# Realizar una búsqueda en el grid para una observación
dmu <- c(3, 4)
position <- search_dmu.GRID(grid_obj, dmu)
print(paste("Posición en el grid: (", paste(position, collapse = ", "), ")", sep = ""))
