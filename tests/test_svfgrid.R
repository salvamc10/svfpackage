source("~/Documents/GitHub/svfpackage/R/svfgrid.R")

# Crear un conjunto de datos de ejemplo
data <- data.frame(x1 = c(1, 2, 3, 4), x2 = c(1, 3, 1, 2), y1 = c(2, 4, 3, 5))

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

# Ejemplo de búsqueda de celda contigua
cell <- c(2, 2)
contiguous_cells <- search_contiguous_cell(cell)
print(paste("Celdas contiguas: (", paste(contiguous_cells, collapse = ", "), ")", sep = ""))

# Ejemplo del calculo de phi para una celda dada
cell <- c(0, 0)
phi_list <- calculate_dmu_phi(grid_obj, cell)
print("Vector phi para la celda dada:")
print(phi_list)

print_df_grid <- function(df_grid) {
  print("df_grid completo:")
  # Crear un data.frame temporal para imprimir
  temp_df <- data.frame(
    id_cell = apply(df_grid$id_cells, 1, function(row) paste0("(", paste(row, collapse = ", "), ")")),
    value = apply(df_grid$values, 1, function(row) paste0("(", paste(sprintf("%.1f", row), collapse = ", "), ")")),
    phi = sapply(df_grid$phi, function(phi) paste(phi[[1]], collapse = " ")),
    c_cells = sapply(df_grid$c_cells, function(cells) if (length(cells) > 0) paste(sapply(cells, function(cell) paste0("(", paste(cell, collapse = ", "), ")")), collapse = " ") else "[]")
  )

  print(temp_df)
}

print_df_grid(grid_obj$df_grid)

