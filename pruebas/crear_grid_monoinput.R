source("R/grid.R")
source("R/svfgrid.R")

# Definir los datos
data <- data.frame(
  x1 = c(1, 2, 3, 4),
  y1 = c(1, 3, 2, 4)
)

# Definir listas de inputs, outputs y la cantidad de particiones
inputs <- c("x1")
outputs <- c("y1")
d <- 2

# Crear la instancia de la clase SVFGrid y llamar al método create_grid
grid_obj <- SVFGrid(data, inputs, outputs, d)
grid_obj <- create_grid.SVFGrid(grid_obj)

# Mostrar knot_list
cat("knot_list:\n")
print(grid_obj$knot_list)

# Realizar una búsqueda en el grid para una observación
dmu <- c(4)
position <- search_dmu.GRID(grid_obj, dmu)
print(paste("Posición en el grid: (", paste(position - 1, collapse = ", "), ")", sep = ""))

# Ejemplo de búsqueda de celda contigua
cell <- c(3)
contiguous_cells <- search_contiguous_cell(cell)
print(paste("Celdas contiguas: (", paste(contiguous_cells, collapse = ", "), ")", sep = ""))

# Ejemplo del calculo de phi para una celda dada
cell <- c(1)
phi_list <- calculate_dmu_phi.SVFGrid(grid_obj, cell)
print("Vector phi para la celda dada:")
print(phi_list)

print_df_grid <- function(df_grid) {
  print("DF grid completo:")
  temp_df <- data.frame(
    id_cell = apply(df_grid$id_cells, 1, function(row) paste0("(", paste(row, collapse = ", "), ")")),
    value = apply(df_grid$values, 1, function(row) paste0("(", paste(sprintf("%.1f", row), collapse = ", "), ")")),
    phi = sapply(df_grid$phi, function(phi) sprintf("[%s]", paste(phi[[1]], collapse=" "))),
    c_cells = sapply(df_grid$c_cells, function(cells) if (length(cells) > 0) paste(sapply(cells, function(cell) paste0("(", paste(cell, collapse = ", "), ")")), collapse = " ") else "[]")
  )
  print(temp_df)
}

print_df_grid(grid_obj$df_grid)


print(grid_obj$data_grid)
