# Crear un conjunto de datos de ejemplo
data <- data.frame(x1 = c(1, 2, 3, 4), x2 = c(5, 6, 7, 8), y1 = c(9, 1, 2, 3))

# Definir listas de inputs, outputs y la cantidad de particiones
inputs <- c("x1", "x2")
outputs <- c("y1")
d <- 2

# Crear la instancia de la clase SVFGrid y llamar al método create_grid
grid_obj <- SVFGrid(data, inputs, outputs, d)
grid_obj <- create_grid.SVFGrid(grid_obj)

print_df_grid <- function(df_grid) {
  print("DF grid completo:")
  temp_df <- data.frame(
    id_cell = apply(df_grid$id_cells, 1, function(row)
      paste0("(", paste(row, collapse = ", "), ")")),
    value = apply(df_grid$values, 1, function(row)
      paste0("(", paste(sprintf("%.1f", row), collapse = ", "), ")")),
    phi = sapply(df_grid$phi, function(phi) sprintf("[%s]", paste(phi[[1]], collapse=" "))),
    c_cells = sapply(df_grid$c_cells, function(cells) if (length(cells) > 0)
      paste(sapply(cells, function(cell)
      paste0("(", paste(cell, collapse = ", "), ")")), collapse = " ") else "[]")
  )
  print(temp_df)
}

print_df_grid(grid_obj$df_grid)
