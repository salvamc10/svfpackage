# Crear un conjunto de datos de ejemplo
data <- data.frame(x1 = c(1, 2, 3, 4), x2 = c(5, 6, 7, 8), y1 = c(9, 1, 2, 3))

# Método print para la clase GRID
print.GRID <- function(x) {
  cat("Datos del GRID:\n")
  cat("----------------------------------\n")
  cat(sprintf("Inputs: %s\n", paste(x$inputs, collapse = ", ")))
  cat(sprintf("Outputs: %s\n", paste(x$outputs, collapse = ", ")))
  cat(sprintf("Dimensiones de Data: %d rows, %d columns\n", nrow(x$data), ncol(x$data)))
  cat(sprintf("  d (Número de particiones): %d\n", x$d))

  cat("Data Preview (solo las primeras columnas):\n")
  if (nrow(x$data) > 0) {
    print(head(x$data))
  } else {
    cat("No hay datos.\n")
  }

  cat("----------------------------------\n")
  invisible(x)
}

# Definir listas de inputs, outputs y la cantidad de particiones
inputs <- c("x1", "x2")
outputs <- c("y1")
d <- 2

# Crear la instancia de la clase SVFGrid y llamar al método create_grid
grid_obj <- SVFGrid(data, inputs, outputs, d)
print(grid_obj)
