source("~/Documents/GitHub/svfpackage/R/svfgrid.R")

# Usar datos de prueba
data <- read.table("~/Documents/GitHub/svfpackage/data/datos.txt", header = TRUE, sep = ";")

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

# Realizar una búsqueda en el grid para una observación
dmu <- c(1, 3 )
position <- search_dmu.GRID(grid_obj, dmu)
print(paste("Posición en el grid: (", paste(position, collapse = ", "), ")", sep = ""))

# Ejemplo de búsqueda de celda contigua
cell <- c(1, 1)
contiguous_cells <- search_contiguous_cell(cell)
print(paste("Celdas contiguas: (", paste(contiguous_cells, collapse = ", "), ")", sep = ""))

# Ejemplo del calculo de phi para una celda dada
cell <- c(0, 0)
phi_list <- calculate_dmu_phi(grid_obj, cell)
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

print_data_grid <- function(data_grid) {
  print("Data grid completo:")
  temp_df <- data.frame(
    x1 = data_grid$x1,
    x2 = data_grid$x2,
    phi = sapply(data_grid$phi, function(phi) sprintf("[%s]", paste(phi[[1]], collapse = " "))),
    c_cells = sapply(data_grid$c_cells, function(cells) if (length(cells) > 0) paste(sapply(cells, function(cell) paste0("(", paste(cell, collapse = ", "), ")")), collapse = " ") else "[]")
  )
  print(temp_df)
}

print_data_grid(grid_obj$data_grid)
