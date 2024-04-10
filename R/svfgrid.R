source("~/Documents/GitHub/svfpackage/R/grid.R")

# Constructor SVFGrid
SVFGrid <- function(data, inputs, outputs, d) {
  grid <- list(data = data, inputs = inputs, outputs = outputs, d = d, df_grid = data.frame(), data_grid = data.frame())
  class(grid) <- c("SVFGrid", "GRID")
  return(grid)
}

# Método create_grid para SVFGrid
create_grid.SVFGrid <- function(grid) {
  x <- grid$data[, grid$inputs]
  n_dim <- ncol(x)
  knot_list <- list()
  knot_index <- list()

  for (col in seq_len(n_dim)) {
    knot_min <- min(x[, col], na.rm = TRUE)
    knot_max <- max(x[, col], na.rm = TRUE)
    amplitud <- (knot_max - knot_min) / grid$d
    knots <- seq(knot_min, knot_max, length.out = grid$d + 1)
    knot_list[[col]] <- knots
    knot_index[[col]] <- 0:grid$d
  }

  grid$knot_list <- knot_list
  id_cells <- expand.grid(knot_index)
  id_cells <- id_cells[, ncol(id_cells):1]
  values <- expand.grid(rev(knot_list))
  values <- values[, ncol(values):1]
  grid$df_grid <- list(id_cells = id_cells, values = values, phi = vector("list", nrow(values)))

  grid <- calculate_df_grid(grid)

  return(grid)
}

# Función para calcular phi para una celda específica
calculate_dmu_phi <- function(grid, cell) {
  df_grid <- grid$df_grid
  phi <- numeric(length = nrow(df_grid$id_cells))
  for (i in seq_len(nrow(df_grid$id_cells))) {
    value <- 1
    for (j in seq_along(cell)) {
      if (cell[j] < df_grid$id_cells[i, j]) {
        value <- 0
        break
      }
    }
    phi[i] <- value
  }
  phi_list <- list(phi)

  return(phi_list)
}

calculate_df_grid <- function(grid) {
  n <- nrow(grid$df_grid$id_cells)
  phi_list <- vector("list", n)
  c_cells_list <- vector("list", n)
  for (i in 1:n) {
    cell <- grid$df_grid$values[i, , drop = FALSE]
    p <- search_dmu.GRID(grid, cell)
    phi <- calculate_dmu_phi(grid, p)[[1]]
    c_cells <- search_contiguous_cell(p)
    phi_list[[i]] <- list(phi)
    c_cells_list[[i]] <- c_cells
  }
  grid$df_grid$phi <- phi_list
  grid$df_grid$c_cells <- c_cells_list

  return(grid)
}

# Busca celdas contiguas
search_contiguous_cell <- function(cell) {
  con_c_list <- list()
  for (dim in seq_along(cell)) {
    value <- cell[dim] - 1
    if (value >= 0) {
      con_cell <- cell
      con_cell[dim] <- value
      con_c_list <- c(con_c_list, list(con_cell))
    }
  }

  return(con_c_list)
}

# Crear un conjunto de datos de ejemplo
data <- data.frame(x1 = c(1, 2, 3, 4), x2 = c(1, 3, 1, 2), y1 = c(2, 4, 3, 5))

# Definir listas de inputs, outputs y la cantidad de particiones
inputs <- c("x1", "x2")
outputs <- c("y1")
d <- 2

# Crear la instancia de la clase SVFGrid y llamar al método create_grid
grid_obj <- SVFGrid(data, inputs, outputs, d)
grid_obj <- create_grid.SVFGrid(grid_obj)
