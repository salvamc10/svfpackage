source("~/Documents/GitHub/svfpackage/R/grid.R")

#' Constructor para la clase SVFGrid
#'
#' Esta función crea una instancia de la clase SVFGrid, la cual es una
#' extensión de la clase GRID con funcionalidades adicionales específicas
#' para el manejo de grids en el contexto de análisis SVF.
#'
#' @param data DataFrame con los datos sobre los cuales se construirá el grid.
#' @param inputs Vector con los nombres de las columnas que serán tratadas como inputs.
#' @param outputs Vector con los nombres de las columnas que serán tratadas como outputs.
#' @param d Entero o vector de enteros que especifica el número de divisiones por dimensión en el grid.
#' @return Un objeto de clase SVFGrid.
#' @export
SVFGrid <- function(data, inputs, outputs, d) {
  grid <- list(data = data, inputs = inputs, outputs = outputs, d = d, df_grid = data.frame(), data_grid = data.frame())
  class(grid) <- c("SVFGrid", "GRID")
  return(grid)
}

#' Crear grid para SVFGrid
#'
#' Este método crea un grid basado en los datos y el parámetro d proporcionados
#' al constructor. Este grid es una representación de los datos en un espacio
#' dividido en celdas definidas por el parámetro d.
#'
#' @param grid Objeto SVFGrid sobre el cual operar.
#' @return El objeto SVFGrid con el grid creado.
#' @export
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

#' Calcular phi para una celda específica en SVFGrid
#'
#' Esta función calcula y retorna el valor de phi para una celda específica del
#' grid, basado en los datos del grid y la posición de la celda.
#'
#' @param grid Objeto SVFGrid sobre el cual operar.
#' @param cell Vector que especifica la posición de la celda en el grid.
#' @return Una lista que contiene los valores de phi para la celda especificada.
#' @export
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

#' Calcular el dataframe de grid para SVFGrid
#'
#' Este método calcula y añade información adicional al dataframe de grid
#' asociado a un objeto SVFGrid. Esta información incluye los valores de phi
#' para cada celda del grid y las celdas contiguas a cada celda.
#'
#' @param grid Objeto SVFGrid sobre el cual operar.
#' @return El objeto SVFGrid con el dataframe de grid actualizado.
#' @export
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

#' Buscar celdas contiguas en SVFGrid
#'
#' Esta función identifica y retorna las celdas contiguas a una celda especificada
#' en el grid. Las celdas contiguas son aquellas que comparten al menos un borde
#' o punto con la celda especificada.
#'
#' @param cell Vector que especifica la posición de la celda en el grid.
#' @return Una lista de celdas contiguas a la especificada.
#' @export
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
