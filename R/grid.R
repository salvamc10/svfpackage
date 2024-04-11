#' Clase GRID para modelado SVF
#'
#' Esta clase representa un grid sobre el que se realiza el modelo SVF.
#' Un grid es una partición del espacio de los inputs dividido por celdas.
#'
#' @name GRID
#'
#' @field data DataFrame con el conjunto de datos sobre los que se construye el grid.
#' @field inputs Vector de nombres de los inputs.
#' @field outputs Vector de nombres de los outputs.
#' @field d Vector con el número de particiones en las que se divide el grid.
#' @field data_grid Grid de datos, inicialmente NULL.
#' @field knot_list Lista de nodos del grid, inicialmente NULL.
#'
#' @example examples/examples_grid.R
#'
#' @export
GRID <- function(data, inputs, outputs, d) {
  structure(list(
    data = data,
    inputs = inputs,
    outputs = outputs,
    d = d,
    data_grid = NULL,
    knot_list = NULL
  ), class = "GRID")
}

#' Buscar DMU en GRID
#'
#' Este método busca una DMU específica en el grid y devuelve la celda
#' en la que se encuentra dicha observación.
#'
#' @param grid Objeto de la clase GRID.
#' @param dmu Vector con la observación a buscar en el grid.
#'
#' @return Vector con la posición de la observación en el grid.
#'
#' @example examples/example_search.R
#'
#' @export
search_dmu.GRID <- function(grid, dmu) {
  cell <- vector("list", length(dmu))
  r <- grid$knot_list
  for (l in seq_along(grid$knot_list)) {
    x_i <- dmu[l]
    t_k <- unlist(r[[l]])
    trans <- sapply(t_k, function(tk) transformation(x_i, tk))
    m <- which(trans == 0)[1]
    if (is.na(m)) {
      m <- which.max(trans < 0)
      if (is.na(m)) {
        m <- length(trans)
      }
    }
    cell[[l]] <- m - 1
  }
  return(unlist(cell))
}

#' Transformación de valores en el GRID
#'
#' Esta función evalúa si el valor de una observación es mayor, igual,
#' o menor que el valor de un nodo en el grid. Devuelve 1 si es mayor,
#' 0 si es igual, y -1 si es menor.
#'
#' @param x_i Valor de la observación a evaluar.
#' @param t_k Valor del nodo con el que se compara.
#'
#' @return Resultado de la comparación: 1, 0, -1.
#'
#' @example examples/example_transform.R
#'
#' @export
transformation <- function(x_i, t_k) {
  z <- x_i - t_k
  if (z < 0) {
    return(-1)
  } else if (z == 0) {
    return(0)
  } else {
    return(1)
  }
}
