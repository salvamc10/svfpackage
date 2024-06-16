#' Clase GRID para modelo SVF
#'
#' Esta clase representa un grid sobre el que se realiza el modelo SVF.
#' Un grid es una partición del espacio de los inputs dividido por celdas.
#'
#' @name GRID
#'
#' @param data DataFrame con el conjunto de datos sobre los que se construye el grid.
#' @param inputs Listado de inputs.
#' @param outputs Listado de outputs.
#' @param d número de particiones en las que se divide el grid.
#' @field data_grid Grid de datos, inicialmente NULL.
#' @field knot_list Lista de nodos del grid, inicialmente NULL.
#'
#' @example examples/example_grid.R
#'
#' @export
GRID <- function(data, inputs, outputs, d) {
  structure(list( data = data, inputs = inputs, outputs = outputs, d = d, data_grid = NULL, knot_list = NULL), class = "GRID")
}

#' Función que devuelve la celda en la que se encuentra una observación en el grid
#'
#' Este método busca una DMU específica en el grid y devuelve la celda
#' en la que se encuentra dicha observación.
#'
#' @param grid Objeto de la clase GRID.
#' @param dmu Observación a buscar en el grid.
#'
#' @return Vector con la posición de la observación en el grid.
#'
#' @example examples/example_search.R
#'
#' @export
search_dmu.GRID <- function(grid, dmu) {
  r <- lapply(grid$knot_list, unlist)
  cell <- numeric(length(dmu))
  for (l in seq_along(dmu)) {
    encontrado <- FALSE
    for (m in seq_along(r[[l]])) {
      trans <- transformation(dmu[l], r[[l]][m])
      if (trans < 0) {
        cell[l] <- m - 1
        encontrado <- TRUE
        break
      } else if (trans == 0) {
        cell[l] <- m
        encontrado <- TRUE
        break
      }
    }
    if (!encontrado) {
      cell[l] <- length(r[[l]])
    }
  }

  return(cell)
}

#' Transformación de valores en el GRID
#'
#' Esta función evalúa si el valor de una observación es mayor, igual,
#' o menor que el valor de un nodo en el grid. Devuelve 1 si es mayor,
#' 0 si es igual, y -1 si es menor.
#'
#' @param x_i Valor de la celda a evaluar.
#' @param t_k Valor del nodo con el que se quiere comparar.
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
