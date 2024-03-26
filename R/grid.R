setClass(
  "GRID",
  representation(
    data = "data.frame",
    inputs = "character",
    outputs = "character",
    d = "numeric",
    data_grid = "NULL",
    knot_list = "list"
  )
)

#' Construye un objeto de clase GRID.
#'
#' Esta función crea un objeto de clase GRID que encapsula un conjunto de datos, las variables de entrada y salida,
#' así como la configuración del grid utilizado para dividir el espacio de entrada.
#'
#' @importFrom methods new
#'
#' @param data Un data.frame que contiene los datos.
#' @param inputs Un vector de caracteres que especifica las variables de entrada.
#' @param outputs Un vector de caracteres que especifica las variables de salida.
#' @param d Un vector numérico que especifica el número de particiones en cada dimensión de las variables de entrada.
#'
#' @return Un objeto de clase GRID.
#'
#' @example examples/examples_grid.R
#'
#' @export
GRID <- function(data, inputs, outputs, d) {
  grid <- new("GRID",
              data = data,
              inputs = inputs,
              outputs = outputs,
              d = d,
              data_grid = NULL,
              knot_list = list())
  return(grid)
}

#' Función para realizar una transformación
#'
#' Esta función evalúa si el valor de una observación es mayor, igual o menor que el valor de un nodo del grid.
#'
#' @param x_i Valor de la observación a evaluar.
#' @param t_k Valor del nodo con el que se quiere comparar.
#'
#' @return -1 si x_i < t_k, 0 si x_i == t_k, 1 si x_i > t_k.
#'
#' @example  examples/example_transform.R
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

#' Función para buscar una observación en el grid
#'
#' Esta función devuelve la celda en la que se encuentra una observación en el grid.
#'
#' @param grid Un objeto de clase GRID que define la estructura del grid.
#' @param dmu Un vector que representa la observación a buscar en el grid.
#' @return Un vector con la posición de la observación en el grid.
#'
#' @example examples/example_search.R
#'
#' @export
search_dmu <- function(grid, dmu) {
  cell <- list()
  r <- t(grid@knot_list)
  for (l in seq_along(grid@knot_list)) {
    for (m in seq_along(grid@knot_list[[l]])) {
      trans <- transformation(dmu[l], r[m, l])
      if (trans < 0) {
        cell <- c(cell, m - 1)
        break
      } else if (trans == 0) {
        cell <- c(cell, m)
        break
      } else if (trans > 0 && m == length(grid@knot_list[[l]])) {
        cell <- c(cell, m)
        break
      }
    }
  }
  return(cell)
}
