#' Transforma un valor basado en un nodo en la cuadrícula.
#'
#' Esta función toma un valor y lo compara con un nodo en la cuadrícula.
#' Dependiendo de la relación entre el valor y el nodo, se devuelve -1, 0 o 1.
#'
#' @param x_i El valor que se va a transformar.
#' @param t_k El nodo en la cuadrícula.
#'
#' @return Retorna -1 si x_i es menor que t_k, 0 si son iguales, y 1 si x_i es mayor que t_k.
#'
#' @example  examples/example_transform.R
#'
#' @export
transformation <- function(x_i, t_k) {
  z <- x_i - t_k
  result <- rep(NA, length(z))
  result[z < 0] <- -1
  result[z == 0] <- 0
  result[z > 0] <- 1
  return(result)
}

#' Función para inicializar el GRID.
#'
#' Esta función inicializa un GRID con los datos, variables de entrada,
#' variables de salida y la cantidad de dimensiones especificadas.
#'
#' @param data Los datos para construir el GRID
#' @param inputs Los nombres de las variables de entrada.
#' @param outputs Los nombres de las variables de salida.
#' @param d La cantidad de dimensiones del GRID.
#'
#' @return Retorna un objeto de clase "grid".
#'
#' @example examples/examples_grid.R
#'
#' @export
initialize_GRID <- function(data, inputs, outputs, d) {
  list(
    data = data,
    inputs = inputs,
    outputs = outputs,
    d = d,
    data_grid = NULL,
    knot_list = NULL
  )
  class(grid) <- "grid"
  return(grid)
}

#' Función para buscar el índice de la celda en la cuadrícula asociada a un punto dado.
#'
#' Esta función busca el índice de la celda en la cuadrícula asociada a un punto
#' dado utilizando el método de transformación definido en la función \code{transformation}.
#'
#' @param grid_instance La instancia de la cuadrícula.
#' @param dmu La lista de valores que representan un punto en la cuadrícula.
#'
#' @return Retorna una lista con los índices de celda correspondientes a las dimensiones de la cuadrícula.
#'
#' @example examples/example_search.R
#'
#' @export
search_dmu <- function(grid_instance, dmu) {
  cell <- vector("list", length(dmu))
  r <- grid_instance$knot_list
  for (l in seq_along(grid_instance$knot_list)) {
    x_i <- dmu[[l]]
    t_k <- unlist(r[[l]])
    trans <- sapply(t_k, function(tk) transformation(x_i, tk))
    m <- which(trans == 0)[1]
    if (is.null(m)) {
      m <- which.max(trans < 0)
      if (is.null(m)) {
        m <- length(trans)
      } else {
        m <- m - 1
      }
    } else {
      m <- m - 1
    }
    cell[[l]] <- m
  }
  return(cell)
}
