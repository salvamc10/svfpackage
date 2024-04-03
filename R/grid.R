#' Transforma un valor basado en un nodo en la cuadrícula.
#'
#' Esta función toma un valor y lo compara con un nodo en la cuadrícula.
#' Dependiendo de la relación entre el valor y el nodo, se devuelve -1, 0 o 1.
#'
#' @param x_i El valor que se va a transformar.
#' @param t_k El nodo en la cuadrícula.
#' @return Retorna -1 si x_i es menor que t_k, 0 si son iguales, y 1 si x_i es mayor que t_k.
#' @examples
#' transformation(2, 5)
#' transformation(3, 3)
#' transformation(7, 4)
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

#' Función para inicializar la cuadrícula.
#'
#' Esta función inicializa una cuadrícula con los datos, variables de entrada,
#' variables de salida y la cantidad de dimensiones especificadas.
#'
#' @param data Los datos para construir la cuadrícula.
#' @param inputs Los nombres de las variables de entrada.
#' @param outputs Los nombres de las variables de salida.
#' @param d La cantidad de dimensiones de la cuadrícula.
#' @return Retorna una lista que representa la cuadrícula inicializada.
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
}

#' Función para buscar el índice de la celda en la cuadrícula asociada a un punto dado.
#'
#' Esta función busca el índice de la celda en la cuadrícula asociada a un punto
#' dado utilizando el método de transformación definido en la función \code{transformation}.
#'
#' @param grid_instance La instancia de la cuadrícula.
#' @param dmu La lista de valores que representan un punto en la cuadrícula.
#' @return Retorna una lista con los índices de celda correspondientes a las dimensiones de la cuadrícula.
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
