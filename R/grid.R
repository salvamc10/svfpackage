# Definir el constructor de la clase GRID
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

# Método search_dmu para objetos de clase GRID en R
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

# Método de transformación
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
