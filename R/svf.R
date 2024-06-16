# Definir la clase SVF
#' Crea una nueva instancia de la clase SVF
#'
#' Esta función inicializa un objeto SVF con los parámetros proporcionados,
#' encapsulando un modelo SVF con las especificaciones de entrada y salida,
#' junto con los hiperparámetros del modelo.
#'
#' @param method Método SVF que se quiere utilizar.
#' @param inputs Inputs a evaluar en el conjunto de datos.
#' @param outputs Outputs a evaluar en el conjunto de datos.
#' @param data Conjunto de datos a evaluar.
#' @param C Valores del hiperparámetro C del modelo.
#' @param eps Valores del hiperparámetro épsilon del modelo.
#' @param d Valor del hiperparámetro d del modelo.
#'
#' @return Un objeto de la clase 'SVF'.
#'
#' @example examples/example_svf.R
#'
#' @export
SVF <- function(method, inputs, outputs, data, C, eps, d) {
  structure(list(
    method = method, inputs = inputs, outputs = outputs, data = data,
    C = C, eps = eps, d = d, grid = NULL, model = NULL, model_d = NULL, solution = NULL, name = NULL
  ), class = "SVF")
}

#' Estimacion de una DMU
#'
#' Esta función calcula la estimación de los outputs para una DMU específica usando el modelo SVF.
#'
#' @param svf Objeto de clase SVF.
#' @param dmu Observación sobre la que se quiere estimar su valor (vector numérico).
#'
#' @return Vector numérico con las estimaciones de cada output.
#'
#' @example examples/example_get_estimation.R
#'
#' @export
get_estimation.SVF <- function(svf, dmu) {
  if (length(dmu) != length(svf$inputs)) {
    stop("El numero de inputs de la DMU no coincide con el numero de inputs del problema.")
  }

  # Convertir dmu a numérico si no lo es
  dmu <- as.numeric(dmu)

  dmu_cell <- search_dmu.GRID(svf$grid, dmu)

  id_cells <- svf$grid$df_grid$id_cells
  cell_index <- which(apply(id_cells, 1, function(row) all(row == dmu_cell)))

  if (length(cell_index) == 0) {
    stop("No se encontro la celda de la DMU en el grid.")
  }

  phi <- unlist(svf$grid$df_grid$phi[[cell_index]])

  solution <- svf$model$xopt
  n_var <- length(svf$grid$data_grid$phi[[1]][[1]])
  n_out <- length(svf$outputs)
  n_w_vars <- n_out * n_var
  w_solution <- solution[1:n_w_vars]

  mat_w <- vector("list", n_out)
  for (out in seq_len(n_out)) {
    start_index <- (out - 1) * n_var + 1
    end_index <- out * n_var
    mat_w[[out]] <- round(w_solution[start_index:end_index], 6)
  }

  prediction_list <- numeric(n_out)
  for (out in seq_along(svf$outputs)) {
    w <- unlist(mat_w[[out]])
    prediction_list[out] <- round(sum(w * phi), 3)
  }

  return(prediction_list)
}
