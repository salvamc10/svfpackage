library(Rcplex)

#' Crea un objeto SSVF
#'
#' @param method Método SVF a utilizar.
#' @param inputs Inputs a evaluar en el conjunto de datos.
#' @param outputs Outputs a evaluar en el conjunto de datos.
#' @param data Conjunto de datos a evaluar.
#' @param c Valores del hiperparámetro C del modelo.
#' @param eps Valores del hiperparámetro épsilon del modelo.
#' @param d Valor del hiperparámetro d del modelo.
#'
#' @return Un objeto de clase SSVF.
#'
#' @example examples/example_ssvf.R
#'
#' @export
SSVF <- function(method, inputs, outputs, data, c, eps, d) {
  svf <- list(method = method, inputs = inputs, outputs = outputs, data = data, c = c, eps = eps, d = d)
  class(svf) <- c("SSVF", "SVF")
  return(svf)
}

#' Entrena el modelo SSVF
#'
#' Esta función configura y resuelve el modelo SSVF utilizando programación lineal mixta.
#'
#' @param svf Objeto SSVF.
#'
#' @return El objeto SSVF con el modelo resuelto añadido.
#'
#' @example examples/example_train.R
#'
#' @import Rcplex
#'
#' @export
train.SSVF <- function(svf) {
  y_df <- svf$data[, svf$outputs, drop = FALSE]
  y <- as.matrix(y_df)

  n_out <- ncol(y_df)
  n_obs <- nrow(y_df)

  svf$grid <- create_grid.SVFGrid(SVFGrid(svf$data, svf$inputs, svf$outputs, svf$d))
  n_var <- length(svf$grid$data_grid$phi[[1]][[1]])

  total_variables <- n_out * n_var + n_out * n_obs

  cvec <- c(rep(1, n_out * n_var), rep(svf$c, n_out * n_obs))

  Amat <- matrix(0, nrow = 2 * n_out * n_obs, ncol = total_variables)
  bvec <- vector("numeric", length = 2 * n_out * n_obs)
  sense <- rep("L", 2 * n_out * n_obs)

  for (out in 1:n_out) {
    for (obs in 1:n_obs) {
      phi_vector <- svf$grid$data_grid$phi[[obs]][[out]]
      row_index1 <- (out - 1) * 2 * n_obs + (obs - 1) * 2 + 1
      row_index2 <- row_index1 + 1

      w_indices <- ((out - 1) * n_var + 1):((out - 1) * n_var + n_var)
      xi_index <- n_out * n_var + (out - 1) * n_obs + obs

      Amat[row_index1, w_indices] <- -phi_vector
      Amat[row_index2, w_indices] <- phi_vector
      Amat[row_index2, xi_index] <- -1

      bvec[row_index1] <- -y[obs, out]
      bvec[row_index2] <- y[obs, out] + svf$eps
    }
  }

  svf$model <- Rcplex(cvec = cvec, Amat = Amat, bvec = bvec, sense = sense, objsense = "min")

  return(svf)
}

#' Resolver el modelo SSVF
#'
#' Esta función extrae las soluciones del modelo SSVF después de que ha sido resuelto.
#'
#' @param svf Objeto SSVF con un modelo ya entrenado.
#'
#' @return Una lista conteniendo las soluciones para las variables 'w' y 'xi'.
#'
#' @example examples/example_solve.R
#'
#' @export
solve <- function(svf) {
  solution <- svf$model$xopt
  n_var <- length(svf$grid$data_grid$phi[[1]][[1]])
  n_out <- length(svf$outputs)
  n_obs <- nrow(svf$data)

  n_w_vars <- n_out * n_var
  n_xi_vars <- n_out * n_obs

  w_solution <- solution[1:n_w_vars]
  xi_solution <- solution[(n_w_vars + 1):(n_w_vars + n_xi_vars)]

  mat_w <- vector("list", n_out)
  for (out in seq_len(n_out)) {
    start_index <- (out - 1) * n_var + 1
    end_index <- out * n_var
    mat_w[[out]] <- round(w_solution[start_index:end_index], 6)
  }

  mat_xi <- vector("list", n_out)
  for (out in seq_len(n_out)) {
    start_index <- (out - 1) * n_obs + 1
    end_index <- out * n_obs
    mat_xi[[out]] <- round(xi_solution[start_index:end_index], 6)
  }

  cat("Solucion para w variables:\n")
  for (out in seq_len(n_out)) {
    cat(sprintf(" [%s]\n", paste(mat_w[[out]], collapse=", ")))
  }

  cat("Solucion para xi variables:\n")
  for (out in seq_len(n_out)) {
    cat(sprintf(" [%s]\n", paste(mat_xi[[out]], collapse=", ")))
  }

  return(list(w = mat_w, xi = mat_xi))

  Rcplex.close()
}
