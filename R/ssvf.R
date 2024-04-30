source("~/Documents/GitHub/svfpackage/R/svf.R")
source("~/Documents/GitHub/svfpackage/R/svfgrid.R")

library(ROI)
library(ROI.plugin.glpk)
library(ompr)
library(ompr.roi)
library(dplyr)

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

train.SSVF <- function(svf) {
  y_df <- svf$data[, svf$outputs, drop = FALSE]
  y <- as.matrix(y_df)

  n_out <- ncol(y_df)
  n_obs <- nrow(y_df)

  svf$grid <- create_grid.SVFGrid(SVFGrid(svf$data, svf$inputs, svf$outputs, svf$d))

  n_var <- length(svf$grid$data_grid$phi[[1]][[1]])

  # Inicializar el modelo de optimización aquí
  model <- MIPModel() %>%
    add_variable(w[out, var], out = 1:n_out, var = 1:n_var, type = "continuous", lb = 0, ub = 1e+33) %>%
    add_variable(xi[out, obs], out = 1:n_out, obs = 1:n_obs, type = "continuous", lb = 0, ub = 1e+33) %>%
    set_objective(sum_expr(w[out, var] * 1, out = 1:n_out, var = 1:n_var) +
                    sum_expr(xi[out, obs] * svf$c, out = 1:n_out, obs = 1:n_obs), "min")

  for (out in 1:n_out) {
    for (obs in 1:n_obs) {
      model <- model %>%
        add_constraint(sum_expr(-w[out, var] * svf$grid$data_grid$phi[[obs]][[out]][var], var = 1:n_var) <= -y[obs, out]) %>%
        add_constraint(sum_expr(w[out, var] * svf$grid$data_grid$phi[[obs]][[out]][var], var = 1:n_var) - xi[out, obs] <= y[obs, out] + svf$eps)
    }
  }

  svf$model <- model

  return(svf)
}

solve.SSVF <- function(svf) {
  result <- solve_model(svf$model, with_ROI(solver = "glpk"))

  w_solution <- result$solution[grep("^w", names(result$solution))]
  xi_solution <- result$solution[grep("^xi", names(result$solution))]

  n_out <- length(svf$outputs)
  n_w_dim <- length(w_solution) / n_out
  n_obs <- nrow(svf$data)

  mat_w <- vector("list", n_out)
  for (out in seq_len(n_out)) {
    start_index <- (out - 1) * n_w_dim + 1
    end_index <- out * n_w_dim
    mat_w[[out]] <- round(w_solution[start_index:end_index], 6)
  }

  mat_xi <- vector("list", n_out)
  for (out in seq_len(n_out)) {
    start_index <- (out - 1) * n_obs + 1
    end_index <- out * n_obs
    mat_xi[[out]] <- round(xi_solution[start_index:end_index], 6)
  }

  cat("Solution for w variables:\n")
  for (out in seq_len(n_out)) {
    cat(sprintf(" ["), paste(mat_w[[out]], collapse=", "), "]\n")
  }

  cat("Solution for xi variables:\n")
  for (out in seq_len(n_out)) {
    cat(sprintf(" ["), paste(mat_xi[[out]], collapse=", "), "]\n")
  }

  return(list(w = mat_w, xi = mat_xi))
}

# Crear y mostrar el objeto SVF
data <- data.frame(x1 = c(1, 2), x2 = c(1, 1), y1 = c(1, 4))
inputs <- c("x1", "x2")
outputs <- c("y1")
d <- 2
C <- 1
eps <- 0
method <- 'SSVF'

# Crear y preparar el modelo
svf <- SSVF(method, inputs, outputs, data, C, eps, d)
trained_svf <- train.SSVF(svf)

# Resolver el modelo y mostrar resultados
solution_svf <- solve.SSVF(trained_svf)
