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
  y <- as.matrix(y_df)  # Usar matriz facilita el manejo en R

  n_out <- ncol(y_df)
  n_obs <- nrow(y_df)

  svf$grid <- create_grid.SVFGrid(SVFGrid(svf$data, svf$inputs, svf$outputs, svf$d))
  n_var <- length(svf$grid$data_grid$phi[[1]][[1]])

  # Generar nombres para variables w y xi comenzando índices en 1
  w_names <- sapply(1:n_out, function(out) sapply(1:n_var, function(var) paste("w", out, var, sep = "_")))
  xi_names <- sapply(1:n_out, function(out) sapply(1:n_obs, function(obs) paste("xi", out, obs, sep = "_")))

  # Crear el modelo
  model <- MIPModel() %>%
    add_variable(w[out, var], out = 1:n_out, var = 1:n_var, type = "continuous", lb = 1, names = c(w_names)) %>%
    add_variable(xi[out, obs], out = 1:n_out, obs = 1:n_obs, type = "continuous", lb = 1, names = c(xi_names)) %>%
    set_objective(sum_expr(w[out, var], out = 1:n_out, var = 1:n_var) + sum_expr(xi[out, obs], out = 1:n_out, obs = 1:n_obs), "min")

  # Imprimir la función objetivo
  objective_expression <- sprintf("Minimize\n obj: %s",
                                  paste(c(paste0(w_names),
                                          paste0(xi_names)), collapse = " + "))
  cat(objective_expression, "\n")

  # Añadir restricciones utilizando bucles con índices ajustados para R
  for (obs in 1:n_obs) {
    for (out in 1:n_out) {
      model <- model %>%
        add_constraint(y[obs, out] - sum_expr(w[out, var] * svf$grid$data_grid$phi[[obs]][[out]][var], var = 1:n_var) <= 0,
                       name = paste("c1", obs - 1, out - 1, sep = "_")) %>%
        add_constraint(-y[obs, out] + sum_expr(w[out, var] * svf$grid$data_grid$phi[[obs]][[out]][var], var = 1:n_var) <= svf$eps + xi[out, obs],
                       name = paste("c2", obs - 1, out - 1, sep = "_"))
    }
  }

  # Resolver el modelo y manejar errores
  tryCatch({
    result <- solve_model(model, with_ROI(solver = "glpk"))
    svf$model <- result
  }, error = function(e) {
    cat("Error al resolver el modelo: ", e$message, "\n")
  })

  return(svf)
}

# Crear y mostrar el objeto SVF
data <- data.frame(x1 = c(1, 2, 3, 4), x2 = c(1, 3, 1, 2), y1 = c(1, 3, 2, 4))
inputs <- c("x1", "x2")
outputs <- c("y1")
d <- 2
C <- 1
eps <- 0
method <- 'SSVF'

svf <- SSVF(method, inputs, outputs, data, C, eps, d)
trained_svf <- train.SSVF(svf)
print(trained_svf$model$solution)
