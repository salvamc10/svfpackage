source("~/Documents/GitHub/svfpackage/R/svf.R")
source("~/Documents/GitHub/svfpackage/R/svfgrid.R")

library(ROI)
library(ROI.plugin.glpk)

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

  # Definición de variables w y xi
  name_w <- expand.grid(out = 1:n_out, var = 1:n_var)
  w <- setNames(rep(1, nrow(name_w)), apply(name_w, 1, paste, collapse=","))
  name_xi <- expand.grid(out = 1:n_out, obs = 1:n_obs)
  xi <- setNames(rep(svf$c, nrow(name_xi)), apply(name_xi, 1, paste, collapse=","))

  for (obs in seq_len(n_obs)) {
    for (out in seq_len(n_out)) {

    }
  }
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
train.SSVF(svf)
