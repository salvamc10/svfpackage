library(ggplot2)

#' Función que crea un objeto del tipo SVF en función del método que se selecciona
#'
#' Esta función es un constructor que permite crear diferentes tipos de objetos SVF.
#' Actualmente, solo soporta el tipo 'SSVF', pero puede ser extendida para incluir
#' otros tipos. Si el método especificado no es soportado, se producirá un error.
#'
#' @param method Método SVF que se quiere utilizar.
#' @param inputs Inputs a evaluar en el conjunto de datos.
#' @param outputs Outputs a evaluar en el conjunto de datos.
#' @param data Conjunto de datos a evaluar.
#' @param c Valores del hiperparámetro C del modelo.
#' @param eps Valores del hiperparámetro épsilon del modelo.
#' @param d Valor del hiperparámetro d del modelo.
#'
#' @example examples/example_create_svf.R
#'
#' @return Devuelve un objeto del método SVF seleccionado.
#'
#' @export
create_SVF <- function(method, inputs, outputs, data, c, eps, d) {
  if (method == "SSVF") {
    svf <- SSVF(method, inputs, outputs, data, c, eps, d)
  } else {
    stop("The method selected doesn't exist")
  }
  return(svf)
}

#' Graficar la frontera de producción
#'
#' Esta función toma un objeto SVF entrenado y grafica la frontera de producción
#' para un solo input (x1) y un solo output (y1).
#'
#' @param svf Objeto de clase SVF entrenado.
#' @param input_col Nombre de la columna del input (x1).
#' @param output_col Nombre de la columna del output (y1).
#' @param d Valor fijo para los otros inputs que no se están graficando.
#' @param n_puntos Número de puntos a utilizar para la estimación.
#'
#' @example examples/example_frontier.R
#'
#' @import ggplot2
#'
#' @export
plot_production_frontier <- function(svf, input_col, output_col, d, n_puntos = 100) {
  if (!(input_col %in% colnames(svf$data)) || !(output_col %in% colnames(svf$data))) {
    stop("El input o output especificado no existen en los datos.")
  }

  x_min <- min(svf$data[[input_col]])
  x_max <- max(svf$data[[input_col]])

  x_seq <- seq(x_min, x_max, length.out = n_puntos)

  estimations <- sapply(x_seq, function(x) {
    dmu <- c(x, d)
    est <- get_estimation.SVF(svf, dmu)
    return(est)
  })

  plot_data <- data.frame(x = x_seq, y = estimations)

  p <- ggplot(plot_data, aes(x = x, y = y)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_point(data = svf$data, aes_string(x = input_col, y = output_col), color = "red") +
    labs(title = "Frontera de Produccion", x = input_col, y = output_col) +
    theme_minimal()

  print(p)
}

# Declarar variables globales
utils::globalVariables(c("x", "y"))
