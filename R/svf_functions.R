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

#' Graficar la frontera del modelo SVF
#'
#' Esta función genera un gráfico que muestra los datos originales y la
#' frontera generada por el modelo SVF.
#'
#' @import ggplot2
#' @importFrom utils tail
#'
#' @param svf Objeto de la clase SVF entrenado.
#'
#' @return Un gráfico que muestra los datos originales y la frontera del modelo SVF.
#'
#' @export
plot_frontier <- function(svf) {

  inputs <- svf$inputs
  outputs <- svf$outputs
  data <- svf$data
  eps <- svf$eps

  x <- c(0)
  y <- c(0)

  if (length(inputs) != 1 || length(outputs) != 1) {
    stop("Esta funcion grafica solo para un input y un output.")
  }

  grid_points <- data.frame(x = seq(min(data[[inputs]]), max(data[[inputs]]), length.out = 100))

  grid_points$y <- sapply(grid_points$x, function(x) {
    dmu <- c(x)
    estimation <- get_estimation.SVF(svf, dmu)
    return(estimation)
  })

  step_points <- data.frame(x = numeric(0), y = numeric(0))

  step_points <- rbind(step_points, data.frame(x = min(data[[inputs]]), y = eps))
  step_points <- rbind(step_points, data.frame(x = min(data[[inputs]]), y = grid_points$y[1] + eps))

  for (i in 1:(nrow(grid_points) - 1)) {
    step_points <- rbind(step_points, data.frame(x = grid_points$x[i], y = grid_points$y[i] + eps))
    if (grid_points$y[i] != grid_points$y[i + 1]) {
      step_points <- rbind(step_points, data.frame(x = grid_points$x[i], y = grid_points$y[i + 1] + eps))
    }
    step_points <- rbind(step_points, data.frame(x = grid_points$x[i + 1], y = grid_points$y[i + 1] + eps))
  }

  max_x <- max(data[[inputs]]) + 0.5
  step_points <- rbind(step_points, data.frame(x = max_x, y = tail(step_points$y, 1)))

  p <- ggplot() +
    geom_point(data = data, aes_string(x = inputs, y = outputs), color = "blue", size = 2) +
    geom_line(data = step_points, aes(x = x, y = y), color = "red", linewidth = 1) +
    labs(x = inputs, y = outputs, title = "Frontera del modelo SVF") +
    theme_minimal() +
    scale_x_continuous(limits = c(min(data[[inputs]]), max_x))

  return(p)
}
