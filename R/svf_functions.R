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
#' @param svf_instance Objeto de la clase SVF entrenado.
#'
#' @return Un gráfico que muestra los datos originales y la frontera del modelo SVF.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line labs theme
#' @importFrom graphics legend lines title
#' @importFrom utils tail
#'
#' @export
plot_svf_frontier <- function(svf_instance) {

  x_min <- min(svf_instance$data[[svf_instance$inputs[1]]])
  x_max <- max(svf_instance$data[[svf_instance$inputs[1]]])
  x_range <- seq(x_min, x_max, length.out = 500)

  y_predictions <- sapply(x_range, function(x) {
    if (!is.numeric(x)) {
      stop("Error: x debe ser numerico")
    }

    prediction <- get_estimation.SVF(svf_instance, list(x))
    return(prediction[1])
  })

  x_frontier <- c(0)
  y_frontier <- c(0)

  for (i in seq_along(x_range)) {
    x <- x_range[i]
    y <- y_predictions[i]
    if (y > tail(y_frontier, 1)) {
      x_frontier <- c(x_frontier, x - 0.01)
      y_frontier <- c(y_frontier, tail(y_frontier, 1))
      x_frontier <- c(x_frontier, x - 0.01)
      y_frontier <- c(y_frontier, y + 0.20)
    }
  }

  plot(svf_instance$data[[svf_instance$inputs[1]]],
       svf_instance$data[[svf_instance$outputs[1]]],
       col = 'blue', pch = 16, xlab = svf_instance$inputs[1], ylab = svf_instance$outputs[1])

  lines(x_frontier, y_frontier, col = 'red')

  legend('topleft', legend = c('Datos originales', 'Frontera SVF'), col = c('blue', 'red'), pch = c(16, NA), lty = c(NA, 1))
  title(main = 'Frontera generada por el modelo SVF')
}
