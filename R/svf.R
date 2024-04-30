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
  structure(list(method = method, inputs = inputs, outputs = outputs, data = data, C = C, eps = eps, d = d), class = "SVF")
}
