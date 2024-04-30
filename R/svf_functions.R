source("~/Documents/GitHub/svfpackage/R/ssvf.R")

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
