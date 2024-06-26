% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/svf_functions.R
\name{create_SVF}
\alias{create_SVF}
\title{Función que crea un objeto del tipo SVF en función del método que se selecciona}
\usage{
create_SVF(method, inputs, outputs, data, c, eps, d)
}
\arguments{
\item{method}{Método SVF que se quiere utilizar.}

\item{inputs}{Inputs a evaluar en el conjunto de datos.}

\item{outputs}{Outputs a evaluar en el conjunto de datos.}

\item{data}{Conjunto de datos a evaluar.}

\item{c}{Valores del hiperparámetro C del modelo.}

\item{eps}{Valores del hiperparámetro épsilon del modelo.}

\item{d}{Valor del hiperparámetro d del modelo.}
}
\value{
Devuelve un objeto del método SVF seleccionado.
}
\description{
Esta función es un constructor que permite crear diferentes tipos de objetos SVF.
Actualmente, solo soporta el tipo 'SSVF', pero puede ser extendida para incluir
otros tipos. Si el método especificado no es soportado, se producirá un error.
}
\examples{
# Método print para la clase SSVF
print.SSVF <- function(x) {
  cat("Detalles del modelo SSVF:\n")
  cat("----------------------------------\n")
  cat(sprintf("Method: \%s\n", x$method))
  cat(sprintf("Inputs: \%s\n", paste(x$inputs, collapse = ", ")))
  cat(sprintf("Outputs: \%s\n", paste(x$outputs, collapse = ", ")))
  cat(sprintf("Dimensiones de Data: \%d rows, \%d columns\n", nrow(x$data), ncol(x$data)))

  cat("Hyperparámetos:\n")
  cat(sprintf("  C: \%f\n", x$c))
  cat(sprintf("  Epsilon: \%f\n", x$eps))
  cat(sprintf("  d (Número de particiones): \%d\n", x$d))

  cat("Data Preview (solo las primeras columnas):\n")
  if (nrow(x$data) > 0) {
    print(head(x$data))
  } else {
    cat("No hay datos.\n")
  }

  cat("----------------------------------\n")
  invisible(x)
}

data <- data.frame(x1 = c(1, 2, 3, 4), x2 = c(5, 6, 7, 8), y1 = c(9, 1, 2, 3))
inputs <- c("x1", "x2")
outputs <- c("y1")
d <- 2
C <- 1
eps <- 0
method <- 'SSVF'

svf <- create_SVF(method, inputs, outputs, data, C, eps, d)
print(svf)
}
