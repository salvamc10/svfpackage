# Definición de la clase SVFPrimalSolution
#' Crea un objeto SVFPrimalSolution
#'
#' @param w Valores de los pesos w del modelo ya solucionado.
#' @param xi Valores xi del modelo ya solucionado.
#'
#' @return Un objeto de clase SVFPrimalSolution.
#'
#' @example examples/example_SVFPrimalSolution.R
#'
#' @export
SVFPrimalSolution <- function(w, xi) {
  solution <- list(w = w, xi = xi)
  class(solution) <- "SVFPrimalSolution"
  return(solution)
}
