library(testthat)

# Definir la función SVFPrimalSolution
SVFPrimalSolution <- function(w, xi) {
  solution <- list(w = w, xi = xi)
  class(solution) <- "SVFPrimalSolution"
  return(solution)
}

# Definir los datos de prueba
w_values <- c(0.5, 1.5, 2.5)
xi_values <- c(0.1, 0.2, 0.3)

# Pruebas unitarias para SVFPrimalSolution
test_that("SVFPrimalSolution function initializes correctly", {

  # Crear el objeto SVFPrimalSolution
  solution <- SVFPrimalSolution(w_values, xi_values)

  # Verificar la clase del objeto
  expect_s3_class(solution, "SVFPrimalSolution")

  # Verificar los valores de los atributos
  expect_equal(solution$w, w_values)
  expect_equal(solution$xi, xi_values)
})
