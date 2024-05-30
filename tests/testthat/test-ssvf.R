library(testthat)

# Definir listas de inputs, outputs y la cantidad de particiones
data <- data.frame(x1 = c(1, 2, 3, 4), x2 = c(1, 3, 1, 2), y1 = c(2, 4, 3, 5), y2 = c(1, 2, 3, 4))
inputs <- c("x1", "x2")
outputs <- c("y1", "y2")
C <- 1
eps <- 0
d <- 2
method <- 'SVF'

# Prueba para la función SSVF
test_that("SSVF function initializes correctly", {
  ssvf <- SSVF(method, inputs, outputs, data, C, eps, d)

  # Verificar la clase del objeto
  expect_s3_class(ssvf, "SSVF")
  expect_s3_class(ssvf, "SVF")

  # Verificar los valores de los atributos
  expect_equal(ssvf$method, method)
  expect_equal(ssvf$inputs, inputs)
  expect_equal(ssvf$outputs, outputs)
  expect_equal(ssvf$data, data)
  expect_equal(ssvf$c, C)
  expect_equal(ssvf$eps, eps)
  expect_equal(ssvf$d, d)
})

# Prueba para la función train.SSVF
test_that("train.SSVF trains the model correctly", {
  ssvf <- SSVF(method, inputs, outputs, data, C, eps, d)
  trained_svf <- train.SSVF(ssvf)

  expect_true(!is.null(trained_svf$model))
  expect_true(is.list(trained_svf$grid))
  expect_true(all(c("id_cells", "values", "phi") %in% names(trained_svf$grid$df_grid)))
})

# Prueba para la función solve
test_that("solve extracts solutions correctly", {
  ssvf <- SSVF(method, inputs, outputs, data, C, eps, d)
  trained_svf <- train.SSVF(ssvf)
  solution_svf <- solve(trained_svf)

  expect_true(is.list(solution_svf))
  expect_true(all(c("w", "xi") %in% names(solution_svf)))

  # Verificar los valores de la solución w
  expected_w1 <- c(2, 2, 0, 1, 0, 0, 0, 0, 0)
  expected_w2 <- c(1, 1, 0, 2, 0, 0, 0, 0, 0)
  expect_equal(solution_svf$w[[1]], expected_w1)
  expect_equal(solution_svf$w[[2]], expected_w2)

  # Verificar los valores de la solución xi
  expected_xi1 <- c(0, 0, 0, 0)
  expected_xi2 <- c(0, 0, 0, 0)
  expect_equal(solution_svf$xi[[1]], expected_xi1)
  expect_equal(solution_svf$xi[[2]], expected_xi2)
})
