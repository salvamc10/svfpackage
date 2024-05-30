library(testthat)

# Definir listas de inputs, outputs y la cantidad de particiones
data <- data.frame(x1 = c(1, 2, 3, 4), x2 = c(1, 3, 1, 2), y1 = c(2, 4, 3, 5), y2 = c(1, 2, 3, 4))
inputs <- c("x1", "x2")
outputs <- c("y1", "y2")
C <- 1
eps <- 0
d <- 2
method <- 'SVF'

# Prueba para la función SVF
test_that("SVF function initializes correctly", {

  svf_obj <- SVF(method, inputs, outputs, data, C, eps, d)

  # Verificar la clase del objeto
  expect_s3_class(svf_obj, "SVF")

  # Verificar los valores de los atributos
  expect_equal(svf_obj$method, method)
  expect_equal(svf_obj$inputs, inputs)
  expect_equal(svf_obj$outputs, outputs)
  expect_equal(svf_obj$data, data)
  expect_equal(svf_obj$C, C)
  expect_equal(svf_obj$eps, eps)
  expect_equal(svf_obj$d, d)
})

test_that("get_estimation.SVF returns correct estimations", {

  # Definir los datos
  data <- data.frame(
    x1 = c(1, 2, 3, 4),
    x2 = c(1, 3, 2, 4),
    y1 = c(1, 3, 2, 4)
  )

  # Definir listas de inputs, outputs y la cantidad de particiones
  inputs <- c("x1", "x2")
  outputs <- c("y1")
  d <- 2

  # Parámetros iniciales del modelo SVF
  C <- 1
  eps <- 0
  method <- 'SSVF'

  # Instanciar y entrenar el modelo SVF
  svf_instance <- create_SVF(method, inputs, outputs, data, C, eps, d)
  svf_instance <- train.SSVF(svf_instance)

  # Resolver el modelo
  svf_solution <- solve(svf_instance)

  # Probar la función get_estimation.SVF con varias DMUs
  dmu_list <- list(
    c(1, 2),
    c(3, 4),
    c(1, 7),
    c(7, 1),
    c(2, 4),
    c(7, 1)
  )

  expected_estimations <- list(
    c(1),
    c(4),
    c(3),
    c(2),
    c(3),
    c(2)
  )

  for (i in seq_along(dmu_list)) {
    estimation <- get_estimation.SVF(svf_instance, dmu_list[[i]])
    expect_equal(estimation, expected_estimations[[i]], tolerance = 1e-3)
  }
})
