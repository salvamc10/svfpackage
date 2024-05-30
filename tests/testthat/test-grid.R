library(testthat)

# Definir listas de inputs, outputs y la cantidad de particiones
data <- data.frame(x1 = c(1, 2, 3, 4), x2 = c(1, 3, 1, 2), y1 = c(2, 4, 3, 5), y2 = c(1, 2, 3, 4))
inputs <- c("x1", "x2")
outputs <- c("y1", "y2")
d <- 2

# Prueba de la función GRID
test_that("GRID function initializes correctly", {
  grid_instance <- GRID(data, inputs, outputs, d)

  expect_s3_class(grid_instance, "GRID")
  expect_equal(grid_instance$data, data)
  expect_equal(grid_instance$inputs, inputs)
  expect_equal(grid_instance$outputs, outputs)
  expect_equal(grid_instance$d, d)
  expect_null(grid_instance$data_grid)
  expect_null(grid_instance$knot_list)
})

# Prueba de la función transformation
test_that("transformation function works correctly", {
  expect_equal(transformation(3, 2), 1)
  expect_equal(transformation(2, 2), 0)
  expect_equal(transformation(1, 2), -1)
})

# Prueba de la función search_dmu.GRID
test_that("search_dmu.GRID function works correctly", {

  grid_instance <- GRID(data, inputs, outputs, d)
  grid_instance$knot_list <- list(list(1, 2.5, 4), list(1, 2, 3))
  dmu <- c(3, 4)

  position <- search_dmu.GRID(grid_instance, dmu)

  expect_equal(position, c(2, 3))
})
