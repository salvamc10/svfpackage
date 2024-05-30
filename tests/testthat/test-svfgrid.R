library(testthat)

# Definir listas de inputs, outputs y la cantidad de particiones
data <- data.frame(x1 = c(1, 2, 3, 4), x2 = c(1, 3, 1, 2), y1 = c(2, 4, 3, 5), y2 = c(1, 2, 3, 4))
inputs <- c("x1", "x2")
outputs <- c("y1", "y2")
d <- 2

# Prueba para SVFGrid constructor
test_that("SVFGrid function initializes correctly", {
  grid_obj <- SVFGrid(data, inputs, outputs, d)

  expect_s3_class(grid_obj, "SVFGrid")
  expect_s3_class(grid_obj, "GRID")
  expect_equal(grid_obj$data, data)
  expect_equal(grid_obj$inputs, inputs)
  expect_equal(grid_obj$outputs, outputs)
  expect_equal(grid_obj$d, d)
  expect_true(is.data.frame(grid_obj$df_grid))
  expect_true(is.data.frame(grid_obj$data_grid))
})

# Prueba para create_grid.SVFGrid
test_that("create_grid.SVFGrid works correctly", {
  grid_obj <- SVFGrid(data, inputs, outputs, d)
  grid_obj <- create_grid.SVFGrid(grid_obj)

  expect_true(length(grid_obj$knot_list) == length(inputs))
  expect_true(is.list(grid_obj$df_grid))
  expect_true(all(c("id_cells", "values", "phi") %in% names(grid_obj$df_grid)))

  # Verificar knot_list
  expect_equal(grid_obj$knot_list[[1]], c(1.0, 2.5, 4.0))
  expect_equal(grid_obj$knot_list[[2]], c(1.0, 2.0, 3.0))
})

# Prueba para search_dmu.GRID
test_that("search_dmu.GRID works correctly", {
  grid_obj <- SVFGrid(data, inputs, outputs, d)
  grid_obj <- create_grid.SVFGrid(grid_obj)

  dmu <- c(1, 3)
  position <- search_dmu.GRID(grid_obj, dmu)
  expect_equal(position, c(1, 3))
})

# Prueba para search_contiguous_cell
test_that("search_contiguous_cell works correctly", {
  cell <- c(1, 1)
  contiguous_cells <- search_contiguous_cell(cell)
  expect_equal(length(contiguous_cells), 0)

  cell <- c(2, 2)
  contiguous_cells <- search_contiguous_cell(cell)
  expect_equal(contiguous_cells, list(c(1, 2), c(2, 1)))
})

# Prueba para calculate_dmu_phi.SVFGrid
test_that("calculate_dmu_phi works correctly", {
  grid_obj <- SVFGrid(data, inputs, outputs, d)
  grid_obj <- create_grid.SVFGrid(grid_obj)

  cell <- c(1, 3)
  phi_list <- calculate_dmu_phi.SVFGrid(grid_obj, cell)

  expect_true(is.list(phi_list))
  expect_true(length(phi_list) == length(outputs))
  expect_equal(phi_list[[1]], c(1, 1, 1, 0, 0, 0, 0, 0, 0))
  expect_equal(phi_list[[2]], c(1, 1, 1, 0, 0, 0, 0, 0, 0))
})

# Prueba para calculate_df_grid.SVFGrid
test_that("calculate_df_grid works correctly", {
  grid_obj <- SVFGrid(data, inputs, outputs, d)
  grid_obj <- create_grid.SVFGrid(grid_obj)
  grid_obj <- calculate_df_grid.SVFGrid(grid_obj)

  expect_true(all(c("phi", "c_cells") %in% names(grid_obj$df_grid)))

  # Verificar los valores de phi y celdas contiguas
  expect_equal(grid_obj$df_grid$phi[[1]], list(c(1, 0, 0, 0, 0, 0, 0, 0, 0)))
  expect_equal(grid_obj$df_grid$phi[[2]], list(c(1, 1, 0, 0, 0, 0, 0, 0, 0)))
  expect_equal(grid_obj$df_grid$c_cells[[1]], list())
  expect_equal(grid_obj$df_grid$c_cells[[2]], list(c(1, 1)))
})

# Prueba para calculate_data_grid.SVFGrid
test_that("calculate_data_grid works correctly", {
  grid_obj <- SVFGrid(data, inputs, outputs, d)
  grid_obj <- create_grid.SVFGrid(grid_obj)
  grid_obj <- calculate_data_grid.SVFGrid(grid_obj)

  expect_true(all(c("phi", "c_cells") %in% names(grid_obj$data_grid)))

  # Verificar los valores de phi y celdas contiguas
  expect_equal(grid_obj$data_grid$phi[[1]], list(c(1, 0, 0, 0, 0, 0, 0, 0, 0), c(1, 0, 0, 0, 0, 0, 0, 0, 0)))
  expect_equal(grid_obj$data_grid$phi[[2]], list(c(1, 1, 1, 0, 0, 0, 0, 0, 0), c(1, 1, 1, 0, 0, 0, 0, 0, 0)))
  expect_equal(grid_obj$data_grid$c_cells[[1]], list())
  expect_equal(grid_obj$data_grid$c_cells[[2]], list(c(1, 2)))
})
