data <- data.frame(x1 = c(1, 2, 3), x2 = c(4, 5, 6), y1 = c(7, 8, 9))
inputs <- c("x1", "x2")
outputs <- c("y1")
d <- c(2, 2)
grid <- GRID(data = data, inputs = inputs, outputs = outputs, d = d)

# Buscar una observación en el grid
search_dmu(grid, c(2, 5))
