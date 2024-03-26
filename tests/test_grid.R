source("~/Desktop/svfpackage/R/grid.R")

# Usar el dataset de ejemplo
data <- read.table("~/Desktop/svfpackage/data/datos.txt", header = TRUE, sep = ";")

# Definir listas de inputs, outputs y la cantidad de particiones
inputs <- c("x1", "x2")
outputs <- c("y1")
d <- c(2)

# Crear un objeto GRID
grid <- GRID(data = data, inputs = inputs, outputs = outputs, d = d)

# Evaluar una observación menor que el valor del nodo del grid
transformation(2, 5)
# Output: -1

# Evaluar una observación igual al valor del nodo del grid
transformation(3, 3)
# Output: 0

# Evaluar una observación mayor que el valor del nodo del grid
transformation(7, 4)
# Output: 1

# Generar el grid
x1_grid <- seq(min(data$x1), max(data$x1), length.out = d[1])
expand_grid <- function(...) expand.grid(...)
X_grid <- expand_grid(x1_grid)

# Graficar el grid
plot(data$x1, data$x2, col = 'blue', xlab = 'x1', ylab = 'x2', main = 'Grid', xlim = range(data$x1), ylim = range(data$x2))
grid()

# Agregar etiquetas a cada punto
text(data$x1, data$x2, labels = paste("(", round(data$x1, 2), ",", round(data$x2, 2), ")"), pos = 3)

# Buscar una observación en el grid
search_dmu(grid, c(2, 5))

