source("~/svfpackage/R/grid.R")

library(ggplot2)

# Usar el dataset de ejemplo
data <- read.table("~/svfpackage/data/datos.txt", header = TRUE, sep = ";")

# Definir listas de inputs, outputs y la cantidad de particiones
inputs <- c("x1", "x2")
outputs <- c("y1")
d <- 2

# Inicializar la cuadrícula
grid_instance <- initialize_GRID(data, inputs, outputs, d)

# Evaluar una observación menor que el valor del nodo del grid
result <- transformation(2, 5)
cat("Resultado de la transformación:", result, "\n")
# Output: -1

# Evaluar una observación igual al valor del nodo del grid
result <- transformation(3, 3)
cat("Resultado de la transformación:", result, "\n")
# Output: 0

# Evaluar una observación mayor que el valor del nodo del grid
result <- transformation(7, 4)
cat("Resultado de la transformación:", result, "\n")
# Output: 1

# Ejemplo de uso de la función search_dmu
grid_instance$knot_list <- list(list(1, 2.5, 4), list(1, 2, 3))
dmu <- list(2.5, 1)
position <- search_dmu(grid_instance, dmu)
position_string <- paste(position, collapse = ", ")
cat("Posición en el grid:", position_string, "\n")

# Función para graficar el grid
plot_GRID <- function(grid_instance, data, dmu) {
  # Extraer las coordenadas del grid
  x_grid <- unlist(grid_instance$knot_list[[1]])
  y_grid <- unlist(grid_instance$knot_list[[2]])

  # Crear un dataframe con los puntos del grid
  grid_df <- expand.grid(x = x_grid, y = y_grid)

  # Crear un dataframe con las coordenadas de los datos
  data_df <- data.frame(x = data$x1, y = data$x2)

  # Convertir las coordenadas de la DMU en un dataframe
  dmu_df <- data.frame(x = unlist(dmu[[1]]), y = unlist(dmu[[2]]))

  # Crear un gráfico
  p <- ggplot() +
    # Añadir los datos
    geom_point(data = data_df, aes(x = x, y = y), color = "blue", size = 3) +
    # Añadir la DMU
    geom_point(data = dmu_df, aes(x = x, y = y), color = "red", size = 3) +
    # Añadir líneas verticales y horizontales para el grid
    geom_vline(xintercept = x_grid, linetype = "dotted", color = "blue") +
    geom_hline(yintercept = y_grid, linetype = "dotted", color = "blue") +
    # Especificar límites del gráfico y ajustar la separación en los ejes x e y
    coord_cartesian(xlim = c(0, max(x_grid)), ylim = c(0, 4)) +
    scale_x_continuous(expand = expansion(add = c(0, 0.1))) +
    scale_y_continuous(expand = expansion(add = c(0, 0.1))) +
    # Agregar borde alrededor del gráfico
    theme(
      plot.margin = margin(1, 1, 1, 1, "cm"),
      panel.border = element_rect(color = "black", linewidth = 0.5, fill = NA)
    )
    # Configuraciones adicionales
    labs(x = "x1", y = "x2", title = "Grid con Inputs y DMU") +
    theme_minimal()

  return(p)
}

# Graficar el grid con los datos y la DMU
plot_GRID(grid_instance, data, dmu)
