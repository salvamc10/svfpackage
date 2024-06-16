source("R/grid.R")
library(ggplot2)

# Definir los datos
data <- data.frame(
  x1 = c(1, 2, 3, 4),
  y1 = c(1, 3, 2, 4)
)

# Definir listas de inputs, outputs y la cantidad de particiones
inputs <- c("x1")
outputs <- c("y1")
d <- 2

# Crear instancia de GRID
grid_instance <- GRID(data, inputs, outputs, d)

print(grid_instance)

# Evaluar una observación igual al valor del nodo del grid
result <- transformation(3, 2)
cat("Resultado de la transformación:", result, "\n")

# Definir knot_list como lista de listas
grid_instance$knot_list <- list(c(1, 2.5, 4))
dmu <- c(3)
position <- search_dmu.GRID(grid_instance, dmu)

print(paste("Posición en el grid: (", paste(position - 1, collapse = ", "), ")", sep = ""))

# Función para graficar el grid
plot_GRID <- function(grid_instance, data, dmu) {
  # Verificar la cantidad de inputs
  num_inputs <- length(grid_instance$inputs)

  if (num_inputs == 1) {
    # Para un solo input, crear una segunda dimensión ficticia
    x_grid <- unlist(grid_instance$knot_list[[1]])
    y_grid <- rep(0, length(x_grid))  # Dimensión ficticia
    data_df <- data.frame(x = data[[grid_instance$inputs[1]]], y = rep(0, nrow(data)))
    dmu_df <- data.frame(x = dmu[[1]], y = 0)
  } else {
    # Para múltiples inputs
    x_grid <- unlist(grid_instance$knot_list[[1]])
    y_grid <- unlist(grid_instance$knot_list[[2]])
    data_df <- data.frame(x = data[[grid_instance$inputs[1]]], y = data[[grid_instance$inputs[2]]])
    dmu_df <- data.frame(x = dmu[[1]], y = dmu[[2]])
  }

  # Crear un dataframe con los puntos del grid
  grid_df <- expand.grid(x = x_grid, y = y_grid)

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
    coord_cartesian(xlim = c(min(x_grid) - 1, max(x_grid) + 1), ylim = c(min(y_grid) - 1, max(y_grid) + 1)) +
    scale_x_continuous(expand = expansion(add = c(0, 0.1))) +
    scale_y_continuous(expand = expansion(add = c(0, 0.1))) +
    # Agregar borde alrededor del gráfico
    theme(
      plot.margin = margin(1, 1, 1, 1, "cm"),
      panel.border = element_rect(color = "black", linewidth = 0.5, fill = NA)
    ) +
    # Configuraciones adicionales
    labs(x = grid_instance$inputs[1], y = ifelse(num_inputs > 1, grid_instance$inputs[2], "Dummy Dimension"), title = "Grid con Inputs y DMU") +
    theme_minimal()

  return(p)
}

# Graficar el grid con los datos y la DMU
plot_GRID(grid_instance, data, dmu)
