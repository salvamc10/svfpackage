# Evaluar una observación menor que el valor del nodo del grid
result <- transformation(2, 5)
cat("Resultado de la transformación:", result, "\n")
# Output: -1

# Evaluar una observación igual al valor del nodo del grid
result <- transformation(3, 3)
cat("Resultado de la transformación:", result, "\n")
# Output: 0

# Evaluar una observación mayor que el valor del nodo del grid
result <- transformation(4, 2)
cat("Resultado de la transformación:", result, "\n")
# Output: 1
