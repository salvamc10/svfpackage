# Crear un objeto SVFPrimalSolution con valores de ejemplo
w_values <- c(0.5, 1.5, 2.5)
xi_values <- c(0.1, 0.2, 0.3)
solution <- SVFPrimalSolution(w_values, xi_values)
# Verificar la clase del objeto
class(solution)

# Acceder a los atributos del objeto
solution$w
solution$xi
