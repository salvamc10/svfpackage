source("~/Documents/GitHub/svfpackage/R/ssvf.R")

# Método print para la clase SSVF
print.SSVF <- function(x) {
  cat("Detalles del modelo SSVF:\n")
  cat("----------------------------------\n")
  cat(sprintf("Method: %s\n", x$method))
  cat(sprintf("Inputs: %s\n", paste(x$inputs, collapse = ", ")))
  cat(sprintf("Outputs: %s\n", paste(x$outputs, collapse = ", ")))
  cat(sprintf("Dimensiones de Data: %d rows, %d columns\n", nrow(x$data), ncol(x$data)))

  cat("Hyperparámetos:\n")
  cat(sprintf("  C: %f\n", x$c))
  cat(sprintf("  Epsilon: %f\n", x$eps))
  cat(sprintf("  d (Número de particiones): %d\n", x$d))

  cat("Data Preview (solo las primeras columnas):\n")
  if (nrow(x$data) > 0) {
    print(head(x$data))
  } else {
    cat("No hay datos.\n")
  }

  cat("----------------------------------\n")
  invisible(x)
}

data <- data.frame(x1 = c(1, 2, 3, 4), x2 = c(5, 6, 7, 8), y1 = c(9, 1, 2, 3))
inputs <- c("x1", "x2")
outputs <- c("y1")
d <- 2
C <- 1
eps <- 0
method <- 'SSVF'

ssvf <- SSVF(method, inputs, outputs, data, C, eps, d)
print(ssvf)
