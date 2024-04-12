# Ejemplo de búsqueda de celda contigua
cell <- c(2, 2)
contiguous_cells <- search_contiguous_cell(cell)
print(paste("Celdas contiguas: (", paste(contiguous_cells, collapse = ", "), ")", sep = ""))
