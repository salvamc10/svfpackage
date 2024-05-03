# svfpackage

<!-- badges: start -->

[![R-CMD-check](https://github.com/salvamc10/svfpackage/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/salvamc10/svfpackage/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

El objetivo de svfpackage es proporcionar una colección de funciones y herramientas para la realización de estimaciones de fornteras

## Installation

You can install the development version of svfpackage from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("salvamc10/svfpackage")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(svfpackage)
library(Rcplex)
## basic example codes

# Usar datos de prueba
data(datos, package = "svfpackage")

# Definición de inputs, outputs y otros parámetros
inputs <- c("x1", "x2")
outputs <- c("y1")
d <- 2
C <- 1
eps <- 0
method <- 'SSVF'

# Crear y mostrar el objeto SVF
svf <- create_SVF(method, inputs, outputs, datos, C, eps, d)

trained_svf <- train.SSVF(svf)

# Resolver el modelo y mostrar resultados
solution_svf <- solve(trained_svf)
```
