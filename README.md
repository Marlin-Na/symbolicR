<!-- README.md is generated from README.Rmd. Please edit that file -->
symbolicR
=========

The goal of symbolicR is to provide a R interface to the python module sympy.

Note that currently this package is rather under experiment, if you are interested, please feel free to give any suggestion, contribute or take over this project.

We have a R package "rSymPy" on CRAN, but the package uses Jython which requires the Java installation. This package is based on the new reticulate package which provides a remarkable R interface to python.

Installation
------------

``` r
if (!require(reticulate))
    devtools::install_github("rstudio/reticulate")
#> Loading required package: reticulate
if (!require(symbolicR))
    devtools::install_github("marlin-na/symbolicR")
#> Loading required package: symbolicR
```

The package also needs python with numpy and sympy available.

Example
-------
