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

The package also needs python with numpy and sympy available, you may want to use functions from the reticulate package to check.

``` r
reticulate::py_available()
reticulate::py_numpy_available()
reticulate::py_module_available("sympy")
```

Usage
-----

### *%:%* Operator

TODO...

(I intended to provide an operator `%:%` (or with another name) to assign symbolic expressions, by doing so, the variables on the right side (may be identified with `all.vars`) will automatically substituted as `sym("variable_name")`, and all binary operators and math functions will be substituted as the corresponding sympy functions. While expressions wrapped with `.()` will not be substituted and evaluated in R (as inspired by `bquote`). For example, `z %:% a * sin(x) + .(y)` may be equivalent to `z <- sym("a") * sin(sym("x")) + y`.)

(Not implemented yet)

### Symbols

We provide two ways to initiate a symbol. First, you can pass a character to function `sym` with assumptions and assign it to a R variable.

``` r
x <- sym("x", negative = TRUE)
y <- sym("y", positive = TRUE)
x >= y
#> False
```

However it is not convenient when you want to initiate two or more variables, thus we provide a function `syms_init` to automatically initiate and assign symbols to a given environment. In this function, named arguments will be treated as assumptions while arguments without a name will be deparsed and initiated as symbols.

``` r
syms_init(alpha, beta, gamma, real = TRUE)
#> Initiate symbol "alpha"
#> Initiate symbol "beta"
#> Initiate symbol "gamma"
alpha ^ beta
#>  β
#> α
```

### R Operator on Symbolic Expressions

The following R operators can be directly used on the symbolic expressions.

-   `+`, `-`, `*`, `/`, `^`, `%%`, `%/%`

-   `&`, `|`, `!`

-   `==`, `!=`, `<`, `<=`, `>=`, `>`

``` r
(x + y) / x
#> x + y
#> ─────
#>   x
```

However the returned object by these operators is no longer a numeric or logical value, but also a symbolic expression. To evalulate and convert a symbolic expression, use .....

### Math Functions on Symbolic Expressions

TODO...

### Other Algebra Functions

TODO...

### Convert a Symbolic Expression to a *R* Function

TODO...
