


.onLoad <- function (libname, pkgname) {
    # Cache the sympy module
    get_sympy(refresh = TRUE)

}

