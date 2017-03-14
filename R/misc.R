

#' @importFrom methods is
NULL


#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`




check_py <- function () {
    refer_reticulate_text <- c(
        "please refer to the ",dQuote("reticulate")," package ",
        "for installation and configuration"
    )

    if (!reticulate::py_available())
        stop("Python is not available, ", refer_reticulate_text)
    if (!reticulate::py_numpy_available())
        stop("Numpy is not available, ", refer_reticulate_text)
    if (!reticulate::py_module_available("sympy"))
        stop("Sympy is not available, ", refer_reticulate_text)
}
