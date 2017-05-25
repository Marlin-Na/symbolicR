

#' @import methods
NULL

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @import reticulate
#' @export
reticulate::py_available

#' @export
reticulate::py_numpy_available

#' @export
reticulate::py_module_available




#' Convert a tuple or dict to R object
#'
#' @param x
#'     A python object
#' @param add_sym_class
#'     Logical value indicating whether to add "symbolic" class to the elements
#'     that can not be converted.
#' @return
#'     A corresponding R object, if any of its elements can not be converted to
#'     R, a "symbolic" class will be added to that element.
#' @export
#' @examples
#' syms_init(x, y)
#' sym_func("div")(x^3L + 6L*x + 1L, x)
#' sym_func("div")(x^3L + 6L*x + 1L, x) %>% R
#' sym_func("div")(x^3L + 6L*x + 1L, x) %>% R %>% lapply(class)
R <- function(x, add_sym_class = TRUE) {
    further <- function (x) {
        if (inherits(x, "symbolic"))
            x
        # TODO: How to convert other types??
        else if (inherits(x, "sympy.logic.boolalg.BooleanAtom"))
            if (inherits(x, "sympy.logic.boolalg.BooleanTrue"))
                TRUE
            else if (inherits(x, "sympy.logic.boolalg.BooleanFalse"))
                FALSE
            else {
                warning("Do not know TRUE or FALSE, NA generated")
                NA
            }
        else if (inherits(x, "python.builtin.object")) {
            if (add_sym_class) {
                class(x) <- append("symbolic", class(x))
                x
            }
            else
                x
        }
        else if (is.list(x))
            lapply(x, further)
        else
            x
    }

    # TODO: Throw an error if the object can not be converted?
    r <- reticulate::py_to_r(x)
    further(r)
}





dots <- function (...) {
    eval(substitute(alist(...)))
}



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

