




#' Pretty Print of Symbolic Expressions
#'
#' @param expr
#'     A symbolic expression
#' @param ...
#'     Printing options, there are six options available in sympy, namely
#'     "wrap_line", "num_columns", "use_unicode", "full_prec", "order" and
#'     "use_unicode_sqrt_char". Detail see the documentation at ........
#' @return
#'     It invisibly returns the expression.
#' @export
#'
pprint <- function (expr, ...) {
    pretty <- sym_func("pretty")
    out <-
        reticulate::py_to_r(pretty(expr, ...))
    cat(out)
    invisible(expr)
}


#' @export
print.symbolic <- function (x, ...) {
    pprint(expr = x, ...)
}


