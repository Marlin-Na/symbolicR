

dots <- function (...) {
    eval(substitute(alist(...)))
}

#' Initiate Symbols
#'
#' This is a convenience function to initiate symbols.
#'
#' @param ...
#'     For arguments with a name, they will be treated as assumptions and pass them
#'     to sympy, for arguments without a name, they are substituted as symbol names
#'     to initiate. See example.
#' @param .envir
#'     The environment to initiate the symbols.
#' @param .quite
#'     Logical value to indicate whether to show messages when initiating the symbols
#' @return
#'     It invisibly returns a list of the initiated symbols.
#' @export
#'
#' @examples
#' init_syms(x, y, z, positive = TRUE, .quite = TRUE)
#' x
#' class(x)
#' GreaterThan(x, 0)
init_syms <- function (..., .envir = parent.frame(), .quite = FALSE) {
    args <- dots(...)
    to_set <- {
        nms <- names(args)
        if (is.null(nms))
            rep(TRUE, length(args))
        else
            nms == ""
    }
    assumptions <- args[!to_set]
    symbols <- unique(args[to_set])
    symbols_chr <- lapply(symbols, function (x) deparse(x))
    syms_list <- vector("list", length = length(symbols_chr))
    for (i in seq_along(symbols_chr)) {
        call <- as.call(c(list(quote(Symbols)), symbols_chr[i], assumptions))
        sym <- eval(call)
        assign(symbols_chr[[i]], sym, envir = .envir)
        syms_list[[i]] <- sym
        if (!.quite)
            message("Initiate symbol ", dQuote(symbols_chr[[i]]))
    }
    names(syms_list) <- symbols_chr
    invisible(syms_list)
}

