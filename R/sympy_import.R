

#' Get the sympy module cached in a R session
#'
#' The sympy module is cached with `options("symbolicR.sympymoudle")`, this function
#' will check whether the option exists, if not, it will use `reticulate::import`
#' to import sympy and cache it.
#'
#' @param refresh
#'     Logical value to indicate whether to force importing and refreshing the cache.
#' @return
#'     The function returns the sympy module.
#' @export
sympy_module <- function (refresh = FALSE) {
    op <- getOption("symbolicR.sympymoudle")

    if (refresh || is.null(op)) {
        # Question: Whether to use convert=TRUE or not??
        sympy <- reticulate::import("sympy", convert = FALSE, delay_load = FALSE)
        options(symbolicR.sympymoudle = sympy)
        return(sympy)
    }
    else if (reticulate::py_is_null_xptr(op))
        return(sympy_module(refresh = TRUE))
    else
        return(op)
}



#' @export
sym_func <- function (func_name) {
    # The function generate a function which wraps the function imported with
    # reticulate and add class "symbolic" to the result.

    args <- alist(... = )

    body <- substitute({
        argv <- list(...)
        result <- do.call(inner_func, argv)

        # Add class "symbolic" to the result
        class(result) <- append("symbolic", class(result))
        result
    }, list(
        inner_func = bquote(sympy_module()[[.(func_name)]])
    ))

    ans <- function () {}
    body(ans) <- body
    formals(ans) <- args
    environment(ans) <- parent.frame()
    ans
}






#####-- Export some functions from sympy    ------------------------------------


### Commons

#' @export
S <- sym_func("S")

#' @export
Simplify <- sym_func("simplify")

#' @export
Expand <- sym_func("expand")

#' @export
Rational <- sym_func("Rational")








#####--- Expression substitute        ------------------------------------------


noImplMsgTemplate <- function (name) {
    ans <- function (...) {}
    body <- bquote(
        stop("Function ", dQuote(.(name)), " for symbolic class not implemented yet\n")
    )
    body(ans) <- body
    environment(ans) <- parent.frame()
    ans
}

# substituteExpr <- function (expr, replacement) {
#     # A wrapper of `substitute`
#     #
#     # Args:
#     #   expr: A quoted R expression;
#     #   replacement: A list passed as pairs of expression to substitute
#     sub <- substitute(
#         substitute(expr, replacement),
#         list(expr = expr, replacement = replacement)
#     )
#     eval(sub)
# }


evalWithSubEnv <- function (expr, replacement, parenv = parent.frame()) {
    # Eval an expression in a temp env which substitute a list of functions
    # This may be more robust than using `substitute`
    #
    # Args:
    #   expr: A quoted R expression;
    #   replacement: A named list for creating the tmp env;
    #   parenv: The parent environment
    env <- as.environment(replacement)
    parent.env(env) <- parenv
    eval(expr, envir = env)
}


### Operators

subOpsList <- list(
    "+"   = function (e1, e2) {
        if (missing(e2))
            sym_func("Add")(e1)
        else
            sym_func("Add")(e1, e2)
    },
    "*"   = function (...) {
        sym_func("Mul")(...)
    },
    "-"   = function (e1, e2) {
        if (missing(e2))
            sym_func("Mul")(-1L, e1)
        else
            sym_func("Add")(e1, sym_func("Mul")(-1L, e2))
    },
    "/"   = function (e1, e2) {
        sym_func("Mul")(e1, sym_func("Pow")(e2, -1L))
    },
    "^"   = function (e1, e2) {
        sym_func("Pow")(e1, e2)
    },
    "%%"  = function (e1, e2) {
        div <- sym_func("div")(e1, e2)
        ans <- R(div)[[2]]
        ans
    },
    "%/%" = function (e1, e2) {
        div <- sym_func("div")(e1, e2)
        ans <- R(div)[[1]]
        ans
    },

    "&"   = sym_func("And"),
    "|"   = sym_func("Or"),
    "!"   = sym_func("Not"),

    "=="  = sym_func("Eq"),
    "!="  = function(e1, e2) {
        eq <- sym_func("Eq")(e1, e2)
        sym_func("Not")(eq)
    },
    "<"   = sym_func("StrictLessThan"),
    "<="  = sym_func("LessThan"),
    ">="  = sym_func("GreaterThan"),
    ">"   = sym_func("StrictGreaterThan")
)

#' Ops S3 Methods for Symbolic Expressions
#'
#' @param e1,e2
#'     Left argument and right argument.
#' @return
#'     All these methods return a symbolic expression.
#' @export
#'
#' @examples
#' syms_init(x, y, z)
#' x + y
#' x * y
#' x ^ y
#' z != x + y
#' z != x + y | x + 1L == x + 1
Ops.symbolic <- function (e1, e2) {
    if (missing(e2))
        if (.Generic %in% c("!", "+", "-"))
            cal <- call(.Generic, e1)
        else
            stop()
    else
        cal <- call(.Generic, e1, e2)

    evalWithSubEnv(cal, replacement = subOpsList, parenv = .GenericCallEnv)
}


### Math Functions

subMathList <- list(
    abs = noImplMsgTemplate("abs"),
    sign = noImplMsgTemplate("sign"),
    sqrt = noImplMsgTemplate("sqrt"),
    floor = noImplMsgTemplate("floor"),
    ceiling = noImplMsgTemplate("ceiling"),
    trunc = noImplMsgTemplate("trunc"),
    round = noImplMsgTemplate("round"),
    signif = noImplMsgTemplate("signif"),

    exp = noImplMsgTemplate("exp"),
    log = noImplMsgTemplate("log"),
    expm1 = noImplMsgTemplate("expm1"),
    log1p = noImplMsgTemplate("log1p"),
    cos = noImplMsgTemplate("cos"),
    sin = noImplMsgTemplate("sin"),
    tan = noImplMsgTemplate("tan"),
    cospi = noImplMsgTemplate("cospi"),
    sinpi = noImplMsgTemplate("sinpi"),
    tanpi = noImplMsgTemplate("tanpi"),
    acos = noImplMsgTemplate("acos"),
    asin = noImplMsgTemplate("asin"),
    atan = noImplMsgTemplate("atan"),

    cosh = noImplMsgTemplate("cosh"),
    sinh = noImplMsgTemplate("sinh"),
    tanh = noImplMsgTemplate("tanh"),
    acosh = noImplMsgTemplate("acosh"),
    asinh = noImplMsgTemplate("asinh"),
    atanh = noImplMsgTemplate("atanh"),

    lgamma = noImplMsgTemplate("lgamma"),
    gamma = noImplMsgTemplate("gamma"),
    digamma = noImplMsgTemplate("digamma"),
    trigamma = noImplMsgTemplate("trigamma"),

    cumsum = noImplMsgTemplate("cumsum"),
    cumprod = noImplMsgTemplate("cumprod"),
    cummax = noImplMsgTemplate("cummax"),
    cummin = noImplMsgTemplate("cummin")
)

Math.symbolic <- function (x, ...) {

}


#####--- squote and symbolic assign   ------------------------------------------



squote <- function (expr, where = parent.frame()) {
    # unquote <- function (e) {
    #     if (is.pairlist(e))
    #         # If a function def is included in the quote, the argument will be
    #         # a pairlist, but in fact we may not want R functions to be included.
    #         as.pairlist(lapply(e, unquote))
    #     else if (length(e) <= 1L)
    #         e
    #     else if (e[[1L]] == as.name("."))
    #         eval(e[[2L]], where)
    #     else
    #         as.call(lapply(e, unquote))
    # }

    unquote(substitute(expr))
}


sassign <- function (x, value) {

}




#####---    Symbols     --------------------------------------------------------

#' Initiate Symbols
#'
#' Symbols can be initiated with \code{sym} or \code{syms_init}
#'
#' @name symbols
#'
#' @param ...
#'     For \code{sym}, pass the assumptions of the symbol.
#'     For \code{syms_init}, arguments with a name will be treated as assumptions,
#'     while arguments without a name are substituted as symbol names
#'     to initiate. See example.
#' @param .envir
#'     The environment to initiate the symbols.
#' @param .quite
#'     Logical value to indicate whether to show messages when initiating the symbols
#' @return
#'     \code{sym} returns a sympy symbol.
#'     \code{syms_init} invisibly returns a list of the initiated symbols.
#' @export
#'
#' @examples
#' x <- sym("x", real = TRUE)
#' syms_init(y, z, positive = TRUE, .quite = TRUE)
#' x
#' y
#' class(x)
sym <- sym_func("symbols")

#' @rdname symbols
#' @export
syms_init <- function (..., .envir = parent.frame(), .quite = FALSE) {
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
        call <- as.call(c(list(quote(sym)), symbols_chr[i], assumptions))
        sym <- eval(call)
        assign(symbols_chr[[i]], sym, envir = .envir)
        syms_list[[i]] <- sym
        if (!.quite)
            message("Initiate symbol ", dQuote(symbols_chr[[i]]))
    }
    names(syms_list) <- symbols_chr
    invisible(syms_list)
}






