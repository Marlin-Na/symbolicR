


# This function tries to get the sympy module and cache it using
# "symbolicR.sympymoudle" option on user session
#' Get the sympy module cached in a R session
#'
#' The sympy module is cached to `options("symbolicR.sympymoudle")`, this function
#' will check whether the option exists, if not, it will use `reticulate::import`
#' to import sympy and cache it.
#'
#' @param refresh
#'     Logical value to indicate whether to force importing and refreshing the cache.
#' @return
#'     The function returns the sympy module.
#' @export
get_sympy <- function (refresh = FALSE) {
    op <- getOption("symbolicR.sympymoudle")

    if (refresh || is.null(op)) {
        # Question: Whether to use convert=TRUE or not??
        sympy <- reticulate::import("sympy", convert = TRUE, delay_load = FALSE)
        options(symbolicR.sympymoudle = sympy)
        return(sympy)
    }

    else
        return(op)
}


# Print using pretty_print
#' @export
pprint <- function (expr, setting_list = getOption("symbolicR.pprint")) {
    pprint <- get_sympy()$pprint
    pprint_args <- append(alist(expr), setting_list)
    out <- reticulate::py_capture_output(
        do.call(pprint, pprint_args)
    )
    cat(out)
    invisible(expr)
}

#' @export
pprint_set_default <- function () {
    # See the documentation of `sympy.pprint` in python as copied below.

    # 1. wrap_line : bool, optional line wrapping enabled/disabled, defaults to True
    # 2. num_columns : int or None, optional number of columns before line breaking
    #                  (default to None which reads the terminal width), useful when
    #                  using SymPy without terminal.
    # 3. use_unicode : bool or None, optional use unicode characters, such as the
    #                  Greek letter pi instead of the string pi.
    # 4. full_prec : bool or string, optional use full precision. Default to "auto"
    # 5. order : bool or string, optional set to 'none' for long expressions if slow;
    #            default is None
    # 6. use_unicode_sqrt_char : bool, optional use compact single-character square
    #                            root symbol (when unambiguous); default is True.

    options(
        symbolicR.pprint = list(
            wrap_line = TRUE,
            num_columns = NULL,
            use_unicode = NULL,
            full_prec = "auto",
            order = NULL,
            use_unicode_sqrt_char = TRUE
        )
    )
    invisible(getOption("symbolicR.pprint"))
}







# I originally thought to have this function to run on package load so that to
# dramatically provide the sympy functions to package user,
# however later I found it is not necessary. But anyway it can be seen a
# convenient (really?) way to wrap sympy functions.
#
# This function will wrap and assign sympy functions (specified as `import_list`) to
# the given environment.
#
# A list of these functions is invisibly returned.
Import_SymPy_Func <- function (envir = parent.frame()) {

    # Here set names and formals of the functions that will be imported
    import_list <- list(
        Add = alist(get_sympy()$Add, lhs, rhs),
        Multiply = alist(get_sympy()$Mul, lhs, rhs),
        Sqrt = alist(get_sympy()$sqrt),

        GreaterThan = alist(get_sympy()$GreaterThan, lhs, rhs),
        StrictGreaterThan = alist(get_sympy()$StrictGreaterThan, lhs, rhs),
        LessThan = alist(get_sympy()$LessThan, lhs, rhs),
        StrictLessThan = alist(get_sympy()$StrictLessThan, lhs, rhs),

        Symbols = alist(get_sympy()$symbols),

        Rational = alist(get_sympy()$Rational, a, b)
    )

    # The function to wrap each element of the `import_list` as function
    wrap_func <- function (lst, envir, use_dots = TRUE) {
        argv <- {
            l <- as.list(lst)[-1]
            # Refer: http://stackoverflow.com/questions/17751862/create-a-variable-length-alist
            argv <- rep(list(quote(expr = )), length(l))
            names(argv) <- l
            if (use_dots) {
                argv <- append(argv, alist(... = ))
            }
            as.pairlist(argv)
        }
        body <- {
            if (use_dots)
                lst <- append(lst, list(quote(...)))
            as.call(lst)
        }

        env <- envir
        eval(call("function", argv, body), env)
    }

    env <- envir
    funclist <- vector("list", length = length(import_list))

    for (i in seq_along(import_list)) {
        func <- wrap_func(import_list[[i]], envir = env)
        name <- names(import_list)[i]
        # Assign to the given environment and also collect them as a list
        funclist[[i]] <- func
        # Throw out a warning if function with the same name exists in that env
        if (exists(name, envir = env, mode = "function") &&
            !identical(func, get(name, envir = env, mode = "function")))
            warning("Function ",dQuote(name), " is overwritten")
        assign(name, func, envir = env)
    }
    names(funclist) <- names(import_list)
    invisible(funclist)
}


Import_SymPy_Func()


#' @export
Add <- Add

#' @export
Multiply <- Multiply

#' @export
Sqrt <- Sqrt

#' @export
GreaterThan <- GreaterThan

#' @export
StrictGreaterThan <- StrictGreaterThan

#' @export
LessThan <- LessThan

#' @export
StrictLessThan <- StrictLessThan


#' @export
Symbols <- Symbols


#' @export
Rational <- Rational

