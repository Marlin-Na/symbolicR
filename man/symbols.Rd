% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sympy_import.R
\name{symbols}
\alias{symbols}
\alias{sym}
\alias{syms_init}
\title{Initiate Symbols}
\usage{
sym(name, ...)

syms_init(..., .envir = parent.frame(), .quite = FALSE)
}
\arguments{
\item{...}{For \code{sym}, pass the assumptions of the symbol.
For \code{syms_init}, arguments with a name will be treated as assumptions,
while arguments without a name are substituted as symbol names
to initiate. See example.}

\item{.envir}{The environment to initiate the symbols.}

\item{.quite}{Logical value to indicate whether to show messages when initiating the symbols}
}
\value{
\code{sym} returns a sympy symbol.
    \code{syms_init} invisibly returns a list of the initiated symbols.
}
\description{
Symbols can be initiated with \code{sym} or \code{syms_init}
}
\examples{
x <- sym("x", real = TRUE)
syms_init(y, z, positive = TRUE, .quite = TRUE)
x
y
class(x)
}
