#' @section Path-like identifiers:
#' 
#' Values for \code{id} are expected to be of structure \code{a/b/c/.../z},
#' i.e. path-like identifiers using a slash as separator. 
#' The identifier is transformed to \code{a$b$c$...$z} and then in turn to a
#' valid \emph{get} or \code{assign} expression 
#' (i.e. \code{a$b$c$...$z} and \code{a$b$c$...$z <- value}).
#' Of course, "atomic" paths (e.g. only \code{a}) are also valid.
