#' @title
#' Create Reactive Expression
#'
#' @description 
#' Creates a reactive option expression. This is a convenience/encapsulation
#' wrapper around \code{\link[reactr]{reactiveExpression}}.
#' 
#' @note 
#' This is just a temporary solution for facilitating debugging the interaction
#' with and the use of \code{\link[reactr]{reactiveExpression}} whose use is 
#' the ultimate goal. That is also why the import of \code{\link{shiny}} is
#' not clearly stated in \code{DESCRIPTION} and \code{NAMESPACE}.
#'  
#' @param expr \code{\link{expression}} (quoted or unquoted).
#' @param where  \code{\link{environment}}.
#'    The parent environment for the reactive expression. 
#'    By default, this is the calling environment, the same as when defining 
#'    an ordinary non-reactive expression.
#' @param Further arguments to be passed along to subsequent functions. 
#'    In particular:
#'    \code{\link[reactr]{reactiveExpression}}.
#' @example inst/examples/reactiveExpression.r
#' @seealso \code{
#'     \link[reactr]{reactiveExpression}
#' }
#' @template author
#' @template references
#' @export 
## @import reactr
## @import shiny
reactiveExpression <- function(
  expr, 
  where = parent.frame(), 
  ...
) {
  
#   x <- expr
#   reactr::reactiveExpression(x = expr, env = where, caller_offset = 2, ...)
#print(ls(where)) 
  
  quoted <- FALSE
  label <-NULL
  domain <- shiny::getDefaultReactiveDomain()
  caller_offset <- 1
  
  ## Ensure that shiny let's us do this //
  shiny_opt <- getOption("shiny.suppressMissingContextError")
  if (is.null(shiny_opt) || !shiny_opt) {
    options(shiny.suppressMissingContextError = TRUE)  
  }
 
  fun <- shiny::exprToFunction(expr = expr, env = where, quoted = quoted, 
    caller_offset = caller_offset)
  # Attach a label and a reference to the original user source for debugging
  if (is.null(label))
    label <- sprintf('reactiveExpression(%s)', paste(deparse(body(fun)), collapse='\n'))
  srcref <- attr(substitute(x), "srcref")
  if (length(srcref) >= 2) attr(label, "srcref") <- srcref[[2]]
  attr(label, "srcfile") <- shiny:::srcFileOfRef(srcref[[1]])
  
  out <- new.env(parent = emptyenv())
  out$fun <- fun
  out$label <- label
  out$domain <- domain
  structure(out, class = c("ReactiveExpression", "environment"))
  
}
