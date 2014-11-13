#' @title
#' To List (generic)
#'
#' @description 
#' Creates a nested \code{list} object structure.
#'   	
#' @param input \strong{Signature argument}.
#'    Object containing list input.
#' @param strict \code{\link{logical}}. 
#'    \code{TRUE}: 
#'    \code{FALSE}: 
#' @template threedots
#' @example inst/examples/toList.r
#' @seealso \code{
#'   	\link[nestr]{toList-env-method},
#'     \link[nestr]{setNested},
#'     \link[nestr]{rmNested}
#' }
#' @template author
#' @template references
#' @export 
setGeneric(
  name = "toList",
  signature = c(
    "input"
  ),
  def = function(
    input,
    strict = FALSE, 
    ...
  ) {
    standardGeneric("toList")       
  }
)

#' @title
#' To List (any)
#'
#' @description 
#' See generic: \code{\link[nestr]{toList}}
#'      
#' @inheritParams toList
#' @param input \code{\link{ANY}}.
#' @return See method
#'    \code{\link[nestr]{toList-env-method}}.
#' @example inst/examples/toList.r
#' @seealso \code{
#'    \link[nestr]{toList}
#' }
#' @template author
#' @template references
#' @aliases toList-any-method
#' @export
setMethod(
  f = "toList", 
  signature = signature(
    input = "ANY"
  ), 
  definition = function(
    input,
    strict,
    ...
  ) {

  input
  
  }
)

#' @title
#' To List (env)
#'
#' @description 
#' See generic: \code{\link[nestr]{toList}}
#'   	 
#' @inheritParams toList
#' @param input \code{\link{environment}}.
#' @return \code{\link{ANY}}. Option value or for non-existing option 
#'    (i.e. wrong \code{input}): \code{NULL} if \code{strict = FALSE} and an error
#'    if \code{strict = TRUE}.
#' @example inst/examples/toList.r
#' @seealso \code{
#'    \link[nestr]{toList}
#' }
#' @template author
#' @template references
#' @aliases toList-env-method
#' @export
setMethod(
  f = "toList", 
  signature = signature(
    input = "environment"
  ), 
  definition = function(
    input,
    strict,
    ...
  ) {

  if (is.environment(input)) {
    out <- eapply(input, toList) 
    names(out) <- gsub("^\\[\\d*\\]$", "", names(out))
  } else {
    out <- input
  }
  out
  
  }
)
