#' @title
#' From List (generic)
#'
#' @description 
#' Creates a nested \code{environment} object structure.
#'   	
#' @param input \strong{Signature argument}.
#'    Object containing list input.
#' @param where \strong{Signature argument}.
#'    Object containing location information.  
#' @param strict \code{\link{logical}}. 
#'    \code{TRUE}: 
#'    \code{FALSE}: 
#' @template threedots
#' @example inst/examples/fromList.r
#' @seealso \code{
#'   	\link[nestr]{fromList-char-env-method},
#'     \link[nestr]{setNested},
#'     \link[nestr]{rmNested}
#' }
#' @template author
#' @template references
#' @export 
setGeneric(
  name = "fromList",
  signature = c(
    "input",
    "where"
  ),
  def = function(
    input,
    where = parent.frame(),
    strict = FALSE, 
    ...
  ) {
    standardGeneric("fromList")       
  }
)

#' @title
#' From List (list-miss)
#'
#' @description 
#' See generic: \code{\link[nestr]{fromList}}
#'      
#' @inheritParams fromList
#' @param input \code{\link{list}}.
#' @param where \code{\link{missing}}.
#' @return See method 
#'    \code{\link[nestr]{fromList-char-char-method}}
#' @example inst/examples/fromList.r
#' @seealso \code{
#'    \link[nestr]{fromList}
#' }
#' @template author
#' @template references
#' @aliases fromList-list-miss-method
#' @export
setMethod(
  f = "fromList", 
  signature = signature(
    input = "list",
    where = "missing"
  ), 
  definition = function(
    input,
    where,
    strict,
    ...
  ) {
 
  fromList(
    input = input,
    where = where,
    strict = strict,
    ...
  )    
    
  }
)

#' @title
#' From List (list-env)
#'
#' @description 
#' See generic: \code{\link[nestr]{fromList}}
#'   	 
#' @inheritParams fromList
#' @param input \code{\link{list}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{ANY}}. Option value or for non-existing option 
#'    (i.e. wrong \code{input}): \code{NULL} if \code{strict = FALSE} and an error
#'    if \code{strict = TRUE}.
#' @example inst/examples/fromList.r
#' @seealso \code{
#'    \link[nestr]{fromList}
#' }
#' @template author
#' @template references
#' @aliases fromList-list-env-method
#' @import conditionr
#' @export
setMethod(
  f = "fromList", 
  signature = signature(
    input = "list",
    where = "environment"
  ), 
  definition = function(
    input,
    where,
    strict,
    ...
  ) {

  sapply(seq(along = input), function(ii) {
    name   <- names(input[ii])
    value  <- input[[ii]]
    
    ## Dummy names for unnamed elements //
    if (is.null(name) || name == "") {
      name <- sprintf("[%s]", ii)
    }
    
    if (all(class(value) != "list") | 
          (all(class(value) == "list") & !length(value))
    ) {
      # Leaf value //
      assign(name, value, where)
    } else {
      # Branch value >> continue //
      where[[name]] <- new.env(parent = emptyenv())
      fromList(
        input = value,
        where = where[[name]],
        ...
      )
    }
  })
  where
  
  }
)
