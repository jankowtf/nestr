#' @title
#' From JSON (generic)
#'
#' @description 
#' Retrieves value from a nested object structure based on a path-like 
#' \code{input} with the last ID component being the actual object name that contains
#' the value.
#' 
#' @template path-like-ids
#'   	
#' @param input \strong{Signature argument}.
#'    Object containing JSON input.
#' @param where \strong{Signature argument}.
#'    Object containing location information.  
#' @param strict \code{\link{logical}}. 
#'    \code{TRUE}: 
#'    \code{FALSE}: 
#' @param Further arguments passed along to subsequent functions. 
#'    In particular: 
#'    \code{\link[jsonlite]{fromJSON}}.
#' @example inst/examples/fromJson.r
#' @seealso \code{
#'   	\link[nestr]{fromJson-char-env-method},
#'     \link[nestr]{setNested},
#'     \link[nestr]{rmNested}
#' }
#' @template author
#' @template references
#' @export 
setGeneric(
  name = "fromJson",
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
    standardGeneric("fromJson")       
  }
)

#' @title
#' From JSON (char-miss)
#'
#' @description 
#' See generic: \code{\link[nestr]{fromJson}}
#'      
#' @inheritParams fromJson
#' @param input \code{\link{character}}.
#' @param where \code{\link{missing}}.
#' @return See method 
#'    \code{\link[nestr]{fromJson-char-char-method}}
#' @example inst/examples/fromJson.r
#' @seealso \code{
#'    \link[nestr]{fromJson}
#' }
#' @template author
#' @template references
#' @aliases fromJson-char-miss-method
#' @export
setMethod(
  f = "fromJson", 
  signature = signature(
    input = "character",
    where = "missing"
  ), 
  definition = function(
    input,
    where,
    strict,
    ...
  ) {
 
  fromJson(
    input = input,
    where = where,
    strict = strict,
    ...
  )    
    
  }
)

#' @title
#' From JSON (json-miss)
#'
#' @description 
#' See generic: \code{\link[nestr]{fromJson}}
#'      
#' @inheritParams fromJson
#' @param input \code{\link{json}}.
#' @param where \code{\link{missing}}.
#' @return See method 
#' 		\code{\link[nestr]{fromJson-json-env-method}}.
#' @example inst/examples/fromJson.r
#' @seealso \code{
#'    \link[nestr]{fromJson}
#' }
#' @template author
#' @template references
#' @aliases fromJson-json-miss-method
#' @import conditionr
#' @import jsonlite
#' @export
setMethod(
  f = "fromJson", 
  signature = signature(
    input = "json",
    where = "missing"
  ), 
  definition = function(
    input,
    where,
    strict,
    ...
  ) {

  fromJson(
    input = input,
    where = where,
    strict = strict,
    ...
  )
    
  }
)

#' @title
#' From JSON (json-env)
#'
#' @description 
#' See generic: \code{\link[nestr]{fromJson}}
#'      
#' @inheritParams fromJson
#' @param input \code{\link{json}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{environment}}. Nested object structure.
#' @example inst/examples/fromJson.r
#' @seealso \code{
#'    \link[nestr]{fromJson}
#' }
#' @template author
#' @template references
#' @aliases fromJson-json-env-method
#' @import conditionr
#' @import jsonlite
#' @export
setMethod(
  f = "fromJson", 
  signature = signature(
    input = "json",
    where = "environment"
  ), 
  definition = function(
    input,
    where,
    strict,
    ...
  ) {

  input <- tryCatch(
    fromJSON(input, ...),
    error = function(cond) {
      message(conditionMessage(cond))
      NULL
    },
    warning = function(cond) {
      message(conditionMessage(cond))
      NULL
    }
  )
  if (is.null(input)) {
    return(input)
  }

  fromList(input = input, where = where)
    
  }
)

#' @title
#' From JSON (char-env)
#'
#' @description 
#' See generic: \code{\link[nestr]{fromJson}}
#'   	 
#' @inheritParams fromJson
#' @param input \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{environment}}. Nested object structure.
#' @example inst/examples/fromJson.r
#' @seealso \code{
#'    \link[nestr]{fromJson}
#' }
#' @template author
#' @template references
#' @aliases fromJson-char-miss-method
#' @import conditionr
#' @import jsonlite
#' @export
setMethod(
  f = "fromJson", 
  signature = signature(
    input = "character",
    where = "environment"
  ), 
  definition = function(
    input,
    where,
    strict,
    ...
  ) {

  input <- tryCatch(
    fromJSON(input, ...),
    error = function(cond) {
      message(conditionMessage(cond))
      NULL
    },
    warning = function(cond) {
      message(conditionMessage(cond))
      NULL
    }
  )
  if (is.null(input)) {
    return(input)
  }
  
  fromList(input = input, where = where)

  }
)
