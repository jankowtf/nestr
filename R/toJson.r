#' @title
#' To JSON (generic)
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
#' @param strict \code{\link{logical}}. 
#'    \code{TRUE}: 
#'    \code{FALSE}: 
#' @param Further arguments passed along to subsequent functions. 
#'    In particular: 
#'    \code{\link[jsonlite]{toJson}}.
#' @example inst/examples/toJson.r
#' @seealso \code{
#'   	\link[nestr]{toJson-env-method}
#' }
#' @template author
#' @template references
#' @export 
setGeneric(
  name = "toJson",
  signature = c(
    "input"
  ),
  def = function(
    input,
    strict = FALSE, 
    ...
  ) {
    standardGeneric("toJson")       
  }
)

#' @title
#' To JSON (env)
#'
#' @description 
#' See generic: \code{\link[nestr]{toJson}}
#'   	 
#' @inheritParams toJson
#' @param input \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{character}}. JSON string. 
#' @example inst/examples/toJson.r
#' @seealso \code{
#'    \link[nestr]{toJson}
#' }
#' @template author
#' @template references
#' @aliases toJson-env-method
#' @import jsonlite
#' @export
setMethod(
  f = "toJson", 
  signature = signature(
    input = "environment"
  ), 
  definition = function(
    input,
    strict,
    ...
  ) {

  input <- toList(input = input)   
  
# jsonlite::toJSON(data.frame(input))
# jsonlite::toJSON(input, dataframe = "columns")
  
  input <- tryCatch(
    jsonlite::toJSON(input, ...),
    error = function(cond) {
      message(conditionMessage(cond))
      NULL
    },
    warning = function(cond) {
      message(conditionMessage(cond))
      NULL
    }
  )
  input
  
  }
)
