#' @title
#' Get Nested (generic)
#'
#' @description 
#' Retrieves value from a nested object structure based on a path-like 
#' \code{id} with the last ID component being the actual object name that contains
#' the value.
#' 
#' @template path-like-ids
#'   	
#' @param id \strong{Signature argument}.
#'    Object containing path-like name/ID information.
#' @param where \strong{Signature argument}.
#'    Object containing location information.  
#' @param default \code{\link{ANY}}. 
#'    Value to be returned if component does not exist. 
#' @param strict \code{\link{logical}}. 
#'    \code{TRUE}: \code{id} pointing to a non-existing component triggers
#'    error; \code{FALSE}: \code{id} pointing to a non-existing component leads
#'    to return value \code{NULL}.
#' @template threedots
#' @example inst/examples/getNested.r
#' @seealso \code{
#'   	\link[nestr]{getNested-char-env-method},
#'     \link[nestr]{setNested},
#'     \link[nestr]{rmNested}
#' }
#' @template author
#' @template references
#' @export 
setGeneric(
  name = "getNested",
  signature = c(
    "id",
    "where"
  ),
  def = function(
    id,
    where = parent.frame(),
    default = NULL,
    strict = FALSE, 
    ...
  ) {
    standardGeneric("getNested")       
  }
)

#' @title
#' Get Nested (char-miss)
#'
#' @description 
#' See generic: \code{\link[nestr]{getNested}}
#'      
#' @inheritParams getNested
#' @param id \code{\link{character}}.
#' @param where \code{\link{missing}}.
#' @return See method 
#'    \code{\link[nestr]{getNested-char-char-method}}
#' @example inst/examples/getNested.r
#' @seealso \code{
#'    \link[nestr]{getNested}
#' }
#' @template author
#' @template references
#' @aliases getNested-char-miss-method
#' @export
setMethod(
  f = "getNested", 
  signature = signature(
    id = "character",
    where = "missing"
  ), 
  definition = function(
    id,
    where,
    default,
    strict,
    ...
  ) {
 
  getNested(
    id = id,
    where = where,
    default = default,
    strict = strict,
    ...
  )    
    
  }
)

#' @title
#' Get Nested (char-env)
#'
#' @description 
#' See generic: \code{\link[nestr]{getNested}}
#'   	 
#' @inheritParams getNested
#' @param id \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{ANY}}. Option value or for non-existing component 
#'    (i.e. wrong \code{id}): \code{NULL} if \code{strict = FALSE} and an error
#'    if \code{strict = TRUE}.
#' @example inst/examples/getNested.r
#' @seealso \code{
#'    \link[nestr]{getNested}
#' }
#' @template author
#' @template references
#' @aliases getNested-char-miss-method
#' @import conditionr
#' @export
setMethod(
  f = "getNested", 
  signature = signature(
    id = "character",
    where = "environment"
  ), 
  definition = function(
    id,
    where,
    default,
    strict,
    ...
  ) {

  if (!length(id)) {
    if (!strict) {
      out <- NULL
    } else {
      conditionr::signalCondition(
        condition = "InvalidComponent",
        msg = c(
          Reason = "Empty ID"
        ),
        ns = "optionr",
        type = "error"
      )
    }
  } else {
    container <- where
    envir_name <- "container"
# print(where)    
# print(ls(where))    
# print(missing(default))    
#     if (missing(default)) {
#       path <- paste0("[[\"", gsub("/", "\"]][[\"", id), "\"]]")
#       expr <- paste0(envir_name, path)
#       out <- eval(parse(text = expr))  
#     } else {
      path <- if (grepl("^\\./", id) || dirname(id) != ".") {
        paste0("[[\"", gsub("/", "\"]][[\"", dirname(id)), "\"]]")
      }
      where <- eval(parse(text = paste0(envir_name, path)))
      if (  is.null(where) ||
            !exists(basename(id), envir = where, inherits = FALSE)) {
        out <- default
      } else {
        out <- get(basename(id), envir = where, inherits = FALSE)
      }
#     }

    if (is.null(out)) {
      if (!strict) {
        out <- out
      } else {
        conditionr::signalCondition(
          condition = "InvalidComponent",
          msg = c(
            Reason = "no such component",
            ID = id
          ),
          ns = "optionr",
          type = "error"
        )
      }
    }
  }
  
  return(out)
    
  }
)
