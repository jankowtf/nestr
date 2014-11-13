#' @title
#' Remove Nested (generic)
#'
#' @description 
#' Removes a component from a nested object structure based on a path-like 
#' \code{id} with the last ID component being the name of the actual object 
#' being removed.
#' 
#' @template path-like-ids
#'   	
#' @param id \strong{Signature argument}.
#'    Object containing path-like name/ID information.
#' @param where \strong{Signature argument}.
#'    Object containing location information.  
#' @param strict \code{\link{logical}}. 
#'    \code{TRUE}: the following constellations trigger an error:
#'    \itemize{
#'        \item{\code{id} pointing to a non-existing component}
#'        \item{empty \code{id}}
#'    }
#'    \code{FALSE}: the stated constellations lead to the return value 
#'    being \code{FALSE}.
#' @template threedots
#' @example inst/examples/rmNested.r
#' @seealso \code{
#'   	\link[nestr]{rmNested-char-env-method},
#'     \link[nestr]{setNested},
#'     \link[nestr]{getNested}
#' }
#' @template author
#' @template references
#' @import devtools
#' @export 
setGeneric(
  name = "rmNested",
  signature = c(
    "id",
    "where"
  ),
  def = function(
    id,
    where = parent.frame(),
    strict = FALSE, 
    ...
  ) {
    standardGeneric("rmNested")       
  }
)

#' @title
#' Remove Nested (char-miss)
#'
#' @description 
#' See generic: \code{\link[nestr]{rmNested}}
#'      
#' @inheritParams rmNested
#' @param id \code{\link{character}}.
#' @param where \code{\link{missing}}.
#' @return See method 
#'    \code{\link[nestr]{rmNested-char-env-method}}
#' @example inst/examples/rmNested.r
#' @seealso \code{
#'    \link[nestr]{rmNested}
#' }
#' @template author
#' @template references
#' @aliases rmNested-char-miss-method
#' @export
setMethod(
  f = "rmNested", 
  signature = signature(
    id = "character",
    where = "missing"
  ), 
  definition = function(
    id,
    where,
    strict,
    ...
  ) {
 
  rmNested(
    id = id,
    where = where,
    strict = strict,
    ...
  )    
    
  }
)

#' @title
#' Remove Nested (char-env)
#'
#' @description 
#' See generic: \code{\link[nestr]{rmNested}}
#'   	 
#' @inheritParams rmNested
#' @param id \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{logical}}. 
#'    \code{TRUE}: removal successful;
#'    \code{FALSE}: removal failed.
#' @example inst/examples/rmNested.r
#' @seealso \code{
#'    \link[nestr]{rmNested}
#' }
#' @template author
#' @template references
#' @aliases rmNested-char-env-method
#' @import conditionr
#' @export
setMethod(
  f = "rmNested", 
  signature = signature(
    id = "character",
    where = "environment"
  ), 
  definition = function(
    id,
    where,
    strict,
    ...
  ) {

  if (!length(id)) {
    if (!strict) {
      out <- FALSE
    } else {
      conditionr::signalCondition(
        condition = "RemovalFailed",
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

    path <- if (grepl("^\\./", id) || dirname(id) != ".") {
      paste0("[[\"", gsub("/", "\"]][[\"", dirname(id)), "\"]]")
    }
    where <- eval(parse(text = paste0(envir_name, path)))
    if (  is.null(where) ||
          !exists(basename(id), envir = where, inherits = FALSE)) {
      out <- NULL
    } else {
      rm(list = basename(id), envir = where, inherits = FALSE)
      out <- TRUE
    }
    if (is.null(out)) {
      if (!strict) {
        out <- FALSE
      } else {
        conditionr::signalCondition(
          condition = "RemovalFailed",
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
