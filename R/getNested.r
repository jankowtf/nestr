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
#'     Controls what happens when \code{id} points to a non-existing component:
#'    \itemize{
#' 			\item{0: }{ignore and return \code{FALSE} to signal that the 
#' 				assignment process was not successful or \code{fail_value} depending
#' 				on the value of \code{return_status}} 
#' 			\item{1: }{ignore and with warning and return \code{FALSE}}
#' 			\item{2: }{ignore and with error}
#'   	}
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
    strict = c(0, 1, 2), 
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
#' @return \code{\link{ANY}}. Component value or for invalid argument input 
#' 		and non-existing component the value of \code{default} unless 
#' 		\code{strict == 1} in which case a warning is issued or
#' 		\code{strict == 2} in which case an error is thrown. 
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

  ## Argument checks //
  strict <- as.numeric(match.arg(as.character(strict), 
    as.character(c(0, 1, 2))))   
    
  out <- default
  if (!length(id)) {
    if (strict == 1) {
      conditionr::signalCondition(
        condition = "InvalidComponent",
        msg = c(
          Reason = "Empty ID"
        ),
        ns = "optionr",
        type = "warning"
      )
    } else if (strict == 2) {
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
    path <- if (grepl("^\\./", id) || dirname(id) != ".") {
      paste0("[[\"", gsub("/", "\"]][[\"", dirname(id)), "\"]]")
    }
    where <- eval(parse(text = paste0("where", path)))

    out <- if (inherits(where, "environment")) {
      out <- if ( is.null(where)|| 
                  !exists(basename(id), envir = where, inherits = FALSE)
      ) {
        default
      } else {
        get(basename(id), envir = where, inherits = FALSE)
      }
    } else {
      default
    }
    
    if (identical(out, default)) {
      if (strict == 1) {
        conditionr::signalCondition(
          condition = "InvalidComponent",
          msg = c(
            Reason = "no such component",
            ID = id
          ),
          ns = "optionr",
          type = "warning"
        )
      } else if (strict == 2) {
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
