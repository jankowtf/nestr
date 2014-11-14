#' @title
#' Exists Nested (generic)
#'
#' @description 
#' Checks if a component exists inside of a nested object structure 
#' based on a path-like \code{id} with the last ID component being the 
#' actual object name that the function looks for.
#' 
#' @template path-like-ids
#'   	
#' @param id \strong{Signature argument}.
#'    Object containing path-like name/ID information.
#' @param where \strong{Signature argument}.
#'    Object containing location information.  
#' @param strict \code{\link{logical}}.
#'   	Controls what happens when \code{id} points to a non-existing component:
#'    \itemize{
#' 			\item{0: }{ignore and return \code{FALSE} to signal that the 
#' 				assignment process was not successful or \code{fail_value} depending
#' 				on the value of \code{return_status}} 
#' 			\item{1: }{ignore and with warning and return \code{FALSE}}
#' 			\item{2: }{ignore and with error}
#'   	}
#' @template threedots
#' @example inst/examples/existsNested.r
#' @seealso \code{
#'   	\link[nestr]{existsNested-char-env-method},
#'     \link[nestr]{setNested},
#'     \link[nestr]{getNested},
#'     \link[nestr]{rmNested}
#' }
#' @template author
#' @template references
#' @export 
setGeneric(
  name = "existsNested",
  signature = c(
    "id",
    "where"
  ),
  def = function(
    id,
    where = parent.frame(),
    strict = c(0, 1, 2), 
    ...
  ) {
    standardGeneric("existsNested")       
  }
)

#' @title
#' Exists Nested (char-miss)
#'
#' @description 
#' See generic: \code{\link[nestr]{existsNested}}
#'      
#' @inheritParams existsNested
#' @param id \code{\link{character}}.
#' @param where \code{\link{missing}}.
#' @return See method 
#'    \code{\link[nestr]{existsNested-char-char-method}}
#' @example inst/examples/existsNested.r
#' @seealso \code{
#'    \link[nestr]{existsNested}
#' }
#' @template author
#' @template references
#' @aliases existsNested-char-miss-method
#' @export
setMethod(
  f = "existsNested", 
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
 
  existsNested(
    id = id,
    where = where,
    strict = strict,
    ...
  )    
    
  }
)

#' @title
#' Exists Nested (char-env)
#'
#' @description 
#' See generic: \code{\link[nestr]{existsNested}}
#'   	 
#' @inheritParams existsNested
#' @param id \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{logical}}. 
#' 		\code{TRUE}: component exists;
#' 		\code{TRUE}: component does not exist;
#' @example inst/examples/existsNested.r
#' @seealso \code{
#'    \link[nestr]{existsNested}
#' }
#' @template author
#' @template references
#' @aliases existsNested-char-miss-method
#' @import conditionr
#' @export
setMethod(
  f = "existsNested", 
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
    
  ## Argument checks //
  strict <- as.numeric(match.arg(as.character(strict), 
    as.character(c(0, 1, 2))))     

  out <- FALSE
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
    out <- if (!is.null(where) && inherits(where, "environment")) {
      exists(basename(id), envir = where, inherits = FALSE)
    } else {
      FALSE
    }

    if (!out) {
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
