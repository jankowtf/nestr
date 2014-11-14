#' @title
#' Get Class of Nested Object (generic)
#'
#' @description 
#' Retrieves the class of a component inside a nested object structure 
#' based on a path-like \code{id} with the last ID component being the actual 
#' object name that is looked up.
#' 
#' @template path-like-ids
#'   	
#' @param id \strong{Signature argument}.
#'    Object containing path-like name/ID information.
#' @param where \strong{Signature argument}.
#'    Object containing location information.  
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
#' @example inst/examples/getNestedClass.r
#' @seealso \code{
#'   	\link[nestr]{getNestedClass-char-env-method},
#'     \link[nestr]{setNested},
#'     \link[nestr]{getNested},
#'     \link[nestr]{existsNested},
#'     \link[nestr]{rmNested}
#' }
#' @template author
#' @template references
#' @export 
setGeneric(
  name = "getNestedClass",
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
    standardGeneric("getNestedClass")       
  }
)

#' @title
#' Get Nested (char-miss)
#'
#' @description 
#' See generic: \code{\link[nestr]{getNestedClass}}
#'      
#' @inheritParams getNestedClass
#' @param id \code{\link{character}}.
#' @param where \code{\link{missing}}.
#' @return See method 
#'    \code{\link[nestr]{getNestedClass-char-char-method}}
#' @example inst/examples/getNestedClass.r
#' @seealso \code{
#'    \link[nestr]{getNestedClass}
#' }
#' @template author
#' @template references
#' @aliases getNestedClass-char-miss-method
#' @export
setMethod(
  f = "getNestedClass", 
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
 
  getNestedClass(
    id = id,
    where = where,
    strict = strict,
    ...
  )    
    
  }
)

#' @title
#' Get Nested (char-env)
#'
#' @description 
#' See generic: \code{\link[nestr]{getNestedClass}}
#'   	 
#' @inheritParams getNestedClass
#' @param id \code{\link{character}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{character}}. Class value.
#' @example inst/examples/getNestedClass.r
#' @seealso \code{
#'    \link[nestr]{getNestedClass}
#' }
#' @template author
#' @template references
#' @aliases getNestedClass-char-miss-method
#' @import conditionr
#' @export
setMethod(
  f = "getNestedClass", 
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
    
  out <- character()
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
    out <- if ( !is.null(where) && 
                inherits(where, "environment") &&
                exists(basename(id), envir = where, inherits = FALSE)
    ) {
      class(get(basename(id), envir = where, inherits = FALSE))
    } else {
      character()
    }

    if (!length(out)) {
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
