#' @title
#' Nested object structures
#'
#' @description
#' The package provides an extendable interface to conveniently 
#' create nested object structures based on using environments. 
#' 
#' Object values can be set and retrieved based on path-like names/identifiers 
#' (e.g. `output/print/type = "pdf"` will be translated into the following nested
#' environment structure: `output$print$type` with the value being`"pdf"`). 
#' 
#' Also, it allows to specify reactive nested object structures, i.e. objects 
#' that are dynamically linked to other objects and thus automatically stay synced. 
#' 
#' Furthermore, the package provides means to transform nested environment 
#' structures to nested lists and JSON objects.
#' 
#' @section Core functions:
#' 
#'  \itemize{
#'    \item{\code{\link[nestr]{setNested}}: }{
#'
#'      Creates a nested object structure based on a path-like \code{id} with the 
#' last ID component being the actual object name that \code{value} is assigned 
#' to.
#'    }
#'    \item{\code{\link[nestr]{getNested}}: }{
#'
#'      Retrieves value from a nested object structure based on a path-like 
#' \code{id} with the last ID component being the actual object name that contains
#' the value.
#'    }
#'    \item{\code{\link[nestr]{rmNested}}: }{
#'
#'      Removes a component from a nested object structure based on a path-like 
#' \code{id} with the last ID component being the name of the actual object 
#' being removed.
#'    }
#' }
#' 
#' @section Transformation functions:
#' 
#'  \itemize{
#'    \item{\code{\link[nestr]{fromList}}: }{
#'
#'      Creates a nested \code{environment} object structure based on a 
#'      \code{list} object structure.
#'    }
#'    \item{\code{\link[nestr]{toList}}: }{
#'
#'      Creates a nested \code{list} object structure based on an 
#'      \code{environment} object structure.
#'    }
#'    \item{\code{\link[nestr]{fromJson}}: }{
#'
#'      Creates a nested \code{environment} object structure based on a \code{JSON}
#'      object structure.
#'    }
#'    \item{\code{\link[nestr]{toJson}}: }{
#'
#'      Creates a nested \code{JSON} object structure based on an nested
#'      \code{environment} object structure.
#'    }
#' }
#'
#' @template author
#' @template references
#' @docType package
#' @name nestr
NULL
