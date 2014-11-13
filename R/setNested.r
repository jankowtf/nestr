#' @title
#' Set Nested (generic)
#'
#' @description 
#' Creates a nested object structure based on a path-like \code{id} with the 
#' last ID component being the actual object name that \code{value} is assigned 
#' to.
#' 
#' @template path-like-ids
#'   	
#' @param id \strong{Signature argument}.
#'    Object containing path-like ID information.
#' @param value \strong{Signature argument}.
#'    Object containing value information.
#' @param where \strong{Signature argument}.
#'    Object containing location information.  
#' @param force \code{\link{logical}}. 
#'    \code{TRUE}: when \code{dirname(id)} points to a \emph{leaf} instead of a 
#'    \emph{branch} (i.e. \code{dirname(id)} is not an \code{environment}), 
#'    overwrite it to turn it into a branch;
#'    \code{FALSE}: either return with \code{FALSE} or throw error in such cases
#'    (depending on value of \code{strict}); 
#' @param gap \code{\link{logical}}. 
#'    \code{TRUE}: when \code{dirname(id)} points to a non-existing parent
#'    branch or if there are any missing branches in the tree structure, 
#'    then auto-create all missing branches; 
#'    \code{FALSE}: either return with \code{FALSE} or throw error in such cases
#'    (depending on \code{strict}); 
#'    Default: \code{TRUE} as this seems to be most practical/convenient for 
#'    actual applications.
#' @param must_exist \code{\link{logical}}. 
#'    \code{TRUE}: \code{id} pointing to a non-existing object value either triggers
#'    an error or results in return value \code{FALSE} (depending on \code{strict}); 
#'    \code{FALSE}: object value that \code{id} points to is set.
#' @param reactive \code{\link{logical}}. 
#'    \code{TRUE}: set reactive object value via 
#'    \code{\link[nestr]{setReactive}} or \code{\link[nestr]{setShinyReactive}}.
#'    \code{FALSE}: set regular/non-reactive object value.
#'    Note that if \code{value = reactiveExpression()}, \code{reactive} is 
#'    automatically set to \code{TRUE}.
#' @param strict \code{\link{logical}}. 
#'    \code{TRUE}: \code{id} pointing to a non-existing object value triggers
#'    error; \code{FALSE}: \code{id} pointing to a non-existing object value leads
#'    to return value \code{NULL}.
#' @param typed \code{\link{logical}}. 
#'    Implies that \code{must_exist} is automatically set to \code{TRUE}.
#'    \code{TRUE}: \code{class(value)} must match the class of the existing 
#'    object value; 
#'    \code{FALSE}: object value that \code{id} points to is set without class check.
#' @param Further arguments to be passed along to subsequent functions.
#'    In particular: 
#'    \itemize{
#'      \item{\code{\link[nestr]{setShinyReactive}}}
#'      \item{\code{\link[typr]{setTyped}}}
#'    }
#' @example inst/examples/setNested.r
#' @seealso \code{
#'   	\link[nestr]{setNested-char-any-char-method},
#'     \link[nestr]{getNested},
#'     \link[nestr]{rmNested}
#' }
#' @template author
#' @template references
#' @export 
setGeneric(
  name = "setNested",
  signature = c(
    "id",
    "value",
    "where"
  ),
  def = function(
    id,
    value,
    where = parent.frame(),
    force = FALSE,
    gap = TRUE,
    must_exist = FALSE, 
    reactive = FALSE,
    strict = c(0, 1, 2),
    typed = FALSE,
    ...
  ) {
    standardGeneric("setNested")       
  }
)

#' @title
#' Set Nested (char-any-miss)
#'
#' @description 
#' See generic: \code{\link[nestr]{setNested}}
#'      
#' @inheritParams setNested
#' @param id \code{\link{character}}.
#' @param value \code{\link{ANY}}.
#' @param where \code{\link{missing}}.
#' @return See method
#'    \code{\link{setNested-char-any-char-method}}.
#' @example inst/examples/setNested.r
#' @seealso \code{
#'    \link[nestr]{setNested}
#' }
#' @template author
#' @template references
#' @aliases setNested-char-any-miss-method
#' @export
setMethod(
  f = "setNested", 
  signature = signature(
    id = "character",
    value = "ANY",
    where = "missing"
  ), 
  definition = function(
    id,
    value,
    where,
    force,
    gap,
    must_exist,
    reactive,
    strict,
    typed,
    ...
  ) {
    
  setNested(
    id = id,
    value = value,
    where = where,
    force = force,
    gap = gap,
    must_exist = must_exist,
    reactive = reactive,
    strict = strict,
    typed = typed,
    ...
  )    
    
  }
)

#' @title
#' Set Nested (char-any-env)
#'
#' @description 
#' See generic: \code{\link[nestr]{setNested}}
#'   	 
#' @inheritParams setNested
#' @param id \code{\link{character}}.
#' @param value \code{\link{ANY}}.
#' @param where \code{\link{environment}}.
#' @return \code{\link{logical}}. \code{TRUE}.
#' @example inst/examples/setNested.r
#' @seealso \code{
#'    \link[nestr]{setNested}
#' }
#' @template author
#' @template references
#' @aliases setNested-char-any-char-method
#' @import reactr
#' @import typr
#' @export
setMethod(
  f = "setNested", 
  signature = signature(
    id = "character",
    value = "ANY",
    where = "environment"
  ), 
  definition = function(
    id,
    value,
    where,
    force,
    gap,
    must_exist,
    reactive,
    strict,
    typed,
    ...
  ) {
    
  ## Argument checks //
  strict <- as.numeric(match.arg(as.character(strict), 
    as.character(c(0, 1, 2))))    
    
  out <- TRUE
  container <- where
  envir_name <- "container"
  
  if (inherits(value, "ReactiveExpression")) {
    reactive <- TRUE
  }
  
  ## Direct parent check //
  id_branch <- dirname(id)
  if (!grepl("^\\./", id) && id_branch == ".") {
    branch_value <- container
  } else {
    branch_value <- tryCatch(
      getNested(id = id_branch, where = where, strict = FALSE),
      error = function(cond) {
        NULL
      }
    )
  }

  ## Handling branch gaps //
  if (is.null(branch_value)) {
    if (gap) {
      ## Check how much to fill //
      id_branch_spl <- unlist(strsplit(id_branch, split = "/"))
      id_branch_tree <- NULL
      expr_get <- NULL
      expr_set <- NULL
      for (ii in 1:length(id_branch_spl)) {
        expr_get <- c(expr_get, 
          paste0(envir_name, "[[\"", paste(id_branch_spl[1:ii], collapse = "\"]][[\""),
             "\"]]"))
        expr_set <- c(expr_set, 
          paste0(envir_name, "[[\"", paste(id_branch_spl[1:ii], collapse = "\"]][[\""),
          "\"]] <- new.env()"))
        id_branch_tree <- c(id_branch_tree, paste(id_branch_spl[1:ii], collapse = "/"))
      }
      
      ## Determine component types //
      ## * yes --> branch
      ## * no --> leaf or not existing
      ## * error --> error
      idx <- sapply(expr_get, function(ii) {
        tryCatch({
          tmp <- switch(
            as.character(inherits(eval(parse(text = ii)), "environment")),
            "TRUE" = "yes",
            "FALSE" = "no"
          )},
          error = function(cond) {
            "error"
          }
        )
      }) 

      ## Invalid branch(es) //
      if (any(idx == "no") & any(idx == "error")) {
        idx_no <- which(idx == "no")
        if (length(idx_no)) {
          if (force) {
          ## Ensure that leafs are transformed to branches //            
            setNested(
              id = id_branch_tree[idx_no],
              value = new.env(),
              where = where
            )
            
            ## Update `idx` and `expr_set` //
            idx <- idx[-idx_no]
            expr_set <- expr_set[-idx_no]
            
            ## Remove error entry //
            idx[which(idx == "error")] <- "no"
          } else {
            if (strict == 0) {
              out <- FALSE
            } else if (strict == 1) {
              conditionr::signalCondition(
                condition = "InvalidBranchConstellation",
                msg = c(
                  Reason = "parent branch is not an environment",
                  ID = id,
                  "ID branch" = id_branch_tree[idx_no]
                ),
                ns = "nestr",
                type = "warning"
              )
              out <- FALSE
            } else if (strict == 2) {
              conditionr::signalCondition(
                condition = "InvalidBranchConstellation",
                msg = c(
                  Reason = "parent branch is not an environment",
                  ID = id,
                  "ID branch" = id_branch_tree[idx_no]
                ),
                ns = "nestr",
                type = "error"
              )  
            }
          }
        }
      }
      
      ## Gap not-yet-existing branch(es) //
      idx_no <- which(idx == "no")
      if (out) {
        if (length(idx_no)) {
          run_scope <- idx_no[1]:length(expr_set)
#         } else {
#           run_scope <- 1:length(expr_set)
#         }
        
#         if (length(run_scope)) {
          sapply(run_scope, function(ii) {
            eval(parse(text = expr_set[ii]))
          })  
          branch_value <- getNested(id = id_branch, 
            where = where, strict = FALSE)
        }
      }
    } else {
      if (strict == 0) {
        out <- FALSE
      } else if (strict == 1) {
        conditionr::signalCondition(
          condition = "InvalidBranchConstellation",
          msg = c(
            Reason = "branch gap",
            ID = id
          ),
          ns = "nestr",
          type = "warning"
        )
        out <- FALSE
      } else if (strict == 2) {
        conditionr::signalCondition(
          condition = "InvalidBranchConstellation",
          msg = c(
            Reason = "branch gap",
            ID = id
          ),
          ns = "nestr",
          type = "error"
        )
      }
    }
  }

  ## Early exit //
  if (!out) {
    return(out)
  }

  ## Parent branch is no environment //
  if (!inherits(branch_value, "environment")) {
    if (force) {
    ## Transform to branch //
      expr_set <- paste0(envir_name, "$", gsub("/", 
        "$", id_branch), " <- new.env()")
      eval(parse(text = expr_set))
    } else {
      if (strict == 0) {
        out <- FALSE
      } else if (strict == 1) {
        conditionr::signalCondition(
          condition = "InvalidBranchConstellation",
          msg = c(
            Reason = "parent branch is not an environment",
            ID = id,
            "ID branch" = id_branch,
            "Class branch" = class(branch_value)
          ),
          ns = "nestr",
          type = "warning"
        )
        out <- FALSE
      } else if (strict == 2) {
        conditionr::signalCondition(
          condition = "InvalidBranchConstellation",
          msg = c(
            Reason = "parent branch is not an environment",
            ID = id,
            "ID branch" = id_branch,
            "Class branch" = class(branch_value)
          ),
          ns = "nestr",
          type = "error"
        )
      }
    }
  }

  ## Early exit //
  if (!out) {
    return(out)
  }

  ## Must exist //
  if (must_exist) {
    if (!exists(basename(id), envir = branch_value, inherits = FALSE)) {
      if (strict == 0) {
        out <- FALSE
      } else if (strict == 1) {
        conditionr::signalCondition(
          condition = "StructurePrerequisitesNotMet",
          msg = c(
            Reason = "leaf does not exist yet",
            ID = id
          ),
          ns = "nestr",
          type = "warning"
        )
        out <- FALSE
      } else if (strict == 2) {
        conditionr::signalCondition(
          condition = "StructurePrerequisitesNotMet",
          msg = c(
            Reason = "leaf does not exist yet",
            ID = id
          ),
          ns = "nestr",
          type = "error"
        )
      }
    }
  }

  ## Early exit //
  if (!out) {
    return(out)
  }

  ## Auto-check if reactive //
  ## This significantly speeds up the assignment process for reactive *sources* 
  ## that already exist as `setShinyReactive()` does not need to be called again
  path <- if (grepl("^\\./", id) || dirname(id) != ".") {
    paste0("[[\"", gsub("/", "\"]][[\"", dirname(id)), "\"]]")
  }
  expr <- paste0(envir_name, path)
# print(expr)
  where <- eval(parse(text = expr))
# print(where)
  reactive_exist <- isReactive(id = basename(id), where = where)
  
  ## This takes care that reactive observers will always updated when this 
  ## function is run:
  is_reactive_value <- inherits(value, "ReactiveExpression")
  
  if (!reactive || reactive_exist && !typed && !is_reactive_value) {  
    if (!typed) {
      path <- paste0("[[\"", gsub("/", "\"]][[\"", id), "\"]]")
      expr <- paste0(envir_name, path, " <- value")
      eval(parse(text = expr))  
    } else {
      setTyped(id = basename(id), value = value, where = where, 
               strict = strict, ...)
    }
  } else {
    setShinyReactive(id = basename(id), value = value, 
      where = where, typed = typed, ...)
  }

  return(out)
  
  }
)
