\dontrun{

##------------------------------------------------------------------------------
## Default `where` //
##------------------------------------------------------------------------------

setNested(id = "a", value = TRUE)
exists("a", environment(), inherits = FALSE)
rmNested(id = "a")
exists("a", environment(), inherits = FALSE)

setNested(id = "a/b/c", value = 10, gap = TRUE)
exists("c", environment()$a$b, inherits = FALSE)
rmNested(id = "a/b/c")
exists("c", environment()$a$b, inherits = FALSE)

## Clean up //
rm(a)

##------------------------------------------------------------------------------
## Different `where` // 
##------------------------------------------------------------------------------

where <- new.env()
setNested(id = "a/b/c", value = 10, where = where, gap = TRUE)
rmNested(id = "a/b/c", where = where)
exists("c", where$a$b, inherits = FALSE)

## Clean up //
rm(where)
  
##------------------------------------------------------------------------------
## Strictness //
##------------------------------------------------------------------------------

rmNested(id = "a")
try(rmNested(id = "a", strict = 1))
try(rmNested(id = "a", strict = 2))

rmNested(id = "a/b/c")
try(rmNested(id = "a/b/c", strict = 1))
try(rmNested(id = "a/b/c", strict = 2))

rmNested(id = character()))
try(rmNested(id = character(), strict = 1))
try(rmNested(id = character(), strict = 2))
    
}
