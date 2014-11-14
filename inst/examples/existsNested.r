\dontrun{

##------------------------------------------------------------------------------
## Basics //
##------------------------------------------------------------------------------

setNested(id = "test", value = TRUE)
existsNested(id = "test")

setNested(id = "a/b/c", value = 10)
existsNested(id = "a")
existsNested(id = "a/b")
existsNested(id = "a/b/c")
existsNested(id = "a/b/c/d")

existsNested(id = "c")
existsNested(id = "c/d/e")
  
##------------------------------------------------------------------------------
## Strictness levels //
##------------------------------------------------------------------------------

## Empty ID //
existsNested(id = character())
try(existsNested(id = character(), strict = 1))
try(existsNested(id = character(), strict = 2))

## Not-existing //  
existsNested(id = "c/d/e")
try(existsNested(id = "c/d/e", strict = 1))
try(existsNested(id = "c/d/e", strict = 2))

##------------------------------------------------------------------------------
## Explicit `where` //
##------------------------------------------------------------------------------

where <- new.env()
setNested(id = "a/b/c", value = 10, where = where)
existsNested(id = "a/b/c", where = where)
existsNested(id = "a/b/c/d", where = where)
existsNested(id = "c/d/e", where = where)

}
