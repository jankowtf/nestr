\dontrun{

##------------------------------------------------------------------------------
## Basics //
##------------------------------------------------------------------------------

setNested(id = "test", value = TRUE)
getNestedClass(id = "test")

setNested(id = "a/b/c", value = 10)
getNestedClass(id = "a")
getNestedClass(id = "a/b")
getNestedClass(id = "a/b/c")
getNestedClass(id = "a/b/c/d")

getNestedClass(id = "c")
getNestedClass(id = "c/d/e")
  
##------------------------------------------------------------------------------
## Strictness levels //
##------------------------------------------------------------------------------

## Empty ID //
getNestedClass(id = character())
try(getNestedClass(id = character(), strict = 1))
try(getNestedClass(id = character(), strict = 2))

## Not-existing //  
getNestedClass(id = "c/d/e")
try(getNestedClass(id = "c/d/e", strict = 1))
try(getNestedClass(id = "c/d/e", strict = 2))

##------------------------------------------------------------------------------
## Explicit `where` //
##------------------------------------------------------------------------------

where <- new.env()
setNested(id = "a/b/c", value = 10, where = where)
getNestedClass(id = "a/b/c", where = where)
getNestedClass(id = "a/b/c/d", where = where)
getNestedClass(id = "c/d/e", where = where)

}
