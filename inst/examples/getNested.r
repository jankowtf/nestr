\dontrun{

## Also see examples at `?setNested`
  
##------------------------------------------------------------------------------
## Basics //
##------------------------------------------------------------------------------

setNested(id = "test", value = TRUE)
getNested(id = "test")
## --> leaf

setNested(id = "a/b/c", value = 10, gap = TRUE)
ls(getNested(id = "a"))
## --> branch
ls(getNested(id = "a/b"))
## --> branch
getNested(id = "a/b/c")
## --> leaf
  
## Clean up //
rm(test)
rm(a)

##------------------------------------------------------------------------------
## Explicit `where` //
##------------------------------------------------------------------------------

where <- new.env()
setNested(id = "a/b/c", value = 10, where = where, gap = TRUE)
getNested(id = "a/b/c", where = where)

## Clean up //
rm(where)

##------------------------------------------------------------------------------
## Different `default` //
##------------------------------------------------------------------------------

setNested(id = "test", value = 10)
getNested(id = "test", default = NA)
getNested(id = "z", default = NA)
getNested(id = "z/a/b", default = NA)

getNested(id = "test", default = "nope")
getNested(id = "z", default = "nope")
getNested(id = "z/a/b", default = "nope")

## Clean up //
rm(test)

}
