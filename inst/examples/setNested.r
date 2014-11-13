\dontrun{

##------------------------------------------------------------------------------
## Basics //
##------------------------------------------------------------------------------

## Simple name/ID //
setNested(id = "test", value = TRUE)
getNested(id = "test")

## Path-like name/ID //
setNested(id = "test/a", value = TRUE, strict = TRUE)
## --> note that currently `test` is a leaf, not a branch
setNested(id = "test/a", value = TRUE, force = TRUE)
## --> `test` needs to be transformed from a "leaf"
## to a "branch" component (i.e. an environment); `force = TRUE` 
## takes care of that

getNested(id = "test")
## --> branch
ls(getNested(id = "test"))
getNested(id = "test/a")
## --> leaf

## Must exist //
setNested(id = "test/b", value = TRUE, must_exist = TRUE)
try(setNested(id = "test/b", value = TRUE, must_exist = TRUE, strict = TRUE))

## Typed //
setNested(id = "test/c", value = "hello world!", typed = TRUE, gap = TRUE)
setNested(id = "test/c", value = 1:3)
## --> wrong class, but `strict_set = 0` --> disregarded without warning or error
getNested(id = "test/c")
## --> still `hello world!` because `value = 1:3` had wrong class

setNested(id = "test/c", value = "hello world!", typed = TRUE, strict_set = 1)
try(setNested(id = "test/c", value = 1:3))
## --> warning and no assignment
getNested(id = "test/c")
## --> still `hello world!`

setNested(id = "test/c", value = "hello world!", typed = TRUE, strict_set = 2)
try(setNested(id = "test/c", value = 1:3))
## --> error
getNested(id = "test/c")
## --> still `hello world!`

setNested(id = "test/a", value = "something else")
## --> correct class --> value changed 
getNested(id = "test/a")
  
## Clean up //
rm(test)

##------------------------------------------------------------------------------
## Different `where` //
##------------------------------------------------------------------------------

where <- new.env()
setNested(id = "a/b/c", value = 10, where = where, gap = TRUE)
getNested(id = "a/b/c", where = where)
identical(getNested(id = "a/b/c", where = where), where$a$b$c)

## Clean up //
rm(where)

##------------------------------------------------------------------------------
## Numerical names/IDs //
##------------------------------------------------------------------------------

setNested(id = "20140101", value = TRUE)
"20140101" %in% ls(all.names = TRUE)
getNested(id = "20140101")

## Clean up //
rm("20140101")

##------------------------------------------------------------------------------
## Branch gaps //
##------------------------------------------------------------------------------
  
setNested(id = "a/b/c/d", value = TRUE)
try(setNested(id = "a/b/c/d", value = TRUE, strict = TRUE))
## --> branch gap: branches a, b and c do not exist yet

## Closing the gap //
setNested(id = "a/b/c/d", value = TRUE, gap = TRUE)

## Inspect //
ls()
ls(getNested(id = "a"))
ls(getNested(id = "a/b"))
ls(getNested(id = "a/b/c"))
getNested(id = "a/b/c/d")

## Clean up //
rm(a)

##------------------------------------------------------------------------------
## Forcing leafs to branches //
##------------------------------------------------------------------------------
  
setNested(id = "a", value = "hello world!")
setNested(id = "a/b", value = 10, gap = TRUE)
try(setNested(id = "a/b", value = 10, gap = TRUE, strict = TRUE))
## --> root branch `a` is not an environment but a leaf:
getNested(id = "a")

## Forcing leaf into a branch //
setNested(id = "a/b", value = 10, force = TRUE)
ls(getNested(id = "a"))
## --> branch 
getNested(id = "a/b")
## --> leaf

## Clean up //
rm(a)

##------------------------------------------------------------------------------
## Reactive object values //
##------------------------------------------------------------------------------

setNested(id = "dirs/root", value = getwd(), reactive = TRUE, gap = TRUE)
setNested(
  id = "dirs/my_dir", 
  value = reactiveExpression(
    file.path(getNested(id = "dirs/root", where = parent.frame(7)), "my_dir")
  )
)
## --> `dirs/my_dir` should always dependent on of `dirs/root`
## --> note that you can ommit `reactive = TRUE` when `value = reactiveExpression(...)`
## --> Issue #1: need to specify `where = parent.frame(7)`

getNested(id = "dirs/root")
getNested(id = "dirs/my_dir")

## Changing via `setNested()` //
setNested(id = "dirs/root", value = tempdir())
getNested(id = "dirs/root")
getNested(id = "dirs/my_dir")

## When changed manually //
dirs$root <- "c:/temp"
dirs$root
dirs$my_dir

## Trying to change reactive observer //
setNested(id = "dirs/my_dir", value = TRUE)
getNested(id = "dirs/my_dir")
## --> has no effect; warning and error behavior can be 
## controlled via `strict_set`

## Clean up //
rm(dirs)

}

