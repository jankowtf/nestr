\dontrun{

##------------------------------------------------------------------------------
## In parent frame //
##------------------------------------------------------------------------------  

input <- list(
  europe = list(germany = list(berlin = 1, hamburg = 2, munich = 3)),
  america = list(usa = list(wisconsin = list(madison = 1))),
  south.america = 1,
  as.list(1:3)
)

res <- fromList(input = input)
res
ls(res)
ls(europe)
ls(europe$germany)
getNested("europe/germany/berlin")

ls(res$"[4]")
getNested("[4]/[1]")
getNested("[4]/[2]")
getNested("[4]/[3]")

##------------------------------------------------------------------------------
## In custom environment //
##------------------------------------------------------------------------------  

input <- list(
  europe = list(germany = list(berlin = 1, hamburg = 2, munich = 3)),
  america = list(usa = list(wisconsin = list(madison = 1))),
  south.america = 1,
  as.list(1:3)
)

where <- new.env()
res <- fromList(input = input, where = where)
identical(res, where)

## A bit more convenient //
where <- fromList(input = input, where = new.env())

ls(where$europe)
ls(where$europe$germany)
getNested("where/europe/germany/berlin")
getNested("europe/germany/berlin", where = where)

ls(where$"[4]")
getNested("[4]/[1]", where = where)
getNested("[4]/[2]", where = where)
getNested("[4]/[3]", where = where)

}
