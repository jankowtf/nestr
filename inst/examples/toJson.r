\dontrun{

##------------------------------------------------------------------------------  
## Names //
##------------------------------------------------------------------------------

input <- new.env()
setNested("europe/germany/berlin", value = 1, where = input, gap = TRUE)
setNested("europe/germany/hamburg", value = 2, where = input)
setNested("europe/germany/munich", value = 3, where = input)
setNested("america/usa/wisconsin/madison", value = 1, where = input, gap = TRUE)
setNested("south.america", value = 1, where = input)

toJson(input = input)
res <- fromJson(toJson(input), where = new.env())
ls(res)

##------------------------------------------------------------------------------  
## No names //
##------------------------------------------------------------------------------

input <- new.env()
setNested("[1]/id", value = 1, where = input, gap = TRUE)
setNested("[1]/name", value = "abc", where = input, gap = TRUE)
setNested("[2]/id", value = "2", where = input, gap = TRUE)
setNested("[2]/name", value = "def", where = input, gap = TRUE)
setNested("[2]/address", value = "asdfasdf", where = input, gap = TRUE)

toJson(input)
res <- fromJson(toJson(input), where = new.env())
ls(res)

##------------------------------------------------------------------------------  
## Mixed //
##------------------------------------------------------------------------------

input <- new.env()
setNested("[1]/id", value = 1, where = input, gap = TRUE)
setNested("[1]/name", value = "abc", where = input, gap = TRUE)
setNested("[2]/id", value = "2", where = input, gap = TRUE)
setNested("[2]/name", value = "def", where = input, gap = TRUE)
setNested("[2]/address", value = "asdfasdf", where = input, gap = TRUE)
setNested("john_doe/id", value = "2", where = input, gap = TRUE)
setNested("john_doe/name", value = "john doe", where = input, gap = TRUE)
setNested("john_doe/address", value = "asdfasdf", where = input, gap = TRUE)

toJson(input)
res <- fromJson(toJson(input), where = new.env())
ls(res)

}
