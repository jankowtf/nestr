##------------------------------------------------------------------------------
context("toJson/custom envir")
##------------------------------------------------------------------------------

test_that(desc="toJson/custom envir", {
  
  where <- new.env()
  setNested("europe/germany/berlin", value = 1, where = where, gap = TRUE)
  setNested("europe/germany/hamburg", value = 2, where = where)
  setNested("europe/germany/munich", value = 3, where = where)
  setNested("america/usa/wisconsin/madison", value = 1, where = where, gap = TRUE)
  setNested("south.america", value = 1, where = where)
  
  expected <- list(
    europe = list(germany = list(berlin = 1, hamburg = 2, munich = 3)),
    america = list(usa = list(wisconsin = list(madison = 1))),
    south.america = 1
  )
  expect_is(res <- toJson(input = where), "json")
  
})
