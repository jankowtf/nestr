##------------------------------------------------------------------------------
context("fromList/parent frame")
##------------------------------------------------------------------------------

test_that(desc="fromList/parent frame", {
  
  input <- list(
    europe = list(germany = list(berlin = 1, hamburg = 2, munich = 3)),
    america = list(usa = list(wisconsin = list(madison = 1))),
    south.america = 1,
    as.list(1:3)
  )
  expect_is(res <- fromList(input=input), "environment")
  expect_true(all(c("[4]", "america", "europe", "south.america") %in% ls(res)))
  
  expect_is(america, "environment")
  expect_identical(ls(america), "usa")
  
  expect_is(europe, "environment")
  expect_identical(ls(europe), "germany")
  
  expect_is(europe$germany, "environment")
  expect_identical(ls(europe$germany), c("berlin", "hamburg", "munich"))
  
  expect_is(environment()[["[4]"]], "environment")  
  expect_identical(ls(environment()[["[4]"]]), c("[1]", "[2]", "[3]"))
  
})

##------------------------------------------------------------------------------
context("fromList/custom envir")
##------------------------------------------------------------------------------

test_that(desc="fromList/custom envir", {
  
  input <- list(
    europe = list(germany = list(berlin = 1, hamburg = 2, munich = 3)),
    america = list(usa = list(wisconsin = list(madison = 1))),
    south.america = 1,
    as.list(1:3)
  )
  
  where <- new.env()
  expect_is(fromList(input = input, where = where), "environment")
  expect_true(all(c("[4]", "america", "europe", "south.america") %in% ls(where)))
  
  expect_is(where$america, "environment")
  expect_identical(ls(where$america), "usa")
  
  expect_is(where$europe, "environment")
  expect_identical(ls(where$europe), "germany")
  
  expect_is(where$europe$germany, "environment")
  expect_identical(ls(where$europe$germany), c("berlin", "hamburg", "munich"))
  
  expect_is(where$"[4]", "environment")  
  expect_identical(ls(where$"[4]"), c("[1]", "[2]", "[3]"))
  
})
