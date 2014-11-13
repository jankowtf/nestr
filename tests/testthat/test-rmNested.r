##------------------------------------------------------------------------------
context("rmNested")
##------------------------------------------------------------------------------

test_that("rmNested", {
  
  setNested(id = "a", value = TRUE)
  expect_true(rmNested(id = "a"))
  expect_false(exists("a", environment(), inherits = FALSE))
  
  setNested(id = "a/b/c", value = 10, gap = TRUE)
  expect_true(rmNested(id = "a/b/c"))
  expect_false(exists("c", environment()$a$b, inherits = FALSE))
  expect_true(rmNested(id = "a"))
  
})

##------------------------------------------------------------------------------
context("rmNested/where")
##------------------------------------------------------------------------------

test_that("rmNested/where", {

  where <- new.env()
  setNested(id = "a/b/c", value = 10, where = where, gap = TRUE)
  expect_true(rmNested(id = "a/b/c", where = where))
  expect_false(exists("c", where$a$b, inherits = FALSE))
  expect_true(rmNested(id = "where"))
  
})

##------------------------------------------------------------------------------
context("rmNested/strict")
##------------------------------------------------------------------------------

test_that("rmNested/strict", {

  expect_false(rmNested(id = "a"))
  expect_error(rmNested(id = "a", strict = TRUE))
  
  expect_false(rmNested(id = "a/b/c"))
  expect_error(rmNested(id = "a/b/c", strict = TRUE))
  
  expect_false(rmNested(id = character()))
  expect_error(rmNested(id = character(), strict = TRUE))
      
})
