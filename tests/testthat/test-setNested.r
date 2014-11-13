##------------------------------------------------------------------------------
context("setNested/basics")
##------------------------------------------------------------------------------

test_that("setNested/basics", {

  expect_true(setNested(id = "test", value = TRUE))
  expect_equal(getNested(id = "test"), TRUE)
  expect_true(setNested(id = "test", value = new.env()))
  expect_true(setNested(id = "test/a", value = TRUE))
  expect_equal(getNested(id = "test/a"), TRUE)
  expect_false(setNested(
    id = "test/b", 
    value = TRUE,
    must_exist = TRUE
  ))
  expect_error(setNested(
    id = "test/b", 
    value = TRUE,
    must_exist = TRUE,
    strict = TRUE
  ))
  
})

##------------------------------------------------------------------------------
context("setNested/typed")
##------------------------------------------------------------------------------

test_that("setNested/typed", {
  
  ## Strict = 0 //
  expect_true(setNested(
    id = "test/a", 
    value = "hello world!",
    gap = TRUE,
    typed = TRUE
  ))
  expect_true(setNested(id = "test/a", value = 10))
  expect_identical(getNested("test/a"), "hello world!")
  
  ## Strict = 1 //
  expect_true(setNested(
    id = "test/a", 
    value = "hello world!",
    typed = TRUE,
    strict_set = 1
  ))
  expect_warning(setNested(id = "test/a", value = 10))
  expect_identical(getNested("test/a"), "hello world!")
  
  ## Strict = 2 //
  expect_true(setNested(
    id = "test/a", 
    value = "hello world!",
    typed = TRUE,
    strict_set = 2
  ))
  expect_error(setNested(id = "test/a", value = 10))
  expect_identical(getNested("test/a"), "hello world!")
  
  expect_true(setNested(
    id = "test/a", 
    value = "something else"
  ))
  expect_equal(getNested(id = "test/a"), "something else")
  
})

##------------------------------------------------------------------------------
context("setNested/numerical names/IDs")
##------------------------------------------------------------------------------

test_that("setNested/numerical names", {

  expect_true(setNested(id = "20140101", value = TRUE))
  expect_equal(getNested(id = "20140101"), TRUE)
  
})

##------------------------------------------------------------------------------
context("setNested/gap")
##------------------------------------------------------------------------------

test_that("setNested/gap", {

  expect_false(setNested(id = "a/b/c/d", value = TRUE))
  expect_error(setNested(id = "a/b/c/d", value = TRUE, strict = TRUE))
  expect_true(setNested(id = "a/b/c/d", value = TRUE, gap = TRUE))
  expect_equal(getNested(id = "a/b/c/d"), TRUE)
  
})

##------------------------------------------------------------------------------
context("setNested/force")
##------------------------------------------------------------------------------

test_that("setNested/force 1", {
  
  expect_true(setNested(id = "a", value = "hello world!"))
  expect_false(setNested(id = "a/b/c/d", value = TRUE, gap = TRUE))
  expect_error(setNested(id = "a/b/c/d", value = TRUE, 
     gap = TRUE, strict = TRUE))
  expect_true(setNested(id = "a/b/c/d", value = TRUE, 
     gap = TRUE, force = TRUE))
  expect_equal(getNested(id = "a/b/c/d"), TRUE)
  
})

test_that("setNested/force 2", {
  
  expect_true(setNested(id = "a", value = "hello world!"))
  expect_false(setNested(id = "a/b", value = TRUE, gap = TRUE))
  expect_error(setNested(id = "a/b", value = TRUE, 
     gap = TRUE, strict = TRUE))
  expect_true(setNested(id = "a/b", value = TRUE, force = TRUE))
  expect_equal(getNested(id = "a/b"), TRUE)
   
})

##------------------------------------------------------------------------------
context("setNested/where")
##------------------------------------------------------------------------------

test_that("setNested/where", {

  where <- new.env()
  expect_true(setNested(id = "a/b/c", value = 10, where = where, gap = TRUE))
  expect_equal(getNested(id = "a/b/c", where = where), 10)
  expect_true(exists("a", where))
  expect_true(exists("b", where$a))
  expect_true(exists("c", where$a$b))
  expect_identical(where$a$b$c, getNested("a/b/c", where))
  
})

##------------------------------------------------------------------------------
context("setNested/reactive/atomic/reactr::reactiveExpression")
##------------------------------------------------------------------------------

test_that("setNested/reactive/atomic/reactr::reactiveExpression", {
  
  skip("manual only due to environment/lexical scoping issues")
  expect_true(setNested(id = "x_1", value = TRUE, reactive = TRUE))
  expect_true(setNested(id = "x_2", 
    value = reactr::reactiveExpression({
      !getNested(id = "x_1", where = parent.frame(7))
    }))
  )
  expect_equal(getNested(id = "x_1"), TRUE)
  expect_equal(getNested(id = "x_2"), FALSE)
  
  expect_true(setNested(id = "x_1", value = FALSE))
  expect_equal(getNested(id = "x_2"), TRUE)
  
})

##------------------------------------------------------------------------------
context("setNested/reactive/atomic/nestr::reactiveExpression")
##------------------------------------------------------------------------------

test_that("setNested/reactive/atomic/nestr::reactiveExpression", {
  
  skip("manual only due to environment/lexical scoping issues")
  expect_true(setNested(id = "x_1", value = TRUE, reactive = TRUE))
  expect_true(setNested(id = "x_2", 
    value = reactiveExpression(
      !getNested(id = "x_1", where = parent.frame(7))
    ))
  )
  expect_equal(getNested(id = "x_1"), TRUE)
  expect_equal(getNested(id = "x_2"), FALSE)
  
  expect_true(setNested(id = "x_1", value = FALSE))
  expect_equal(getNested(id = "x_2"), TRUE)

})

##------------------------------------------------------------------------------
context("setNested/reactive/path")
##------------------------------------------------------------------------------

test_that("setNested/reactive/path", {
  
  skip("manual only due to environment/lexical scoping issues")
  expect_true(setNested(id = "a/test", value = TRUE, reactive = TRUE, gap = TRUE))
  expect_true(setNested(id = "b/test", 
    value = reactiveExpression(!getNested(id = "a/test", where = parent.frame(7))), 
    reactive = TRUE, 
    gap = TRUE
  ))
  
  expect_equal(getNested(id = "b/test"), FALSE)
  expect_true(setNested(id = "a/test", value = FALSE))
  expect_equal(getNested(id = "a/test"), FALSE)
  expect_equal(getNested(id = "b/test"), TRUE)
  
})