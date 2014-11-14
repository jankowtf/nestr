if (FALSE) {
  ## Default values //
  id = "a/b"
  value = 10
  where = parent.frame()
  fail_value = NULL
  force = FALSE
  gap = TRUE
  must_exist = FALSE
  reactive = FALSE
  return_status = TRUE
  strict = c(0, 1, 2)
  typed = FALSE
}

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
  expect_warning(setNested(
    id = "test/b", 
    value = TRUE,
    must_exist = TRUE,
    strict = 1
  ))
  expect_error(setNested(
    id = "test/b", 
    value = TRUE,
    must_exist = TRUE,
    strict = 2
  ))
  
  ## Clean up //
  rm(test)
  
})

##------------------------------------------------------------------------------
context("setNested/return value")
##------------------------------------------------------------------------------

test_that("setNested/return value", {

  expect_equal(setNested(id = "test", value = 10, return_status = FALSE), 10)
  expect_equal(getNested(id = "test"), 10)
  
  expect_equal(setNested(id = "a/b", value = 10, return_status = FALSE), 10)
  expect_equal(getNested(id = "a/b"), 10)
  rm(a)
  
  ## Constellations that lead to failed assignment //
  expect_equal(setNested(id = "a/b", value = 10, gap = FALSE, 
                         return_status = FALSE), NULL)
  expect_equal(getNested(id = "a/b"), NULL)
  
  expect_equal(setNested(id = "a/b", value = 10, gap = FALSE, 
    return_status = FALSE, fail_value = NA), NA)
  expect_equal(getNested(id = "a/b"), NULL)
  
  ## Clean up //
  rm(test)
  
})

##------------------------------------------------------------------------------
context("setNested/typed")
##------------------------------------------------------------------------------
  
test_that("setNested/typed/strict = 0", {
  
  expect_true(setNested(
    id = "test/a", 
    value = "hello world!",
    typed = TRUE
  ))
  expect_true(setNested(id = "test/a", value = 10))
  expect_identical(getNested("test/a"), "hello world!")
  
  ## Clean up //
  rm(test)
  
})

test_that("setNested/typed/strict = 1", {
  
  expect_true(setNested(
    id = "test/a", 
    value = "hello world!",
    typed = TRUE,
    strict = 1
  ))
  expect_warning(setNested(id = "test/a", value = 10))
  expect_identical(getNested("test/a"), "hello world!")
  
  ## Clean up //
  rm(test)
  
})

test_that("setNested/typed/strict = 2", {
  
  expect_true(setNested(
    id = "test/a", 
    value = "hello world!",
    typed = TRUE,
    strict = 2
  ))
  expect_error(setNested(id = "test/a", value = 10))
  expect_identical(getNested("test/a"), "hello world!")
  
  expect_true(setNested(id = "test/a", value = "something else"))
  expect_equal(getNested(id = "test/a"), "something else")

  ## Clean up //
  rm(test)
  
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

  expect_false(setNested(id = "a/b/c/d", value = TRUE, gap = FALSE))
  expect_error(setNested(id = "a/b/c/d", value = TRUE, gap = FALSE, strict = 2))
  expect_true(setNested(id = "a/b/c/d", value = TRUE))
  expect_equal(getNested(id = "a/b/c/d"), TRUE)
  
  ## Clean up //
  rm(a)
  
})

##------------------------------------------------------------------------------
context("setNested/force")
##------------------------------------------------------------------------------

test_that("setNested/force leaf to branch", {
  
  expect_true(setNested(id = "a", value = "hello world!"))
  expect_false(setNested(id = "a/b/c/d", value = TRUE))
  expect_error(setNested(id = "a/b/c/d", value = TRUE, strict = 2))
  expect_true(setNested(id = "a/b/c/d", value = TRUE, force = TRUE))
  expect_equal(getNested(id = "a/b/c/d"), TRUE)

  ## Clean up //
  rm(a)
  
})

test_that("setNested/force leaf to branch", {
  
  expect_true(setNested(id = "a", value = "hello world!"))
  expect_false(setNested(id = "a/b", value = TRUE))
  expect_error(setNested(id = "a/b", value = TRUE, strict = 2))
  expect_true(setNested(id = "a/b", value = TRUE, force = TRUE))
  expect_equal(getNested(id = "a/b"), TRUE)

  ## Clean up //
  rm(a)
  
})

test_that("setNested/force branch to leaf", {
  
  expect_true(setNested(id = "a/b", value = 10))
  expect_false(setNested(id = "a", value = 10))
  expect_error(setNested(id = "a", value = 10, strict = 2))
  expect_true(setNested(id = "a", value = 10, force = TRUE))
  expect_equal(getNested(id = "a"), 10)

  ## Clean up //
  rm(a)
  
})

##------------------------------------------------------------------------------
context("setNested/explicit where")
##------------------------------------------------------------------------------

test_that("setNested/explicit where", {

  where <- new.env()
  expect_true(setNested(id = "a/b/c", value = 10, where = where))
  expect_equal(getNested(id = "a/b/c", where = where), 10)
  expect_true(exists("a", where))
  expect_true(exists("b", where$a))
  expect_true(exists("c", where$a$b))
  expect_identical(where$a$b$c, getNested("a/b/c", where))

  ## Clean up //
  rm(where)
  
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
  
  ## Clean up //
  rm(x_1, x_2)
  
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

  ## Clean up //
  rm(x_1, x_2)
  
})

##------------------------------------------------------------------------------
context("setNested/reactive/path")
##------------------------------------------------------------------------------

test_that("setNested/reactive/path", {
  
  skip("manual only due to environment/lexical scoping issues")
  expect_true(setNested(id = "a/test", value = TRUE, reactive = TRUE))
  expect_true(setNested(id = "b/test", 
    value = reactiveExpression(!getNested(id = "a/test", where = parent.frame(7))), 
    reactive = TRUE
  ))
  
  expect_equal(getNested(id = "b/test"), FALSE)
  expect_true(setNested(id = "a/test", value = FALSE))
  expect_equal(getNested(id = "a/test"), FALSE)
  expect_equal(getNested(id = "b/test"), TRUE)

  ## Clean up //
  rm(a, b)

})
